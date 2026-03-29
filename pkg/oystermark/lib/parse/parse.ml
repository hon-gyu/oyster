(** Pre-resolution file-level parsing  *)

open Core
module Block_id = Block_id
module Callout = Callout
module Div = Div
module Frontmatter = Frontmatter
module Heading_slug = Heading_slug
module Wikilink = Wikilink
module Extract = Extract
module Attribute = Attribute

type block_id =
  | Caret of Block_id.t
  | Heading of string

(** Render inlines to plain text, losing their markdown syntax. Used in rendering
    heading to plain text. *)
let inline_to_plain_text (inline : Cmarkit.Inline.t) : string =
  let lines =
    Cmarkit.Inline.to_plain_text
      ~ext:(fun ~break_on_soft inline ->
        match inline with
        | Wikilink.Ext_wikilink (wl, _meta) ->
          let text = Wikilink.to_plain_text wl in
          Cmarkit.Inline.Text (text, Cmarkit.Meta.none)
        | other -> other)
      ~break_on_soft:false
      inline
  in
  String.concat ~sep:"\n" (List.map lines ~f:(String.concat ~sep:""))
;;

(** Create the single-pass mapper that:
    - parses wikilinks in inline text nodes ({!module:Wikilink})
    - tags block identifiers at paragraph ends ({!module:Block_id})
    - tags callout metadata on block quotes ({!module:Callout})
    - tags code block attributes onto code blocks ({!module:Attribute})
    - tags deduplicated heading slugs onto heading block meta ({!module:Heading_slug})

    Returns a fresh mapper each time (heading slug dedup requires per-document state). *)
let make_mapper () : Cmarkit.Mapper.t =
  let slug_seen = Hashtbl.create (module String) in
  let map_block (mapper : Cmarkit.Mapper.t) (block : Cmarkit.Block.t)
    : Cmarkit.Block.t Cmarkit.Mapper.result
    =
    match block with
    | Cmarkit.Block.Heading (h, meta) ->
      let orig_inline = Cmarkit.Block.Heading.inline h in
      let mapped_inline =
        Cmarkit.Mapper.map_inline mapper orig_inline |> Option.value ~default:orig_inline
      in
      let text = inline_to_plain_text mapped_inline in
      let slug = Heading_slug.dedup_slug slug_seen text in
      let meta' = Cmarkit.Meta.add Heading_slug.meta_key slug meta in
      let h' =
        Cmarkit.Block.Heading.make
          ?id:(Cmarkit.Block.Heading.id h)
          ~layout:(Cmarkit.Block.Heading.layout h)
          ~level:(Cmarkit.Block.Heading.level h)
          mapped_inline
      in
      Cmarkit.Mapper.ret (Cmarkit.Block.Heading (h', meta'))
    | _ ->
      (match Callout.map_callout mapper block with
       | `Map _ as result -> result
       | `Default ->
         (match Attribute.tag_cb_attr_meta mapper block with
          | `Map _ as result -> result
          | `Default -> Block_id.tag_block_id_meta mapper block))
  in
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:Wikilink.parse
    ~block:map_block
    ()
;;

(** [of_string ?strict ?layout s] parses markdown string [s] into a
    [Cmarkit.Doc.t] with frontmatter embedded as a {!Frontmatter.Frontmatter}
    block and wikilinks/block IDs parsed. Heading slugs are stamped onto
    heading block metadata. *)
let of_string ?(strict = false) ?(layout = false) ?(locs = false) (s : string)
  : Cmarkit.Doc.t
  =
  let open Cmarkit in
  let yaml_opt, body = Frontmatter.of_string s in
  let cmarkit_doc = Doc.of_string ~strict ~layout ~locs body in
  let body_doc = Mapper.map_doc (make_mapper ()) cmarkit_doc in
  let body_doc = Div.rewrite_doc body_doc in
  match yaml_opt, Doc.block body_doc with
  | None, _ -> body_doc
  | Some yaml, Block.Blocks (blocks, meta) ->
    let blocks' = Frontmatter.Frontmatter yaml :: blocks in
    Doc.make (Block.Blocks (blocks', meta))
  | Some yaml, other ->
    Doc.make (Block.Blocks ([ Frontmatter.Frontmatter yaml; other ], Meta.none))
;;

let commonmark_of_doc (doc : Cmarkit.Doc.t) : string =
  let custom =
    let inline (c : Cmarkit_renderer.context) = function
      | Wikilink.Ext_wikilink (wl, _) ->
        Cmarkit_renderer.Context.string c (Wikilink.to_commonmark wl);
        true
      | _ -> false
    in
    let block (c : Cmarkit_renderer.context) = function
      | Frontmatter.Frontmatter y ->
        Cmarkit_renderer.Context.string c (Frontmatter.to_commonmark y);
        true
      | Div.Ext_div (div, body) ->
        let fence = String.make div.colons ':' in
        let class_suffix =
          match div.class_name with
          | Some cls -> " " ^ cls
          | None -> ""
        in
        Cmarkit_renderer.Context.string c (fence ^ class_suffix ^ "\n\n");
        Cmarkit_renderer.Context.block c body;
        Cmarkit_renderer.Context.string c ("\n" ^ fence ^ "\n");
        true
      | _ -> false
    in
    Cmarkit_renderer.make ~inline ~block ()
  in
  let default = Cmarkit_commonmark.renderer () in
  let r = Cmarkit_renderer.compose default custom in
  Cmarkit_renderer.doc_to_string r doc
;;

let sexp_of_meta (meta : Cmarkit.Meta.t) : Sexp.t list =
  let open Cmarkit.Meta in
  let items = ref [] in
  (match find Heading_slug.meta_key meta with
   | Some slug -> items := Sexp.List [ Atom "heading-slug"; Atom slug ] :: !items
   | None -> ());
  (match find Block_id.meta_key meta with
   | Some bid -> items := Sexp.List [ Atom "block-id"; Block_id.sexp_of_t bid ] :: !items
   | None -> ());
  (match find Callout.meta_key meta with
   | Some c -> items := Sexp.List [ Atom "callout"; Callout.sexp_of_t c ] :: !items
   | None -> ());
  (match find Attribute.meta_key meta with
   | Some a ->
     items
     := Sexp.List [ Atom "attribute"; Attribute.sexp_of_code_block_info a ] :: !items
   | None -> ());
  List.rev !items
;;

let rec sexp_of_inline (i : Cmarkit.Inline.t) : Sexp.t =
  let open Cmarkit in
  match i with
  | Inline.Text (s, _) -> Sexp.List [ Atom "Text"; Atom s ]
  | Inline.Autolink (a, _) ->
    let link = fst (Inline.Autolink.link a) in
    Sexp.List [ Atom "Autolink"; Atom link ]
  | Inline.Break (b, _) ->
    let type_s =
      match Inline.Break.type' b with
      | `Hard -> "hard"
      | `Soft -> "soft"
    in
    Sexp.List [ Atom "Break"; Atom type_s ]
  | Inline.Code_span (cs, _) ->
    Sexp.List [ Atom "Code_span"; Atom (Inline.Code_span.code cs) ]
  | Inline.Emphasis (e, _) ->
    Sexp.List [ Atom "Emphasis"; sexp_of_inline (Inline.Emphasis.inline e) ]
  | Inline.Strong_emphasis (e, _) ->
    Sexp.List [ Atom "Strong_emphasis"; sexp_of_inline (Inline.Emphasis.inline e) ]
  | Inline.Link (l, _) -> Sexp.List [ Atom "Link"; sexp_of_inline (Inline.Link.text l) ]
  | Inline.Image (l, _) -> Sexp.List [ Atom "Image"; sexp_of_inline (Inline.Link.text l) ]
  | Inline.Raw_html (html, _) ->
    let s =
      List.map html ~f:(fun bl -> Cmarkit.Block_line.tight_to_string bl)
      |> String.concat ~sep:""
    in
    Sexp.List [ Atom "Raw_html"; Atom s ]
  | Inline.Inlines (is, _) -> Sexp.List (Atom "Inlines" :: List.map is ~f:sexp_of_inline)
  | Inline.Ext_strikethrough (s, _) ->
    Sexp.List [ Atom "Strikethrough"; sexp_of_inline (Inline.Strikethrough.inline s) ]
  | Inline.Ext_math_span (m, _) ->
    Sexp.List [ Atom "Math_span"; Atom (Inline.Math_span.tex m) ]
  | Wikilink.Ext_wikilink (wl, _) -> Sexp.List [ Atom "Wikilink"; Wikilink.sexp_of_t wl ]
  | _ -> Sexp.Atom "<unknown-inline>"

and sexp_of_block (b : Cmarkit.Block.t) : Sexp.t =
  let open Cmarkit in
  let with_meta meta sexp =
    match sexp_of_meta meta with
    | [] -> sexp
    | metas -> Sexp.List [ sexp; Sexp.List (Atom "meta" :: metas) ]
  in
  match b with
  | Block.Blank_line (_, meta) -> with_meta meta (Sexp.Atom "Blank_line")
  | Block.Paragraph (p, meta) ->
    with_meta
      meta
      (Sexp.List [ Atom "Paragraph"; sexp_of_inline (Block.Paragraph.inline p) ])
  | Block.Heading (h, meta) ->
    with_meta
      meta
      (Sexp.List
         [ Atom "Heading"
         ; Atom (Int.to_string (Block.Heading.level h))
         ; sexp_of_inline (Block.Heading.inline h)
         ])
  | Block.Code_block (cb, meta) ->
    let info =
      match Block.Code_block.info_string cb with
      | None -> Sexp.Atom "no-info"
      | Some (s, _) -> Sexp.Atom s
    in
    let code =
      List.map (Block.Code_block.code cb) ~f:(fun bl ->
        Sexp.Atom (Block_line.to_string bl))
    in
    with_meta meta (Sexp.List (Atom "Code_block" :: info :: code))
  | Block.Html_block (lines, meta) ->
    let s =
      List.map lines ~f:(fun bl -> Block_line.to_string bl) |> String.concat ~sep:"\n"
    in
    with_meta meta (Sexp.List [ Atom "Html_block"; Atom s ])
  | Block.Block_quote (bq, meta) ->
    with_meta
      meta
      (Sexp.List [ Atom "Block_quote"; sexp_of_block (Block.Block_quote.block bq) ])
  | Block.List (l, meta) ->
    let items =
      List.map (Block.List'.items l) ~f:(fun (item, _item_meta) ->
        sexp_of_block (Block.List_item.block item))
    in
    with_meta meta (Sexp.List (Atom "List" :: items))
  | Block.Blocks (bs, meta) ->
    with_meta meta (Sexp.List (Atom "Blocks" :: List.map bs ~f:sexp_of_block))
  | Block.Link_reference_definition _ -> Sexp.Atom "Link_reference_definition"
  | Block.Thematic_break (_, meta) -> with_meta meta (Sexp.Atom "Thematic_break")
  | Frontmatter.Frontmatter _ -> Sexp.Atom "Frontmatter"
  | Div.Ext_div (div, body) ->
    Sexp.List [ Atom "Div"; Div.sexp_of_t div; sexp_of_block body ]
  | _ -> Sexp.Atom "<unknown-block>"
;;

module For_test = struct
  let make_block (s : string) : Cmarkit.Block.t =
    let doc = of_string s in
    Cmarkit.Doc.block doc
  ;;

  let parse (s : string) : string =
    let block = make_block s in
    Sexp.to_string_hum ~indent:2 (sexp_of_block block)
  ;;
end

(* Tests for module Attribute *)
let%test_module "Attribute" =
  (module struct
    let parse = For_test.parse

    let%expect_test "no attribute" =
      print_endline
        (parse
           {|```python
II
```|});
      [%expect
        {| ((Code_block python II) (meta (attribute ((lang python) (attribute ()))))) |}]
    ;;

    let%expect_test "attribute" =
      print_endline
        (parse
           {|```python {#myid .class_a .class_b key1=val1 key2="val2"}
II
```|});
      [%expect
        {|
        ((Code_block "python {#myid .class_a .class_b key1=val1 key2=\"val2\"}" II)
          (meta
            (attribute
              ((lang python)
                (attribute
                  (((id (#myid)) (classes (.class_a .class_b))
                     (kvs ((key1 val1) (key2 val2))))))))))
        |}]
    ;;

    let%expect_test "invalid attribute: multiple ids" =
      print_endline
        (parse
           {|```python {#myid #myid2 .class_a .class_b key1=val1 key2="val2"}
II
```|});
      [%expect
        {|
        ((Code_block
           "python {#myid #myid2 .class_a .class_b key1=val1 key2=\"val2\"}" II)
          (meta (attribute ((lang python) (attribute ())))))
        |}]
    ;;

    let%expect_test "invalid attribute: invalid item" =
      print_endline
        (parse
           {|```python {#myid .class_a .class_b hi}
II
```|});
      [%expect
        {|
        ((Code_block "python {#myid .class_a .class_b hi}" II)
          (meta (attribute ((lang python) (attribute ())))))
        |}]
    ;;

    let%expect_test "invalid attribute: no info string" =
      print_endline
        (parse
           {|```{#myid .class_a .class_b}
II
```|});
      [%expect {| (Code_block "{#myid .class_a .class_b}" II) |}]
    ;;
  end)
;;

(* Tests for module Extract *)
let%test_module "Extract" =
  (module struct
    let%expect_test "get_heading_section" =
      let block =
        For_test.make_block
          {|\
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6
# Heading 7
## Heading 8
### Heading 9
## Heading 10
#### Heading 11
### Heading 12|}
      in
      let heading_id = "heading-1" in
      let extracted = Extract.get_heading_section [ block ] heading_id in
      print_endline
        (commonmark_of_doc
           (Cmarkit.Doc.make (Cmarkit.Block.Blocks (extracted, Cmarkit.Meta.none))));
      [%expect
        {|
    # Heading 1
    ## Heading 2
    ### Heading 3
    #### Heading 4
    ##### Heading 5
    ###### Heading 6
    |}];
      let heading_id = "heading-8" in
      let extracted = Extract.get_heading_section [ block ] heading_id in
      print_endline
        (commonmark_of_doc
           (Cmarkit.Doc.make (Cmarkit.Block.Blocks (extracted, Cmarkit.Meta.none))));
      [%expect
        {|
    ## Heading 8
    ### Heading 9
    |}]
    ;;

    let%expect_test "get_block_by_caret_id" =
      let render_block (block : Cmarkit.Block.t option) : unit =
        match block with
        | None -> print_endline "<none>"
        | Some b -> print_endline (commonmark_of_doc (Cmarkit.Doc.make b))
      in
      (* Case 1: inline block ID at end of paragraph *)
      let doc1 =
        of_string
          {|\
First paragraph.

Second paragraph text ^abc123|}
      in
      render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc1 ] "abc123");
      [%expect {| Second paragraph text ^abc123 |}];
      (* Case 2: standalone block ID referencing previous block (blockquote) *)
      let doc2 =
        of_string
          {|\
> A blockquote here.

^bq001
|}
      in
      render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc2 ] "bq001");
      [%expect {| > A blockquote here. |}];
      (* Case 3: not found *)
      let doc3 =
        of_string
          {|
Some text ^exists
|}
      in
      render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc3 ] "nope");
      [%expect {| <none> |}];
      (* Case 4: standalone block ID referencing previous list *)
      let doc4 =
        of_string
          {|
- Item one
- Item two

^lst001
|}
      in
      render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc4 ] "lst001");
      [%expect
        {|
    - Item one
    - Item two
    |}];
      (* Case 5: block ID inside a list item *)
      let doc5 =
        of_string
          {|
- a nested list ^firstline
    - item
      ^inneritem
|}
      in
      render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc5 ] "firstline");
      [%expect {| a nested list ^firstline |}];
      render_block (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc5 ] "inneritem");
      [%expect
        {|
    item
    ^inneritem
    |}]
    ;;
  end)
;;

(* Tests for module Div *)
let%test_module "Div" =
  (module struct
    let parse = For_test.parse

    let%expect_test "basic div with class" =
      print_endline
        (parse
           {|::: warning
Here is a paragraph.

And here is another.
:::|});
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Blocks (Paragraph (Text "Here is a paragraph.")) Blank_line
              (Paragraph (Text "And here is another.")))))
        |}]
    ;;

    let%expect_test "div without class" =
      print_endline
        (parse
           {|:::
content
:::|});
      [%expect
        {| (Blocks (Div ((class_name ()) (colons 3)) (Paragraph (Text content)))) |}]
    ;;

    let%expect_test "nested divs with longer fences" =
      print_endline
        (parse
           {|:::: outer
::: inner
content
:::
::::|});
      [%expect
        {|
        (Blocks
          (Div ((class_name (outer)) (colons 4))
            (Div ((class_name (inner)) (colons 3)) (Paragraph (Text content)))))
        |}]
    ;;

    let%expect_test "nested divs same length" =
      print_endline
        (parse
           {|::: outer
::: inner
content
:::
:::|});
      [%expect
        {|
        (Blocks
          (Div ((class_name (outer)) (colons 3))
            (Div ((class_name (inner)) (colons 3)) (Paragraph (Text content)))))
        |}]
    ;;

    let%expect_test "unclosed div (EOF closes)" =
      print_endline
        (parse
           {|::: warning
unclosed content|});
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Paragraph (Text "unclosed content"))))
        |}]
    ;;

    let%expect_test "unbalanced: extra closing fence" =
      print_endline
        (parse
           {|::: warning
content
:::
:::|});
      [%expect
        {|
        (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test "ill-formed: less than 3 colons" =
      print_endline
        (parse
           {|:: not-a-div
content
::|});
      [%expect
        {|
        (Paragraph
          (Inlines (Text ":: not-a-div") (Break soft) (Text content) (Break soft)
            (Text ::)))
        |}]
    ;;

    let%expect_test "ill-formed: extra words after class" =
      print_endline
        (parse
           {|::: warning extra
content
:::|});
      [%expect
        {|
        (Blocks (Paragraph (Text "::: warning extra")) (Paragraph (Text content))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test "div does not interfere with code blocks" =
      print_endline
        (parse
           {|```
::: not-a-div
```|});
      [%expect {| (Code_block no-info "::: not-a-div") |}]
    ;;

    let%expect_test "div does not interfere with blockquotes" =
      print_endline
        (parse
           {|> a blockquote

::: warning
content
:::|});
      [%expect
        {|
        (Blocks (Block_quote (Paragraph (Text "a blockquote"))) Blank_line
          (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content))))
        |}]
    ;;

    let%expect_test "div does not interfere with headings" =
      print_endline
        (parse
           {|# heading

::: warning
content
:::|});
      [%expect
        {|
        (Blocks ((Heading 1 (Text heading)) (meta (heading-slug heading))) Blank_line
          (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content))))
        |}]
    ;;

    let%expect_test "closing fence must be at least as long" =
      print_endline
        (parse
           {|:::: warning
content
:::
::::|});
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 4))
            (Blocks (Paragraph (Text content))
              (Div ((class_name ()) (colons 3)) (Blocks)))))
        |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      let inputs =
        [ {|::: warning
Here is a paragraph.

And here is another.
:::|}
        ; {|:::
content
:::|}
        ; {|:::: outer
::: inner
content
:::
::::|}
        ; {|::: warning
unclosed content|}
        ]
      in
      let normalize s = String.rstrip s in
      List.iter inputs ~f:(fun input ->
        let cm1 = commonmark_of_doc (of_string input) in
        let cm2 = commonmark_of_doc (of_string cm1) in
        if not (String.equal (normalize cm1) (normalize cm2))
        then failwithf "MISMATCH:\n  cm1: %s\n  cm2: %s" cm1 cm2 ())
    ;;
  end)
;;
