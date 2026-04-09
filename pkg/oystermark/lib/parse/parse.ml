(** {0 Pre-resolution file-level parsing}

Each module provides a single-pass mapper that might
- introduce new inline or block extensions
- rewrite Cmarkit.Doc AST
- add metadata to AST nodes
- {!Frontmatter} operates on the raw file content before Cmarkit.Doc parsing.
  Every other mapper operates on the Cmarkit.Doc AST.
- Some mappers operate on node of Cmarkit AST, i.e. [Block.t] or [Inline.t]. Their
  provided mapper follows the signature of [Cmarkit.Inline.t Cmarkit.Mapper.mapper]
  or [Cmarkit.Block.t Cmarkit.Mapper.mapper]
- `-> other mappers rely on multiple nodes as input, thus operates on the whole
  [Cmarkit.Doc.t]. E.g., {!Div} and {!Struct}

*)

open Core
module Block_id = Block_id
module Callout = Callout
module Div = Div
module Frontmatter = Frontmatter
module Heading_slug = Heading_slug
module Wikilink = Wikilink
module Attribute = Attribute
module Textloc_conv = Textloc_conv
module Struct = Struct
(** Does not provide a mapper  *)
module Extract = Extract


(* TODO: mapper should be provided directly by each sub-module so that
  test can be put inside corresponding sub-module.
*)

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
      (match Callout.block_map mapper block with
       | `Map _ as result -> result
       | `Default ->
         (match Attribute.block_map mapper block with
          | `Map _ as result -> result
          | `Default -> Block_id.block_map mapper block))
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
let of_string ?(strict = false) ?(layout = false) ?(locs = true) (s : string)
  : Cmarkit.Doc.t
  =
  let open Cmarkit in
  let yaml_opt, body = Frontmatter.of_string s in
  let cmarkit_doc = Doc.of_string ~strict ~layout ~locs:true body in
  let body_doc = Mapper.map_doc (make_mapper ()) cmarkit_doc in
  let body_doc = Div.rewrite_doc body_doc in
  let body_doc = Struct.rewrite_doc ~source:(Some body) body_doc in
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
    let open Cmarkit_renderer in
    let inline (c : context) = function
      | Wikilink.Ext_wikilink (wl, _) ->
        Context.string c (Wikilink.to_commonmark wl);
        true
      | _ -> false
    in
    let block (c : context) = function
      | Frontmatter.Frontmatter y ->
        Context.string c (Frontmatter.to_commonmark y);
        true
      | Div.Ext_div (div, body) ->
        let fence = String.make div.colons ':' in
        let class_suffix =
          match div.class_name with
          | Some cls -> " " ^ cls
          | None -> ""
        in
        let buf = Context.buffer c in
        let len = Buffer.length buf in
        let needs_nl = len > 0 && not (Char.equal (Buffer.nth buf (len - 1)) '\n') in
        if needs_nl then Context.byte c '\n';
        Context.string c (fence ^ class_suffix ^ "\n\n");
        Context.block c body;
        Context.string c ("\n" ^ fence ^ "\n");
        true
      | Struct.Ext_keyed_block ({ label }, body) ->
        Context.inline c label;
        Context.string c ":\n";
        Context.block c body;
        true
      | Struct.Ext_keyed_list_item ({ label }, body) ->
        Context.string c "- ";
        Context.inline c label;
        Context.string c ":\n";
        Context.block c body;
        true
      | _ -> false
    in
    Cmarkit_renderer.make ~inline ~block ()
  in
  let default = Cmarkit_commonmark.renderer () in
  let r = Cmarkit_renderer.compose default custom in
  Cmarkit_renderer.doc_to_string r doc
;;

(* {1 sexp of Cmarkit.{Meta.t, Block.t, Inline.t} }*)

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
  | Struct.Ext_keyed_list_item ({ label }, body) ->
    Sexp.List [ Atom "Keyed_list_item"; sexp_of_inline label; sexp_of_block body ]
  | Struct.Ext_keyed_block ({ label }, body) ->
    Sexp.List [ Atom "Keyed_block"; sexp_of_inline label; sexp_of_block body ]
  | _ -> Sexp.Atom "<unknown-block>"
;;

(** {1:test Test} *)

module For_test = struct
  let make_block (s : string) : Cmarkit.Block.t =
    let doc = of_string s in
    Cmarkit.Doc.block doc
  ;;

  (** Count the number of div blocks in a doc  *)
  let count_div (doc : Cmarkit.Doc.t) : int =
    let folder =
      Cmarkit.Folder.make
        ~block:(fun f acc -> function
           | Div.Ext_div (_div, body) ->
             Cmarkit.Folder.ret (1 + Cmarkit.Folder.fold_block f acc body)
           | _ -> Cmarkit.Folder.default)
        ()
    in
    Cmarkit.Folder.fold_doc folder 0 doc
  ;;

  let pp_doc (doc : Cmarkit.Doc.t) : unit =
    let block = Cmarkit.Doc.block doc in
    block |> sexp_of_block |> Sexp.to_string_hum ~indent:2 |> print_endline
  ;;

  (** Assert that the commonmark roundtrip of a doc is idempotent under normalization.
    @return ()
    @raise Failure if the roundtrip is not idempotent.
  *)
  let commonmark_of_doc_idempotent s =
    let normalize s = String.rstrip s in
    let cm1 = commonmark_of_doc (of_string s) in
    let cm2 = commonmark_of_doc (of_string cm1) in
    [%test_eq: string] (normalize cm1) (normalize cm2)
  ;;

  (** Count code blocks that have a non-[None] attribute parsed *)
  let count_attr (doc : Cmarkit.Doc.t) : int =
    let folder =
      Cmarkit.Folder.make
        ~block:(fun _f acc -> function
           | Cmarkit.Block.Code_block (_, meta) ->
             (match Cmarkit.Meta.find Attribute.meta_key meta with
              | Some { Attribute.attribute = Some _; _ } -> Cmarkit.Folder.ret (acc + 1)
              | _ -> Cmarkit.Folder.default)
           | _ -> Cmarkit.Folder.default)
        ()
    in
    Cmarkit.Folder.fold_doc folder 0 doc
  ;;
end

(** {2 Attribute}

Tests for {!module-"Attribute"}. *)

let%test_module "Attribute" =
  (module struct
    open For_test
    open Attribute.For_test

    let%expect_test _ =
      let doc = of_string example_no_attribute in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {| ((Code_block python II) (meta (attribute ((lang python) (attribute ()))))) |}]
    ;;

    let%expect_test _ =
      let doc = of_string example_with_attribute in
      [%test_result: int] (count_attr doc) ~expect:1;
      pp_doc doc;
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

    let%expect_test _ =
      let doc = of_string non_example_invalid_multiple_ids in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        ((Code_block
           "python {#myid #myid2 .class_a .class_b key1=val1 key2=\"val2\"}" II)
          (meta (attribute ((lang python) (attribute ())))))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string non_example_invalid_item in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        ((Code_block "python {#myid .class_a .class_b hi}" II)
          (meta (attribute ((lang python) (attribute ())))))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string non_example_no_info_string in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect {| (Code_block "{#myid .class_a .class_b}" II) |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      List.iter all_examples ~f:commonmark_of_doc_idempotent
    ;;
  end)
;;

(** {2 Extract}

Tests for {!module-"Extract"}. *)

let%test_module "Extract" =
  (module struct
    open For_test
    open Extract.For_test

    let pp_section (blocks : Cmarkit.Block.t list) : unit =
      print_endline
        (commonmark_of_doc
           (Cmarkit.Doc.make (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none))))
    ;;

    let pp_block_opt (block : Cmarkit.Block.t option) : unit =
      match block with
      | None -> print_endline "<none>"
      | Some b -> print_endline (commonmark_of_doc (Cmarkit.Doc.make b))
    ;;

    let%expect_test "get_heading_section: heading-1" =
      let block = make_block example_headings in
      pp_section (Extract.get_heading_section [ block ] "heading-1");
      [%expect
        {|
    # Heading 1
    ## Heading 2
    ### Heading 3
    #### Heading 4
    ##### Heading 5
    ###### Heading 6
    |}]
    ;;

    let%expect_test "get_heading_section: heading-8" =
      let block = make_block example_headings in
      pp_section (Extract.get_heading_section [ block ] "heading-8");
      [%expect
        {|
    ## Heading 8
    ### Heading 9
    |}]
    ;;

    let%expect_test "get_block_by_caret_id: inline" =
      let doc = of_string example_inline_caret_id in
      pp_block_opt (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc ] "abc123");
      [%expect {| Second paragraph text ^abc123 |}]
    ;;

    let%expect_test "get_block_by_caret_id: standalone blockquote" =
      let doc = of_string example_blockquote_caret_id in
      pp_block_opt (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc ] "bq001");
      [%expect {| > A blockquote here. |}]
    ;;

    let%expect_test "get_block_by_caret_id: not found" =
      let doc = of_string example_not_found in
      pp_block_opt (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc ] "nope");
      [%expect {| <none> |}]
    ;;

    let%expect_test "get_block_by_caret_id: standalone list" =
      let doc = of_string example_list_caret_id in
      pp_block_opt (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc ] "lst001");
      [%expect
        {|
    - Item one
    - Item two
    |}]
    ;;

    let%expect_test "get_block_by_caret_id: nested list" =
      let doc = of_string example_nested_list_caret_id in
      pp_block_opt (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc ] "firstline");
      [%expect {| a nested list ^firstline |}];
      pp_block_opt (Extract.get_block_by_caret_id [ Cmarkit.Doc.block doc ] "inneritem");
      [%expect
        {|
    item
    ^inneritem
    |}]
    ;;
  end)
;;

(** {2 Div}

Tests for {!module-"Div"}. *)

let%test_module "Div" =
  (module struct
    open For_test
    open Div.For_test

    let%expect_test _ =
      let doc = of_string example_basic in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Blocks (Paragraph (Text "Here is a paragraph.")) Blank_line
              (Paragraph (Text "And here is another.")))))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string example_no_class in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Div ((class_name ()) (colons 3)) (Paragraph (Text content)))
          Blank_line)
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string example_nested_divs in
      [%test_result: int] (count_div doc) ~expect:2;
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (outer)) (colons 4))
            (Div ((class_name (inner)) (colons 3)) (Paragraph (Text content))))
          Blank_line)
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string example_nested_divs_same_length in
      [%test_result: int] (count_div doc) ~expect:2;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string example_EOF_closes in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Paragraph (Text "unclosed content"))))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string example_extra_closing_fence in
      [%test_result: int] (count_div doc) ~expect:2;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string non_example_less_than_3_colons in
      [%test_result: int] (count_div doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        (Paragraph
          (Inlines (Text ":: not-a-div") (Break soft) (Text content) (Break soft)
            (Text ::)))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string non_example_extra_words_after_class in
      [%test_result: int] (count_div doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        (Blocks (Paragraph (Text "::: warning extra")) (Paragraph (Text content))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        |}]
    ;;

    let%expect_test _ =
      let doc = of_string non_example_div_does_not_interfere_with_code_blocks in
      [%test_result: int] (count_div doc) ~expect:0;
      pp_doc doc;
      [%expect {| (Code_block no-info "::: not-a-div") |}]
    ;;

    let%expect_test _ =
      let doc = of_string example_closing_fence_must_be_at_least_as_long in
      pp_doc doc;
      [%expect
        {|
        (Blocks
          (Div ((class_name (warning)) (colons 4))
            (Blocks (Paragraph (Text content))
              (Div ((class_name ()) (colons 3)) (Blocks)))))
        |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      List.iter all_examples ~f:commonmark_of_doc_idempotent
    ;;
  end)
;;

(** {2 Struct}

Tests for {!module-"Struct"}. *)

let%test_module "Struct" =
  (module struct
    open For_test
    open Struct.Spec

    let%expect_test "rule 1: keyed list item with indented content" =
      pp_doc (of_string rule1_keyed_list_item_with_indented_content);
      [%expect
        {|
        (List
          (Keyed_list_item (Text foo)
            (List (Paragraph (Text bar)) (Paragraph (Text baz)))))
        |}]
    ;;

    let%expect_test "rule 2: keyed list item followed by blank" =
      pp_doc (of_string rule2_keyed_list_item_followed_by_blank_line);
      [%expect
        {| (Blocks (List (Paragraph (Text foo:))) Blank_line (Paragraph (Text bar))) |}]
    ;;

    let%expect_test "rule 3: keyed list item with contiguous blocks after list" =
      pp_doc (of_string rule3_keyed_list_item_with_contiguous_blocks);
      [%expect
        {| (Blocks (List (Keyed_list_item (Text foo) (Code_block no-info bar)))) |}]
    ;;

    let%expect_test "rule 4: keyed paragraph" =
      pp_doc (of_string rule4_keyed_paragraph);
      [%expect
        {|
        (Blocks
          (Keyed_block (Text foo)
            (List (Paragraph (Text bar)) (Paragraph (Text baz))))
          Blank_line (Paragraph (Text bee)))
        |}]
    ;;

    let%expect_test "rule 5: keyed paragraph with multiple children" =
      pp_doc (of_string rule5_keyed_paragraph_multiple_children);
      [%expect
        {|
        (Blocks
          (Keyed_block (Text foo)
            (List (Paragraph (Text bar))
              (Paragraph (Inlines (Text baz) (Break soft) (Text "some text"))))))
        |}]
    ;;

    let%expect_test "rule 6: nesting" =
      pp_doc (of_string rule6_nesting);
      [%expect
        {|
        (Blocks
          (Keyed_block (Text foo)
            (List (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))
              (Paragraph (Text qux)))))
        |}]
    ;;

    let%expect_test "colon chain" =
      pp_doc (of_string colon_chain_inline_keying);
      [%expect
        {|
        (List
          (Keyed_list_item (Text foo)
            (Keyed_list_item (Text bar) (List (Paragraph (Text baz))))))
        |}]
    ;;

    let%expect_test "non-example: no colon" =
      pp_doc (of_string non_example_no_colon);
      [%expect {| (List (Paragraph (Text foo)) (Paragraph (Text bar))) |}]
    ;;

    let%expect_test "non-example: colon in code span" =
      pp_doc (of_string non_example_colon_in_code_span);
      [%expect
        {|
        (Paragraph
          (Inlines (Text "text with ") (Code_span code:) (Break soft)
            (Text "following paragraph")))
        |}]
    ;;

    (* Universal predicates: driven by both the named examples
       (regression) and a [Quickcheck] stream (property-based). *)

    let check_universal_predicates (input : string) : unit =
      let doc = of_string input in
      let source = Some input in
      let fail name =
        Core.raise_s
          [%message "universal predicate failed" (name : string) (input : string)]
      in
      if not (keyed_bodies_non_empty doc) then fail "keyed_bodies_non_empty";
      if not (keying_is_maximal ~source doc) then fail "keying_is_maximal"
    ;;

    let%test_unit "examples: universal predicates hold" =
      List.iter all_examples ~f:check_universal_predicates
    ;;

    let%test_unit "generated: universal predicates hold" =
      Core.Quickcheck.test
        ~sexp_of:[%sexp_of: string]
        ~examples:all_examples
        ~trials:200
        gen_markdown
        ~f:check_universal_predicates
    ;;

    (* Commonmark roundtrip (idempotence through serialization): the
       rewritten tree serialized back to commonmark, then reparsed +
       rewritten, serializes identically.  Strictly stronger than an
       AST-level fixpoint check, and exercises the renderer too. *)

    let%test_unit "examples: commonmark roundtrip is idempotent" =
      List.iter all_examples ~f:commonmark_of_doc_idempotent
    ;;
  end)
;;
