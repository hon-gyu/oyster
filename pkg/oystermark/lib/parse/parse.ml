(** {1 Pre-resolution file-level parsing}

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
  [Cmarkit.Doc.t]. E.g., {!Oy_div} and {!Struct}

*)

open Core
open Common
module Common = Common
module Frontmatter = Frontmatter
module Heading_slug = Heading_slug
module Cb_attribute = Cb_attribute
module Textloc_conv = Textloc_conv
module Struct = Struct

(** Does not provide a mapper  *)
module Extract = Extract

type block_id =
  | Caret of Cmarkit.Block.Block_id.t
  | Heading of string

let mk_mapper () : Cmarkit.Mapper.t =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~block:
      (compose_all_block_maps
         [ Heading_slug.mk_block_map ()
         ; Cb_attribute.block_map
         ])
    ()
;;

(** [of_string ?strict ?layout s] parses markdown string [s] into a
    [Cmarkit.Doc.t] with frontmatter embedded as a {!Frontmatter.Frontmatter}
    block and wikilinks/block IDs parsed. Heading slugs are stamped onto
    heading block metadata. *)
let of_string
      (* Cmarkit config *)
      ?(strict = false)
      ?(layout = false)
      ?(locs = true)
      (* Oystermark config *)
      ?(config = Config.default)
      (s : string)
  : Cmarkit.Doc.t
  =
  let enable_struct = config.ext_struct.enable in
  let open Cmarkit in
  let yaml_opt, body = Frontmatter.of_string s in
  let cmarkit_doc =
    Doc.of_string
      ~strict
      ~layout
      ~locs:true
      ~block_id:true
      ~div:true
      ~wikilink:true
      ~djot_inline_attributes:true
      ~djot_block_attributes:true
      ~callout:(Block.Callout.Config.make ())
      body
  in
  let body_doc = Mapper.map_doc (mk_mapper ()) cmarkit_doc in
  let body_doc = if enable_struct then Struct.rewrite_doc body_doc else body_doc in
  match yaml_opt, Doc.block body_doc with
  | None, _ -> body_doc
  | Some yaml, Block.Blocks (blocks, meta) ->
    let blocks' = Frontmatter.Frontmatter yaml :: blocks in
    Doc.make (Block.Blocks (blocks', meta))
  | Some yaml, other ->
    Doc.make (Block.Blocks ([ Frontmatter.Frontmatter yaml; other ], Meta.none))
;;

let commonmark_of_doc (doc : Cmarkit.Doc.t) : string =
  let r =
    List.fold
      ~f:Cmarkit_renderer.compose
      ~init:(Cmarkit_commonmark.renderer ())
      [ Cmarkit_renderer.make ~block:Frontmatter.block_commonmark_renderer () ]
  in
  Cmarkit_renderer.doc_to_string r doc
;;

(* {1 sexp of Cmarkit.{Meta.t, Block.t, Inline.t} }

   Each submodule that introduces an extension constructor or a meta key
   provides a converter of type {!Common.inline_sexp} / {!Common.block_sexp}
   / {!Common.meta_sexp}. Here we compose them, placing the core converters
   last so extensions win on their constructors. *)

(* Wikilinks are represented by {!Cmarkit.Inline.Wikilink} when the [~wikilink]
   parser knob is enabled. *)
let wikilink_sexp_of_inline : Common.inline_sexp =
  fun _recurse ~with_meta:_ i ->
  match i with
  | Cmarkit.Inline.Ext_wikilink (wl, _) ->
    Some (Sexp.List [ Atom "Wikilink"; Common.sexp_of_wikilink wl ])
  | _ -> None
;;

(* Div fences are represented by {!Cmarkit.Block.Div} when the [~div] parser
   knob is enabled. *)
let div_sexp_of_block : Common.block_sexp =
  fun ~recurse_inline:_ ~recurse_block ~with_meta b ->
  match b with
  | Cmarkit.Block.Ext_div (d, meta) ->
    let class_sexp =
      match Cmarkit.Block.Div.class' d with
      | Some (cls, _) -> Sexp.List [ Atom "class"; Atom cls ]
      | None -> Sexp.List [ Atom "class" ]
    in
    Some
      (with_meta
         meta
         (Sexp.List
            [ Atom "Div"; class_sexp; recurse_block (Cmarkit.Block.Div.block d) ]))
  | _ -> None
;;

(* Block IDs are attached to paragraph metadata as {!Cmarkit.Block.Block_id}
   values when the [~block_id] parser knob is enabled. *)
let block_id_sexp_of_meta : Common.meta_sexp =
  fun meta ->
  Cmarkit.Block.Block_id.find meta
  |> Option.map ~f:(fun bid ->
    Sexp.List [ Atom "block-id"; Atom (Cmarkit.Block.Block_id.id bid) ])
;;

(* Inline/block attributes are wrapper nodes carrying the merged
   {!Cmarkit.Attribute.t} and the target. *)
let inline_attributes_sexp_of_inline : Common.inline_sexp =
  fun recurse ~with_meta i ->
  match i with
  | Cmarkit.Inline.Ext_attributes (a, m) ->
    let attrs = Cmarkit.Inline.Attributes.attributes a in
    Some
      (with_meta
         m
         (Sexp.List
            [ Atom "Attributes"
            ; Atom (Cmarkit.Attribute.to_string attrs)
            ; recurse (Cmarkit.Inline.Attributes.inline a)
            ]))
  | _ -> None
;;

let block_attributes_sexp_of_block : Common.block_sexp =
  fun ~recurse_inline:_ ~recurse_block ~with_meta b ->
  match b with
  | Cmarkit.Block.Ext_attributes (a, m) ->
    let attrs = Cmarkit.Block.Attributes.attributes a in
    Some
      (with_meta
         m
         (Sexp.List
            [ Atom "Attributes"
            ; Atom (Cmarkit.Attribute.to_string attrs)
            ; recurse_block (Cmarkit.Block.Attributes.block a)
            ]))
  | _ -> None
;;

(* Callout metadata carries only kind and fold; the title lives in the
   block-quote body. *)
let callout_sexp_of_meta : Common.meta_sexp =
  fun meta ->
  Cmarkit.Block.Callout.find meta
  |> Option.map ~f:(fun c ->
    let fold =
      match Cmarkit.Block.Callout.fold c with
      | None -> Sexp.List []
      | Some Cmarkit.Block.Callout.Foldable_open -> Sexp.Atom "Foldable_open"
      | Some Cmarkit.Block.Callout.Foldable_closed -> Sexp.Atom "Foldable_closed"
    in
    Sexp.List
      [ Atom "callout"
      ; Sexp.List
          [ Sexp.List [ Atom "kind"; Atom (Cmarkit.Block.Callout.kind c) ]
          ; Sexp.List [ Atom "fold"; fold ]
          ]
      ])
;;

let sexp_of_ =
  Common.make_sexp_of
    ~inlines:[ wikilink_sexp_of_inline; inline_attributes_sexp_of_inline ]
    ~blocks:
      [ Frontmatter.sexp_of_block
      ; div_sexp_of_block
      ; Struct.sexp_of_block
      ; block_attributes_sexp_of_block
      ]
    ~metas:
      [ Heading_slug.sexp_of_meta
      ; block_id_sexp_of_meta
      ; callout_sexp_of_meta
      ; Cb_attribute.sexp_of_meta
      ]
    ()
;;

let sexp_of_inline = sexp_of_.inline
let sexp_of_block = sexp_of_.block
let sexp_of_meta = sexp_of_.meta
let sexp_of_doc = sexp_of_.doc

(** {1:test Test} *)

module For_test = struct
  let make_block (s : string) : Cmarkit.Block.t =
    let doc = of_string s in
    Cmarkit.Doc.block doc
  ;;

  let pp_doc (doc : Cmarkit.Doc.t) : unit =
    let block = Cmarkit.Doc.block doc in
    block |> sexp_of_block |> Sexp.to_string_hum ~indent:2 |> print_endline
  ;;
end

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

(** {2 Interactions}

{3 Oy_div and Struct}

Tests for interaction between {!module-"Oy_div"} and {!module-"Struct"}

The open and closing fence of div should not be keyed.

*)

let%test_module "Oy_div and Struct" =
  (module struct
    let full_commonmark_of_doc = commonmark_of_doc

    open Common.For_test
    open For_test

    let example_basic =
      ( "basic"
      , {|::: warning
Here is a paragraph.

And here is another.
:::|}
      , 1 )
    ;;

    let example_no_class = "no_class", {|:::
content
:::
|}, 1

    let example_nested_divs =
      ( "nested_divs"
      , {|:::: outer
::: inner
content
:::
::::
|}
      , 2 )
    ;;

    let example_nested_divs_same_length =
      "nested_divs_same_length", {|::: warning
content
:::
:::|}, 2
    ;;

    let example_EOF_closes = "EOF_closes", {|::: warning
unclosed content|}, 1

    let example_extra_closing_fence =
      "extra_closing_fence", {|::: warning
content
:::
:::|}, 2
    ;;

    let non_example_less_than_3_colons =
      "less_than_3_colons", {|:: not-a-div
content
::|}, 0
    ;;

    let non_example_extra_words_after_class =
      "extra_words_after_class", {|::: warning extra
content
:::|}, 0
    ;;

    let non_example_div_does_not_interfere_with_code_blocks =
      "div_does_not_interfere_with_code_blocks", {|```
::: not-a-div
```|}, 0
    ;;

    let example_closing_fence_must_be_at_least_as_long =
      "closing_fence_must_be_at_least_as_long", {|:::: warning
content
:::
::::|}, 2
    ;;

    let examples =
      [ example_basic
      ; example_no_class
      ; example_nested_divs
      ; example_nested_divs_same_length
      ; example_EOF_closes
      ; example_extra_closing_fence
      ; non_example_less_than_3_colons
      ; non_example_div_does_not_interfere_with_code_blocks
      ; example_closing_fence_must_be_at_least_as_long
      ]
    ;;

    let count_div (doc : Cmarkit.Doc.t) : int =
      let folder =
        Cmarkit.Folder.make
          ~block:(fun f acc -> function
             | Cmarkit.Block.Ext_div (d, _) ->
               Cmarkit.Folder.ret
                 (1 + Cmarkit.Folder.fold_block f acc (Cmarkit.Block.Div.block d))
             | Cmarkit.Block.Ext_keyed ((_label, block), _) ->
               Cmarkit.Folder.ret (Cmarkit.Folder.fold_block f acc block)
             | _ -> Cmarkit.Folder.default)
          ()
      in
      Cmarkit.Folder.fold_doc folder 0 doc
    ;;

    let count_keyed (doc : Cmarkit.Doc.t) : int =
      let folder =
        Cmarkit.Folder.make
          ~block:(fun f acc -> function
             | Cmarkit.Block.Ext_div (d, _) ->
               Cmarkit.Folder.ret
                 (Cmarkit.Folder.fold_block f acc (Cmarkit.Block.Div.block d))
             | Cmarkit.Block.Ext_keyed ((_label, block), _) ->
               Cmarkit.Folder.ret (1 + Cmarkit.Folder.fold_block f acc block)
             | _ -> Cmarkit.Folder.default)
          ()
      in
      Cmarkit.Folder.fold_doc folder 0 doc
    ;;

    let pp_src src =
      print_endline "```md {#original}";
      print_endline src;
      print_endline "```"
    ;;

    (* [n_div]/[n_keyed] are ignored: the actual counts are printed so the expect
       output reflects the parser configuration. *)
    let test ?(n_div : int = 0) ?(n_keyed : int = 0) (_name, src, _expected_n_div) =
      ignore (n_div : int);
      ignore (n_keyed : int);
      pp_src src;
      let doc = of_string src in
      print_endline "```sexp";
      pp_doc doc;
      print_endline "```";
      Printf.printf "n_div=%d n_keyed=%d\n" (count_div doc) (count_keyed doc)
    ;;

    let%expect_test _ =
      example_basic |> test ~n_div:1;
      [%expect
        {|
        ```md {#original}
        ::: warning
        Here is a paragraph.

        And here is another.
        :::
        ```
        ```sexp
        (Div (class warning)
          (Blocks (Paragraph (Text "Here is a paragraph.")) Blank_line
            (Paragraph (Text "And here is another."))))
        ```
        n_div=1 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      example_no_class |> test ~n_div:1;
      [%expect
        {|
        ```md {#original}
        :::
        content
        :::

        ```
        ```sexp
        (Blocks (Div (class) (Paragraph (Text content))) Blank_line)
        ```
        n_div=1 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      example_nested_divs |> test ~n_div:2;
      [%expect
        {|
        ```md {#original}
        :::: outer
        ::: inner
        content
        :::
        ::::

        ```
        ```sexp
        (Blocks (Div (class outer) (Div (class inner) (Paragraph (Text content))))
          Blank_line)
        ```
        n_div=2 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      example_nested_divs_same_length |> test ~n_div:2;
      [%expect
        {|
        ```md {#original}
        ::: warning
        content
        :::
        :::
        ```
        ```sexp
        (Blocks (Div (class warning) (Paragraph (Text content)))
          (Div (class) (Blocks)))
        ```
        n_div=2 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      example_EOF_closes |> test ~n_div:1;
      [%expect
        {|
        ```md {#original}
        ::: warning
        unclosed content
        ```
        ```sexp
        (Div (class warning) (Paragraph (Text "unclosed content")))
        ```
        n_div=1 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      example_extra_closing_fence |> test ~n_div:2;
      [%expect
        {|
        ```md {#original}
        ::: warning
        content
        :::
        :::
        ```
        ```sexp
        (Blocks (Div (class warning) (Paragraph (Text content)))
          (Div (class) (Blocks)))
        ```
        n_div=2 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      non_example_less_than_3_colons |> test ~n_div:0;
      [%expect
        {|
        ```md {#original}
        :: not-a-div
        content
        ::
        ```
        ```sexp
        (Paragraph
          (Inlines (Text ":: not-a-div") (Break soft) (Text content) (Break soft)
            (Text ::)))
        ```
        n_div=0 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      non_example_extra_words_after_class |> test ~n_div:1 ~n_keyed:1;
      [%expect
        {|
        ```md {#original}
        ::: warning extra
        content
        :::
        ```
        ```sexp
        (Blocks
          (Keyed (Text "::: ")
            (Paragraph (Inlines (Text "warning extra") (Break soft) (Text content))))
          (Div (class) (Blocks)))
        ```
        n_div=1 n_keyed=1
        |}]
    ;;

    let%expect_test _ =
      non_example_div_does_not_interfere_with_code_blocks |> test ~n_div:0;
      [%expect
        {|
        ```md {#original}
        ```
        ::: not-a-div
        ```
        ```
        ```sexp
        (Code_block no-info "::: not-a-div")
        ```
        n_div=0 n_keyed=0
        |}]
    ;;

    let%expect_test _ =
      example_closing_fence_must_be_at_least_as_long |> test ~n_div:2;
      [%expect
        {|
        ```md {#original}
        :::: warning
        content
        :::
        ::::
        ```
        ```sexp
        (Div (class warning)
          (Blocks (Paragraph (Text content)) (Div (class) (Blocks))))
        ```
        n_div=2 n_keyed=0
        |}]
    ;;

    let example_absorb_two_codeblocks =
      {|- foo
- bar:
::: two-example
```py
code1
```
```js
code2
```
:::|}
    ;;

    let%expect_test _ =
      test ~n_div:1 ~n_keyed:1 ("", example_absorb_two_codeblocks, 0);
      [%expect
        {|
        ```md {#original}
        - foo
        - bar:
        ::: two-example
        ```py
        code1
        ```
        ```js
        code2
        ```
        :::
        ```
        ```sexp
        (Blocks
          (List (Paragraph (Text foo))
            (Keyed (Text bar:)
              (Div (class two-example)
                (Blocks
                  ((Code_block py code1)
                    (meta (attribute ((lang py) (attribute ())))))
                  ((Code_block js code2)
                    (meta (attribute ((lang js) (attribute ()))))))))))
        ```
        n_div=1 n_keyed=1
        |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      List.iter
        (List.map examples ~f:(fun (_, content, _) -> content))
        ~f:
          (commonmark_of_doc_idempotent
             ~doc_of_string:of_string
             ~commonmark_of_doc:full_commonmark_of_doc)
    ;;
  end)
;;

let%test_module "Block attribute" =
  (module struct
    open For_test

    let%expect_test "attaches to div" =
      let doc =
        of_string
          {|{#foo}
::: warning
body
:::|}
      in
      pp_doc doc;
      [%expect
        {| (Attributes #foo (Div (class warning) (Paragraph (Text body)))) |}]
    ;;

    let%expect_test "attaches to keyed block" =
      let doc =
        of_string
          {|{#foo}
key:
- bar|}
      in
      pp_doc doc;
      [%expect
        {| (Blocks (Attributes #foo (Keyed (Text key:) (List (Paragraph (Text bar)))))) |}]
    ;;

    let%expect_test "attaches to keyed list" =
      let doc =
        of_string
          {|{#foo}
- key:
  - bar|}
      in
      pp_doc doc;
      [%expect
        {| (Attributes #foo (List (Keyed (Text key:) (List (Paragraph (Text bar)))))) |}]
    ;;

    let%expect_test "no attribute" =
      let doc = of_string "foo" in
      pp_doc doc;
      [%expect {| (Paragraph (Text foo)) |}]
    ;;

    (* A djot inline attribute inside a keyable paragraph rides on the inline it
       follows (the fork's [keyed_last_pass] attaches it like the normal inline
       pass). It is content-invisible: keying is unaffected on the value side,
       and the key itself may carry one. *)
    let%expect_test "inline attribute on keyed value" =
      let doc = of_string "key: value{.x}" in
      pp_doc doc;
      [%expect
        {| (Keyed (Text "key: ") (Paragraph (Attributes .x (Text value)))) |}]
    ;;

    let%expect_test "inline attribute on keyed key" =
      let doc = of_string "key{.x}: value" in
      pp_doc doc;
      [%expect
        {|
        (Keyed (Inlines (Attributes .x (Text key)) (Text ": "))
          (Paragraph (Text value)))
        |}]
    ;;

    (* The CommonMark renderer re-emits [{...}] specifiers from the wrapper, so
       block/inline attributes (and an attribute wrapping a keyed node) round-trip
       through [commonmark_of_doc] idempotently. *)
    let%test_unit "commonmark roundtrip is idempotent" =
      List.iter
        [ "{#foo .bar}\nkey:\n- bar"
        ; "_em_{#x .y}"
        ; "{source=\"Iliad\"}\n> Sing, muse"
        ; "word{lang=fr}{.blue}"
        ; "{#water .important key=\"my val\"}\nDon't forget!"
        ; "{#custom .big}\n# Hello world"
        ; (* inline attribute inside a keyable paragraph: value-side and key-side *)
          "key: value{.x}"
        ; "key{.x}: value"
        ; "a: b{.x}: c"
        ]
        ~f:
          (Common.For_test.commonmark_of_doc_idempotent
             ~doc_of_string:of_string
             ~commonmark_of_doc)
    ;;
  end)
;;
