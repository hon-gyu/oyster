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
open Common
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

type block_id =
  | Caret of Block_id.t
  | Heading of string

let mk_mapper () : Cmarkit.Mapper.t =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:(compose_all_inline_maps [ Wikilink.inline_map ])
    ~block:
      (compose_all_block_maps
         [ Heading_slug.mk_block_map ()
         ; Callout.block_map
         ; Attribute.block_map
         ; Block_id.block_map
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
  let cmarkit_doc = Doc.of_string ~strict ~layout ~locs:true body in
  let body_doc = Mapper.map_doc (mk_mapper ()) cmarkit_doc in
  let body_doc = Div.rewrite_doc body_doc in
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
      [ Cmarkit_renderer.make ~inline:Wikilink.inline_commonmark_renderer ()
      ; Cmarkit_renderer.make ~block:Frontmatter.block_commonmark_renderer ()
      ; Cmarkit_renderer.make ~block:Div.block_commonmark_renderer ()
      ; Cmarkit_renderer.make ~block:Struct.block_commonmark_renderer ()
      ]
  in
  Cmarkit_renderer.doc_to_string r doc
;;

(* {1 sexp of Cmarkit.{Meta.t, Block.t, Inline.t} }

   Each submodule that introduces an extension constructor or a meta key
   provides a converter of type {!Common.inline_sexp} / {!Common.block_sexp}
   / {!Common.meta_sexp}. Here we compose them, placing the core converters
   last so extensions win on their constructors. *)

let sexp_of_ =
  Common.make_sexp_of
    ~inlines:[ Wikilink.sexp_of_inline ]
    ~blocks:[ Frontmatter.sexp_of_block; Div.sexp_of_block; Struct.sexp_of_block ]
    ~metas:
      [ Heading_slug.sexp_of_meta
      ; Block_id.sexp_of_meta
      ; Callout.sexp_of_meta
      ; Attribute.sexp_of_meta
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

{3 Div and Struct}

Tests for interaction between {!module-"Div"} and {!module-"Struct"}

The open and closing fence of div should not be keyed.

TODO: the test expection is wrong at the moment.

*)

let%test_module "Div and Struct" =
  (module struct
    open Common.For_test
    open Div.For_test
    open For_test

    let count_div (doc : Cmarkit.Doc.t) : int =
      let folder =
        Cmarkit.Folder.make
          ~block:(fun f acc -> function
             | Div.Ext_div (_div, block) ->
               Cmarkit.Folder.ret (1 + Cmarkit.Folder.fold_block f acc block)
             | Struct.Ext_keyed_block (_label, block)
             | Struct.Ext_keyed_list_item (_label, block) ->
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
             | Div.Ext_div (_div, block) ->
               Cmarkit.Folder.ret (Cmarkit.Folder.fold_block f acc block)
             | Struct.Ext_keyed_block (_label, block)
             | Struct.Ext_keyed_list_item (_label, block) ->
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

    let test ?(n_div : int = 0) ?(n_keyed : int = 0) src =
      pp_src src;
      let doc = of_string src in
      print_endline "```sexp";
      pp_doc doc;
      print_endline "```";
      [%test_result: int] (count_div doc) ~expect:n_div;
      [%test_result: int] (count_keyed doc) ~expect:n_keyed
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
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Blocks (Paragraph (Text "Here is a paragraph.")) Blank_line
              (Paragraph (Text "And here is another.")))))
        ```
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
        (Blocks (Div ((class_name ()) (colons 3)) (Paragraph (Text content)))
          Blank_line)
        ```
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
        (Blocks
          (Div ((class_name (outer)) (colons 4))
            (Div ((class_name (inner)) (colons 3)) (Paragraph (Text content))))
          Blank_line)
        ```
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
        (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        ```
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
        (Blocks
          (Div ((class_name (warning)) (colons 3))
            (Paragraph (Text "unclosed content"))))
        ```
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
        (Blocks (Div ((class_name (warning)) (colons 3)) (Paragraph (Text content)))
          (Div ((class_name ()) (colons 3)) (Blocks)))
        ```
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
        (Blocks (Keyed_block (Text ::) (Paragraph (Text "warning extra")))
          (Paragraph (Text content)) (Div ((class_name ()) (colons 3)) (Blocks)))
        ```
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
        (Blocks
          (Div ((class_name (warning)) (colons 4))
            (Blocks (Paragraph (Text content))
              (Div ((class_name ()) (colons 3)) (Blocks)))))
        ```
        |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      List.iter
        all_examples
        ~f:(commonmark_of_doc_idempotent ~doc_of_string:of_string ~commonmark_of_doc)
    ;;
  end)
;;
