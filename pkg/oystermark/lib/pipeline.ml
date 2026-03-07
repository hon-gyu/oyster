(** Processing pipeline for vault rendering.

    The pipeline is a record of hooks that run at successive stages of vault
    processing.  Each hook receives exactly the data available at its stage
    and returns [Some _] to keep the note or [None] to drop it.

    Stages (in order):
    1. {b discover} — path only, before reading.  Return [false] to skip.
    2. {b parse} — after full parse, before index construction.
    3. {b vault} — after indexing and link resolution; full vault context available. *)

open Core

(** Pipeline: a record of hooks, one per stage. *)
type t =
  { on_discover : string -> bool
  ; on_parse : string -> Cmarkit.Doc.t -> Cmarkit.Doc.t option
  ; on_vault : Vault.t -> string -> Cmarkit.Doc.t -> Cmarkit.Doc.t option
  }

(** The identity pipeline — passes everything through unchanged. *)
let id : t =
  { on_discover = (fun _path -> true)
  ; on_parse = (fun _path doc -> Some doc)
  ; on_vault = (fun _ctx _path doc -> Some doc)
  }
;;

(** Make a pipeline from individual hooks. *)
let make
      ?(on_discover = id.on_discover)
      ?(on_parse = id.on_parse)
      ?(on_vault = id.on_vault)
      ()
  =
  { on_discover; on_parse; on_vault }
;;

(** Compose two pipelines: run [a] then [b] at each stage.
    Short-circuits on [None]/[false]. *)
let compose (a : t) (b : t) : t =
  { on_discover = (fun p -> a.on_discover p && b.on_discover p)
  ; on_parse =
      (fun path doc ->
        match a.on_parse path doc with
        | None -> None
        | Some doc' -> b.on_parse path doc')
  ; on_vault =
      (fun ctx path doc ->
        match a.on_vault ctx path doc with
        | None -> None
        | Some doc' -> b.on_vault ctx path doc')
  }
;;

let ( >> ) a b = compose a b

(* {1 Built-in pipelines} *)

(** Exclude files with [draft: true] frontmatter. Apply on parse stage. *)
let exclude_drafts : t =
  make
    ~on_parse:(fun _path doc ->
      match Parse.Frontmatter.of_doc doc with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "draft" with
         | Some (`Bool true) -> None
         | _ -> Some doc)
      | _ -> Some doc)
    ()
;;

(** Exclude files without [publish: true] frontmatter. Apply on parse stage. *)
let exclude_unpublish : t =
  make
    ~on_parse:(fun _path doc ->
      match Parse.Frontmatter.of_doc doc with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "publish" with
         | Some (`Bool true) -> Some doc
         | _ -> None)
      | _ -> None)
    ()
;;

(** Exclude notes that has `.draft` in stem. Apply on discover stage. *)
let exclude_draft_by_note_name : t =
  make ~on_discover:(fun path -> not (String.is_suffix ~suffix:".draft.md" path)) ()
;;

let add_block
      ?(after_frontmatter = true)
      (loc : [ `Prepend | `Append ])
      (new_b : Cmarkit.Block.t)
  : Cmarkit.Block.t Cmarkit.Mapper.mapper
  =
  let open Cmarkit in
  let fired = ref false in
  fun _m (b : Block.t) ->
    if !fired
    then Mapper.default
    else (
      fired := true;
      match b with
      | Block.Blocks (blocks, meta) ->
        let blocks' =
          match after_frontmatter, blocks with
          | true, (Parse.Frontmatter.Frontmatter _ as fm) :: body ->
            (match loc with
             | `Prepend -> fm :: new_b :: body
             | `Append -> (fm :: body) @ [ new_b ])
          | _ ->
            (match loc with
             | `Prepend -> new_b :: blocks
             | `Append -> blocks @ [ new_b ])
        in
        Mapper.ret (Block.Blocks (blocks', meta))
      | _ ->
        Mapper.ret
          (match loc with
           | `Prepend -> Block.Blocks ([ new_b; b ], Meta.none)
           | `Append -> Block.Blocks ([ b; new_b ], Meta.none)))
;;

let add_html_code_block ?(after_frontmatter = true) (loc : [ `Prepend | `Append ]) (content : string)
  : Cmarkit.Block.t Cmarkit.Mapper.mapper
  =
  let open Cmarkit in
  let cb : Block.Code_block.t =
    Block.Code_block.make
      ~info_string:("=html", Meta.none)
      (Block_line.list_of_string content)
  in
  add_block ~after_frontmatter loc (Block.Code_block (cb, Meta.none))
;;

let of_block_mapper (block_mapper : Cmarkit.Block.t Cmarkit.Mapper.mapper) : t =
  let open Cmarkit in
  let mapper : Mapper.t =
    Mapper.make ~inline_ext_default:(fun _m i -> Some i) ~block:block_mapper ()
  in
  make ~on_parse:(fun _path doc -> Some (Mapper.map_doc mapper doc)) ()
;;

(** Add TOC to page named "home.md" *)
let home_toc : t =
  let on_vault (ctx : Vault.t) (path : string) (doc : Cmarkit.Doc.t)
    : Cmarkit.Doc.t option
    =
    let all_note_paths = Vault.all_note_paths ctx in
    if not (String.equal path "home.md")
    then Some doc
    else (
      let toc_cmark_list = Component.toc_cmark_list all_note_paths in
      let block_mapper = add_block `Append toc_cmark_list in
      let mapper = Cmarkit.Mapper.make ~block:block_mapper () in
      let new_home = Cmarkit.Mapper.map_doc mapper doc in
      Some new_home)
  in
  make ~on_vault ()
;;

let default : t = exclude_draft_by_note_name >> exclude_unpublish >> home_toc

let%test_module "prepend block" =
  (module struct
    let%expect_test "prepend_block after_frontmatter inserts after frontmatter" =
      let block_mapper =
        add_html_code_block `Prepend ~after_frontmatter:true "<nav>toc</nav>"
      in
      let pipeline = of_block_mapper block_mapper in
      let doc = Parse.of_string "---\ntitle: Hello\n---\n# Heading\n\nBody text." in
      let doc' = pipeline.on_parse "test.md" doc |> Option.value_exn in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
        ---
        title: Hello
        ---
        ```=html
        <nav>toc</nav>
        ```
        # Heading

        Body text.
        |}]
    ;;

    let%expect_test
        "prepend_block after_frontmatter without frontmatter prepends normally"
      =
      let block_mapper =
        add_html_code_block `Prepend ~after_frontmatter:true "<nav>toc</nav>"
      in
      let pipeline = of_block_mapper block_mapper in
      let doc = Parse.of_string "# Heading\n\nBody text." in
      let doc' = pipeline.on_parse "test.md" doc |> Option.value_exn in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
    ```=html
    <nav>toc</nav>
    ```
    # Heading

    Body text.
    |}]
    ;;

    let%expect_test "prepend_html_code_block" =
      let block_mapper = add_html_code_block `Prepend "<p>Hello, world!</p>" in
      let pipeline = of_block_mapper block_mapper in
      let doc = Parse.of_string "Hello, world again!" in
      let doc' = pipeline.on_parse "test.md" doc |> Option.value_exn in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
    ```=html
    <p>Hello, world!</p>
    ```
    Hello, world again\!
    |}]
    ;;

    let%expect_test "prepend_html_code_block fires exactly once on multi-block doc" =
      let block_mapper = add_html_code_block `Prepend "<nav>toc</nav>" in
      let pipeline = of_block_mapper block_mapper in
      let doc = Parse.of_string "# Heading\n\nParagraph one.\n\nParagraph two." in
      let doc' = pipeline.on_parse "test.md" doc |> Option.value_exn in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
    ```=html
    <nav>toc</nav>
    ```
    # Heading

    Paragraph one.

    Paragraph two.
    |}]
    ;;
  end)
;;
