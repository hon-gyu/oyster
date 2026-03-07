(** Processing pipeline for vault rendering.

    The pipeline is a record of hooks that run at successive stages of vault
    processing.  Each hook receives exactly the data available at its stage
    and returns a list of [(path, doc)] pairs (concat_map style):
    - [[]] to drop the note
    - [[(path, doc)]] to keep/transform it
    - [[(p1, d1); (p2, d2); ...]] to expand one input into multiple outputs

    Stages (in order):
    1. {b discover} — path only, before reading.  Return [false] to skip.
    2. {b parse} — after full parse, before index construction.
    3. {b vault} — after indexing and link resolution; full vault context available. *)

open Core

(** Pipeline: a record of hooks, one per stage. *)
type t =
  { on_discover : string -> string list -> bool
  ; on_parse : string -> Cmarkit.Doc.t -> (string * Cmarkit.Doc.t) list
  ; on_vault : Vault.t -> string -> Cmarkit.Doc.t -> (string * Cmarkit.Doc.t) list
  }

(** The identity pipeline — passes everything through unchanged. *)
let id : t =
  { on_discover = (fun _path _paths -> true)
  ; on_parse = (fun path doc -> [ path, doc ])
  ; on_vault = (fun _ctx path doc -> [ path, doc ])
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
    Short-circuits on [false]/empty. *)
let compose (a : t) (b : t) : t =
  { on_discover = (fun p ps -> a.on_discover p ps && b.on_discover p ps)
  ; on_parse =
      (fun path doc ->
        List.concat_map (a.on_parse path doc) ~f:(fun (p', d') -> b.on_parse p' d'))
  ; on_vault =
      (fun ctx path doc ->
        List.concat_map (a.on_vault ctx path doc) ~f:(fun (p', d') ->
          b.on_vault ctx p' d'))
  }
;;

let ( >> ) a b = compose a b

(* {1 Built-in pipelines} *)

(** Validate that a [.md] file does not conflict with a same-named directory.
    e.g. [note1.md] and [note1/] would both produce [note1/index.html]. *)
let validate_no_duplicates : t =
  let on_discover (path : string) (paths : string list) : bool =
    match String.chop_suffix path ~suffix:".md" with
    | None -> true
    | Some stem ->
      let dir_prefix = stem ^ "/" in
      if List.exists paths ~f:(fun p -> String.is_prefix p ~prefix:dir_prefix)
      then
        failwith
          (Printf.sprintf
             "Conflict: %s and %s/ both produce %s/index.html"
             path
             stem
             stem)
      else true
  in
  make ~on_discover ()
;;

(** Exclude notes that has [.draft] in stem. Apply on discover stage. *)
let exclude_draft_by_note_name : t =
  make
    ~on_discover:(fun path _paths -> not (String.is_suffix ~suffix:".draft.md" path))
    ()
;;

(** Exclude files with [draft: true] frontmatter. Apply on parse stage. *)
let exclude_drafts : t =
  make
    ~on_parse:(fun path doc ->
      match Parse.Frontmatter.of_doc doc with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "draft" with
         | Some (`Bool true) -> []
         | _ -> [ path, doc ])
      | _ -> [ path, doc ])
    ()
;;

(** Exclude files without [publish: true] frontmatter. Apply on parse stage. *)
let exclude_unpublish : t =
  make
    ~on_parse:(fun path doc ->
      match Parse.Frontmatter.of_doc doc with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "publish" with
         | Some (`Bool true) -> [ path, doc ]
         | _ -> [])
      | _ -> [])
    ()
;;

let drop_keys_in_frontmatter (keys : string list) : t =
  let yaml_f : Yaml.value -> Yaml.value = function
    | `O fields ->
      `O
        (List.filter_map fields ~f:(fun (k, v) ->
           if List.mem keys k ~equal:String.equal then None else Some (k, v)))
    | other -> other
  in
  make
    ~on_parse:(fun path doc ->
      let b_mapper = Parse.Frontmatter.make_block_mapper yaml_f in
      let mapper =
        Cmarkit.Mapper.make
          ~inline_ext_default:(fun _m i -> Some i)
          ~block_ext_default:(fun _m b -> Some b)
          ~block:b_mapper
          ()
      in
      [ path, Cmarkit.Mapper.map_doc mapper doc ])
    ()
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

let add_html_code_block
      ?(after_frontmatter = true)
      (loc : [ `Prepend | `Append ])
      (content : string)
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
  make ~on_parse:(fun path doc -> [ path, Mapper.map_doc mapper doc ]) ()
;;

(** Add TOC to page named "home.md" *)
let home_toc : t =
  let on_vault (ctx : Vault.t) (path : string) (doc : Cmarkit.Doc.t)
    : (string * Cmarkit.Doc.t) list
    =
    let all_note_paths = Vault.all_note_paths ctx in
    if not (String.equal path "home.md")
    then [ path, doc ]
    else (
      let toc_cmark_list = Component.toc_cmark_list all_note_paths in
      let block_mapper = add_block `Append toc_cmark_list in
      let mapper = Cmarkit.Mapper.make ~block:block_mapper () in
      let new_home = Cmarkit.Mapper.map_doc mapper doc in
      [ path, new_home ])
  in
  make ~on_vault ()
;;

let default : t =
  exclude_draft_by_note_name
  >> exclude_unpublish
  >> drop_keys_in_frontmatter [ "publish"; "draft" ]
  >> home_toc
;;

let%test_module "prepend block" =
  (module struct
    let%expect_test "prepend_block after_frontmatter inserts after frontmatter" =
      let block_mapper =
        add_html_code_block `Prepend ~after_frontmatter:true "<nav>toc</nav>"
      in
      let pipeline = of_block_mapper block_mapper in
      let doc = Parse.of_string "---\ntitle: Hello\n---\n# Heading\n\nBody text." in
      let doc' = pipeline.on_parse "test.md" doc |> List.hd_exn |> snd in
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
      let doc' = pipeline.on_parse "test.md" doc |> List.hd_exn |> snd in
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
      let doc' = pipeline.on_parse "test.md" doc |> List.hd_exn |> snd in
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
      let doc' = pipeline.on_parse "test.md" doc |> List.hd_exn |> snd in
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
