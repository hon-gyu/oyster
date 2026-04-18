(** Processing pipeline for vault rendering.

    The pipeline is a record of hooks that run at successive stages of vault
    processing.

    Stages (in order):
    1. {b discover} — path only, before reading.  Return [false] to skip.
    2. {b parse} — per-doc concat_map after full parse, before index construction.
    3. {b vault} — full vault transform after indexing and link resolution. *)

open Core
include Common
include On_discover
include On_parse
include Code_exec_

(** Add TOC to page named "home.md".
    @param dir_link controls whether directory entries in the TOC are rendered as
    wikilinks (to [dir/index]) or plain text.  Set to [true] when [dir_index]
    is also in the pipeline. *)
let home_toc ?(dir_link : bool = false) () : t =
  let on_vault : Vault.t -> Vault.t =
    map_each_doc (fun (ctx : Vault.t) (path : string) (doc : Cmarkit.Doc.t) ->
      if not (String.equal path "home.md")
      then [ path, doc ]
      else (
        let toc_paths : string list =
          List.filter_map (Vault.all_entry_paths ctx) ~f:(fun p ->
            if String.is_suffix p ~suffix:"/" then None else Some p)
        in
        let toc_cmark_list = Component.toc_cmark_list ~dir_link toc_paths in
        let block_mapper = add_block `Append toc_cmark_list in
        let mapper = Cmarkit.Mapper.make ~block:block_mapper () in
        let new_home = Cmarkit.Mapper.map_doc mapper doc in
        [ path, new_home ]))
  in
  make ~on_vault ()
;;

(** Generate an index page for each directory entry.
    For a dir path like [subdir/], emits [(subdir/index.md, toc_doc)] where
    [toc_doc] is a page listing the directory's children.
    [immediate_only] when [true] lists only direct children (files and subdirs);
    when [false] lists all descendants as a nested tree.
    Skips if [dir/index.md] already exists in the vault. *)
let dir_index ?(immediate_only : bool = false) () : t =
  let on_vault (ctx : Vault.t) : Vault.t =
    let doc_paths : string list = List.map ctx.docs ~f:fst in
    let non_empty_dirs : string list =
      List.filter ctx.index.dirs ~f:(fun (dir_path : string) ->
        List.exists ctx.docs ~f:(fun (p, _) ->
          String.is_prefix p ~prefix:dir_path && not (String.equal p dir_path)))
    in
    let all_paths : string list = doc_paths @ non_empty_dirs in
    let new_docs : (string * Cmarkit.Doc.t) list =
      List.filter_map ctx.index.dirs ~f:(fun (dir_path : string) ->
        let index_path : string = dir_path ^ "index.md" in
        (* Skip if an explicit index.md already exists *)
        if List.Assoc.mem ctx.docs ~equal:String.equal index_path
        then None
        else (
          (* Skip directories that contain no notes *)
          let has_notes : bool =
            List.exists ctx.docs ~f:(fun (p, _) ->
              String.is_prefix p ~prefix:dir_path && not (String.equal p dir_path))
          in
          if not has_notes
          then None
          else (
            let is_child (p : string) : bool =
              String.is_prefix p ~prefix:dir_path && not (String.equal p dir_path)
            in
            let is_immediate (p : string) : bool =
              let rel : string = String.chop_prefix_exn p ~prefix:dir_path in
              not (String.mem (String.rstrip ~drop:(Char.equal '/') rel) '/')
            in
            let rel_children : string list =
              List.filter_map all_paths ~f:(fun p ->
                if is_child p && ((not immediate_only) || is_immediate p)
                then
                  if String.is_suffix p ~suffix:"/"
                  then (
                    let dir_name : string =
                      String.chop_suffix_exn p ~suffix:"/"
                      |> String.chop_prefix_exn ~prefix:dir_path
                    in
                    Some dir_name)
                  else Some (String.chop_prefix_exn p ~prefix:dir_path)
                else None)
            in
            let toc_block : Cmarkit.Block.t =
              Component.toc_cmark_list ~path_prefix:dir_path ~dir_link:true rel_children
            in
            Some (index_path, Cmarkit.Doc.make toc_block))))
    in
    { ctx with docs = ctx.docs @ new_docs }
  in
  make ~on_vault ()
;;

(** Append backlink component to every note's last block. *)
let backlinks : t =
  let on_vault : Vault.t -> Vault.t =
    map_each_doc (fun (ctx : Vault.t) (path : string) (doc : Cmarkit.Doc.t) ->
      let html : string = Component.backlinks path ctx in
      match html with
      | "" -> [ path, doc ]
      | content ->
        let block_mapper = add_html_code_block `Append content in
        let mapper =
          Cmarkit.Mapper.make
            ~inline_ext_default:(fun _m i -> Some i)
            ~block_ext_default:(fun _m b -> Some b)
            ~block:block_mapper
            ()
        in
        [ path, Cmarkit.Mapper.map_doc mapper doc ])
  in
  make ~on_vault ()
;;

(** Append an interactive graph widget to [home.md].
    [view] controls which dir/tag clusters appear and which are selected by
    default. *)
let home_graph ?(config : Config.Home_graph_view.t = Config.Home_graph_view.default) ()
  : t
  =
  let open Vault_graph in
  let open Graph_view in
  let on_vault (ctx : Vault.t) : Vault.t =
    let g = of_vault ctx in
    let html = to_widget_html ~config g in
    let docs =
      List.map ctx.docs ~f:(fun (path, doc) ->
        if not (String.equal path "home.md")
        then path, doc
        else (
          let block_mapper = add_html_code_block `Append html in
          let mapper =
            Cmarkit.Mapper.make
              ~inline_ext_default:(fun _m i -> Some i)
              ~block_ext_default:(fun _m b -> Some b)
              ~block:block_mapper
              ()
          in
          path, Cmarkit.Mapper.map_doc mapper doc))
    in
    { ctx with docs }
  in
  make ~on_vault ()
;;

let default ?(cache : Cache.cache option) ?(config : Config.t = Config.default) () : t =
  id
  >> exclude_draft_by_note_name
  >> exclude_unpublish
  >> validate_no_duplicates
  >> drop_keys_in_frontmatter [ "publish"; "draft" ]
  >> drop_emtpy_frontmatter
  >> transclude_code_files
  >> py_executor ?cache ()
  >> dot_render ()
  >> backlinks
  >> home_graph ~config:config.home_graph_view ()
  >> home_toc ~dir_link:true ()
  >> dir_index ()
;;

let basic : t = id >> backlinks

(** Build a pipeline from a {!Config.t}, dispatching on [pipeline_profile]. *)
let of_config ?(cache : Cache.cache option) ~(config : Config.t) () : t =
  match config.pipeline_profile with
  | Config.Pipeline_profile_def.Default -> default ?cache ~config ()
  | Basic -> basic
  | None_profile -> id
;;

(* Test
==================== *)

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

let%test_module "code executor cache" =
  (module struct
    let echo_doc =
      Parse.of_string
        {|
```echo {}
hello
```
|}
    ;;

    let echo_hash doc =
      Code_executor.hash_fn_of_lang "echo" (Code_executor.extract_exec_ctx doc)
    ;;

    let run_echo cache doc =
      (code_exec
         ~cache
         ~executor:Code_executor.echo_executor
         ~hash_fn:(Code_executor.hash_fn_of_lang "echo")
         ())
        .on_parse
        "test.md"
        doc
      |> List.hd_exn
      |> snd
    ;;

    let%expect_test "cache hit — cached output rendered, not real execution" =
      let cache = Cache.empty_cache () in
      Cache.cache_set
        cache
        ~path:"test.md"
        ~hash:(echo_hash echo_doc)
        ~outputs:[ { Code_executor.id = 0; res = `Markdown "CACHED" } ];
      let doc' = run_echo cache echo_doc in
      print_endline (Parse.commonmark_of_doc doc');
      [%expect
        {|
        ```echo {}
        hello
        ```
        ```
        CACHED
        ```
        |}]
    ;;

    let%expect_test "cache miss — executes and populates cache" =
      let cache = Cache.empty_cache () in
      let doc' = run_echo cache echo_doc in
      print_endline (Parse.commonmark_of_doc doc');
      let cached = Cache.cache_lookup cache ~path:"test.md" ~hash:(echo_hash echo_doc) in
      print_s [%sexp (cached : Code_executor.output list option)];
      [%expect
        {|
        ```echo {}
        hello
        ```
        ```
        hello
        ```

        ((((id 0) (res (Markdown hello)))))
        |}]
    ;;
  end)
;;
