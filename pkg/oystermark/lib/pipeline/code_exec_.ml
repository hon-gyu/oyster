open Core
open Common

(** Transclude code files as codeblocks.
    - Applicable to both markdown inline link and wikilink embed.
    If the target file has a file extension in the list, replace it with a codeblock
    with corresponding lang info.
*)
let transclude_code_files : t =
  let lang_of_path (p : string) : string option =
    match String.rsplit2 p ~on:'.' with
    | Some (_, ext) ->
      if
        List.mem
          ([ "py"; "ml"; "mli"; "rs"; "js"; "ts"; "sh"; "bash"
           ; "json"; "yaml"; "yml"; "toml"; "css"; "sql"; "go"; "java"
           ; "c"; "h"; "cpp"; "hpp"; "rb"; "lua"; "zig"; "nix"; "el"; "clj"
           ] [@ocamlformat "disable"])
          ext
          ~equal:String.equal
      then Some ext
      else None
    | None -> None
  in
  let extract_file_target (i : Cmarkit.Inline.t) : string option =
    let meta =
      match i with
      | Parse.Wikilink.Ext_wikilink (w, m) when w.embed -> Some m
      | Cmarkit.Inline.Image (_, m) -> Some m
      | _ -> None
    in
    match meta with
    | Some m ->
      (match Cmarkit.Meta.find Vault.Resolve.resolved_key m with
       | Some (Vault.Resolve.File { path }) ->
         lang_of_path path |> Option.map ~f:(fun _ -> path)
       | _ -> None)
    | None -> None
  in
  let extract_from_inline (inline : Cmarkit.Inline.t) : string option =
    match inline with
    | Cmarkit.Inline.Inlines ([ i ], _) -> extract_file_target i
    | i -> extract_file_target i
  in
  let on_vault : Vault.t -> Vault.t =
    map_each_doc (fun (ctx : Vault.t) (path : string) (doc : Cmarkit.Doc.t) ->
      let mapper =
        Cmarkit.Mapper.make
          ~inline_ext_default:(fun _m i -> Some i)
          ~block_ext_default:(fun _m b -> Some b)
          ~block:(fun _m block ->
            match block with
            | Cmarkit.Block.Paragraph (p, _) ->
              (match extract_from_inline (Cmarkit.Block.Paragraph.inline p) with
               | Some file_path ->
                 (match lang_of_path file_path with
                  | None -> Cmarkit.Mapper.default
                  | Some lang ->
                    let full_path = Filename.concat ctx.vault_root file_path in
                    let content = In_channel.read_all full_path in
                    let cb =
                      Cmarkit.Block.Code_block.make
                        ~info_string:(lang, Cmarkit.Meta.none)
                        (Cmarkit.Block_line.list_of_string content)
                    in
                    Cmarkit.Mapper.ret (Cmarkit.Block.Code_block (cb, Cmarkit.Meta.none)))
               | None -> Cmarkit.Mapper.default)
            | _ -> Cmarkit.Mapper.default)
          ()
      in
      [ path, Cmarkit.Mapper.map_doc mapper doc ])
  in
  make ~on_vault ()
;;

include struct
  let fm_has_pyproject_in_oyster (fm_opt : Parse.Frontmatter.t option) : bool =
    match fm_opt with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal "oyster" with
       | Some (`O pyproject_fields) ->
         List.Assoc.mem pyproject_fields ~equal:String.equal "pyproject"
       | _ -> false)
    | _ -> false
  ;;

  let fm_traced_in_oyster (fm_opt : Parse.Frontmatter.t option) : bool =
    match fm_opt with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal "oyster" with
       | Some (`O pyproject_fields) ->
         (match List.Assoc.find pyproject_fields ~equal:String.equal "traced" with
          | Some (`Bool true) -> true
          | _ -> false)
       | _ -> false)
    | _ -> false
  ;;
end

(** Code executor pipeline: extract code cells, execute them, and merge outputs
    back into the document. Handles caching when [~cache] is provided.

    @param path_filter filter function for document paths.
    @param fm_filter filter function for frontmatter.
    @param cache optional execution cache.
    @param loc_map controls how outputs are spliced (append/replace/silent).
    @param executor the computation that produces outputs from an exec_ctx.
    @param hash_fn cache key derivation; use {!Cache.make_hash_fn} to build one.
    *)
let code_exec
      ?(path_filter : string -> bool = fun _ -> true)
      ?(fm_filter : Parse.Frontmatter.t option -> bool = fun _ -> true)
      ?(loc_map : (Parse.Attribute.t option -> [ `Append | `Replace | `Silent ]) option)
      ?(cache : Cache.cache option)
      ~(executor : Code_executor.executor)
      ~(hash_fn : Code_executor.exec_ctx -> string)
      ()
  : t
  =
  make
    ~on_parse:(fun path (doc : Cmarkit.Doc.t) ->
      if (not (path_filter path)) || not (fm_filter (Parse.Frontmatter.of_doc doc))
      then [ path, doc ]
      else (
        let ctx = Code_executor.extract_exec_ctx doc in
        let hash = hash_fn ctx in
        let outputs =
          Cache.run_with ?cache ~path ~hash ~executor:(fun () -> executor ctx) ()
        in
        let doc' =
          match loc_map with
          | Some f -> Code_executor.merge_outputs ~loc_map:f outputs doc
          | None -> Code_executor.merge_outputs outputs doc
        in
        [ path, doc' ]))
    ()
;;

(** Execute Python code blocks in each document and splice the outputs back in.
    Thin wrapper around {!code_exec} with {!Code_executor.Uv} executor and hash. *)
let py_executor
      ?(path_filter : string -> bool = fun _ -> true)
      ?(fm_filter : Parse.Frontmatter.t option -> bool = fm_has_pyproject_in_oyster)
      ?(attr_filter : (Parse.Attribute.t option -> bool) option)
      ?(attr_session_map : (Parse.Attribute.t option -> string) option)
      ?(attr_hash_key : (Parse.Attribute.t option -> string) option)
      ?(loc_map : (Parse.Attribute.t option -> [ `Append | `Replace | `Silent ]) option)
      ?(cache : Cache.cache option)
      ()
  : t
  =
  code_exec
    ~path_filter
    ~fm_filter
    ?loc_map
    ?cache
    ~executor:(Code_executor.Uv.executor ?attr_filter ?attr_session_map)
    ~hash_fn:(Code_executor.Uv.hash_fn ?attr_filter ?attr_hash_key)
    ()
;;

let traced_code_exec
      ?(path_filter : string -> bool = fun _ -> true)
      ?(fm_filter : Parse.Frontmatter.t option -> bool = fm_traced_in_oyster)
      ?(cache : Cache.cache option)
      ~(executor : Code_executor.executor)
      ~(hash_fn : Code_executor.exec_ctx -> string)
      ()
  : t
  =
  code_exec
    ~path_filter
    ~fm_filter
    ~loc_map:(fun _ -> `Replace)
    ?cache
    ~executor:(Code_executor.traced_executor_of_executor executor)
    ~hash_fn
    ()
;;

(** Run a Graphviz layout engine on [dot] code blocks and replace them with
    inline SVG.  The layout engine defaults to ["dot"] but can be overridden
    via the Pandoc attribute [layout], e.g. [```dot {layout=neato}].

    @param on_error controls what is rendered when [dot] fails.
    [`Keep_original] (default) leaves the code block unchanged.
    [`Show_error] replaces it with an [=html] block showing the stderr. *)
let dot_render ?(on_error : [ `Keep_original | `Show_error ] = `Keep_original) () : t =
  code_exec
    ~fm_filter:(fun _ -> true)
    ~loc_map:(fun _ -> `Replace)
    ~executor:(Code_executor.dot_executor ~on_error)
    ~hash_fn:(Code_executor.hash_fn_of_lang "dot")
    ()
;;
