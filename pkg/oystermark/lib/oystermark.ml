(** Oystermark extends CommonMark with Obsidian-style wikilinks and block
    references.

    Processing uses a staged {!Pipeline.t}:
    1. {b discover} — filter paths before reading
    2. {b frontmatter} — filter/transform after frontmatter extraction
    3. {b parse} — transform after full parse, before indexing
    4. {b index} — transform with full vault context (link resolution lives here) *)

open Core

module Parse = Parse
module Vault = Vault
module Html = Html
module Pipeline = Pipeline

(** Build a [Cmarkit.Mapper.t] that resolves links against the vault index. *)
let resolution_cmarkit_mapper ~(index : Vault.Index.t) ~(curr_file : string)
  : Cmarkit.Mapper.t
  =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i ->
      match i with
      | Parse.Wikilink.Ext_wikilink (w, meta) ->
        let link_ref = Vault.Link_ref.of_wikilink w in
        let result = Vault.Resolve.resolve link_ref curr_file index in
        let meta' = Cmarkit.Meta.add Vault.Resolve.resolved_key result meta in
        Some (Parse.Wikilink.Ext_wikilink (w, meta'))
      | other -> Some other)
    ~inline:(fun _m i ->
      match i with
      | Cmarkit.Inline.Link (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Vault.Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = Vault.Resolve.resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add Vault.Resolve.resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Link (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | Cmarkit.Inline.Image (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Vault.Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = Vault.Resolve.resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add Vault.Resolve.resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Image (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default)
    ()
;;

(** Link resolution as a pipeline on_index hook. *)
let resolve_hook : Pipeline.vault_ctx -> string -> Parse.doc -> Parse.doc option =
  fun (ctx : Pipeline.vault_ctx) rel_path pdoc ->
  let mapper = resolution_cmarkit_mapper ~index:ctx.index ~curr_file:rel_path in
  Some { pdoc with doc = Cmarkit.Mapper.map_doc mapper pdoc.doc }
;;

(** The default pipeline: exclude drafts (at frontmatter stage),
    then resolve links (at index stage). *)
let default_pipeline : Pipeline.t =
  Pipeline.compose Pipeline.exclude_drafts
    { Pipeline.default with on_index = resolve_hook }
;;

(** Build and render a vault through the pipeline.

    Stages:
    1. List files, apply [on_discover].
    2. Read + extract frontmatter, apply [on_frontmatter].
    3. Full parse, apply [on_parse].
    4. Build index.
    5. Apply [on_index] with vault context.
    6. Render to HTML. *)
let render_vault
  ?(pipeline : Pipeline.t = default_pipeline)
  ~(safe : bool)
  (vault_root : string)
  : (string * string) list
  =
  let all_files = Vault.list_files vault_root in
  (* Stage 1: discover *)
  let discovered = List.filter all_files ~f:pipeline.on_discover in
  (* Stage 2+3: frontmatter + parse *)
  let parsed =
    List.filter_map discovered ~f:(fun rel_path ->
      if not (String.is_suffix rel_path ~suffix:".md")
      then None
      else (
        let full_path = Filename.concat vault_root rel_path in
        let content = In_channel.read_all full_path in
        let { Parse.Frontmatter.yaml; body } = Parse.Frontmatter.of_string content in
        match pipeline.on_frontmatter rel_path yaml with
        | None -> None
        | Some yaml' ->
          let cmarkit_doc = Cmarkit.Doc.of_string ~strict:false body in
          let doc = Cmarkit.Mapper.map_doc Parse.mapper cmarkit_doc in
          let pdoc : Parse.doc =
            { doc; frontmatter = yaml'; meta = Cmarkit.Meta.none }
          in
          (match pipeline.on_parse rel_path pdoc with
           | None -> None
           | Some pdoc' -> Some (rel_path, pdoc'))))
  in
  (* Build index *)
  let other_files =
    List.filter discovered ~f:(fun p -> not (String.is_suffix p ~suffix:".md"))
  in
  let index = Vault.build_index ~md_docs:parsed ~other_files in
  (* Stage 4: on_index *)
  let vault_ctx : Pipeline.vault_ctx =
    { vault_root; index; docs = parsed; vault_meta = Cmarkit.Meta.none }
  in
  List.filter_map parsed ~f:(fun (rel_path, pdoc) ->
    match pipeline.on_index vault_ctx rel_path pdoc with
    | None -> None
    | Some final ->
      let html = Html.of_doc ~safe ~frontmatter:final.frontmatter final.doc in
      Some (rel_path, html))
;;
