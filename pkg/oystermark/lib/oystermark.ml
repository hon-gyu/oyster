(** Oystermark extends CommonMark with Obsidian-style wikilinks and block
    references.

    Processing uses a staged {!Pipeline.t}:
    1. {b discover} — filter paths before reading
    2. {b parse} — transform after full parse, before indexing
    3. {b vault} — transform with full vault context (after link resolution) *)

open Core
module Parse = Parse
module Vault = Vault
module Html = Html
module Pipeline = Pipeline

(** Build and render a vault through the pipeline.

    Stages:
    1. List files, apply [on_discover].
    2. Parse (frontmatter + cmark + wikilinks + block IDs), apply [on_parse].
    3. Build index, resolve links, apply [on_vault] with vault context.
    4. Render to HTML. *)
let render_vault
      ?(pipeline : Pipeline.t = Pipeline.default)
      ?(top_fm_keys_ignored = Set.of_list (module String) [ "published"; "draft" ])
      ~(backend_blocks : bool)
      ~(safe : bool)
      (vault_root : string)
  : (string * string) list
  =
  let all_files = Vault.list_files vault_root in
  (* Stage 1: discover *)
  let discovered = List.filter all_files ~f:pipeline.on_discover in
  (* Stage 2: parse *)
  let parsed =
    List.filter_map discovered ~f:(fun rel_path ->
      if not (String.is_suffix rel_path ~suffix:".md")
      then None
      else (
        let full_path = Filename.concat vault_root rel_path in
        let content = In_channel.read_all full_path in
        let doc = Parse.of_string content in
        match pipeline.on_parse rel_path doc with
        | None -> None
        | Some doc' -> Some (rel_path, doc')))
  in
  (* Stage 3: Build index, resolve links *)
  let other_files =
    List.filter discovered ~f:(fun p -> not (String.is_suffix p ~suffix:".md"))
  in
  let index = Vault.build_index ~md_docs:parsed ~other_files in
  let resolved : (string * Cmarkit.Doc.t) list =
    Vault.Resolve.resolve_docs parsed index
  in
  let vault_ctx : Vault.t =
    { vault_root; index; docs = resolved; vault_meta = Cmarkit.Meta.none }
  in
  (* Stage 4: Render *)
  List.filter_map resolved ~f:(fun (rel_path, doc) ->
    match pipeline.on_vault vault_ctx rel_path doc with
    | None -> None
    | Some final ->
      let html = Html.of_doc ~top_fm_keys_ignored ~backend_blocks ~safe final in
      Some (rel_path, html))
;;
