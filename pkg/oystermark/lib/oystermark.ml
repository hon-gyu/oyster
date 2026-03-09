(** Oystermark extends CommonMark with Obsidian-style wikilinks and block
    references.

    Processing uses a staged {!Pipeline.t}:
    1. {b discover} — filter paths before reading
    2. {b parse} — concat_map after full parse, before indexing
    3. {b vault} — concat_map with full vault context (after link resolution) *)

open Core
module Parse = Parse
module Vault = Vault
module Html = Html
module Pipeline = Pipeline
module Theme = Theme

(** Build and render a vault through the pipeline.

    Stages:
    1. List files and dirs, apply [on_discover].
    2. Parse [.md] files only, apply [on_parse].
    3. Build index, resolve links, apply [on_vault] to all entries
       (docs + dirs with synthetic empty docs), then render to HTML. *)
let render_vault
      ?(pipeline : Pipeline.t = Pipeline.default)
      ?(theme : Theme.t = Theme.none)
      ~(backend_blocks : bool)
      ~(safe : bool)
      (vault_root : string)
  : (string * string) list
  =
  let all_entries = Vault.list_entries vault_root in
  (* Stage 1: discover *)
  let discovered =
    List.filter all_entries ~f:(fun p -> pipeline.on_discover p all_entries)
  in
  let is_dir (p : string) : bool = String.is_suffix p ~suffix:"/" in
  let dirs : string list = List.filter discovered ~f:is_dir in
  (* Stage 2: parse — only .md files go through on_parse *)
  let parsed : (string * Cmarkit.Doc.t) list =
    List.concat_map discovered ~f:(fun rel_path ->
      if String.is_suffix rel_path ~suffix:".md"
      then (
        let full_path = Filename.concat vault_root rel_path in
        let content = In_channel.read_all full_path in
        let doc = Parse.of_string content in
        pipeline.on_parse rel_path doc)
      else [])
  in
  (* Stage 3: Build index, resolve links *)
  let md_docs = List.filter parsed ~f:(fun (p, _) -> String.is_suffix p ~suffix:".md") in
  let other_files =
    List.filter discovered ~f:(fun p ->
      (not (String.is_suffix p ~suffix:".md")) && not (is_dir p))
  in
  let index = Vault.build_index ~md_docs ~other_files ~dirs in
  let resolved : (string * Cmarkit.Doc.t) list =
    Vault.Resolve.resolve_docs md_docs index
  in
  let vault_ctx : Vault.t =
    { vault_root; index; docs = resolved; vault_meta = Cmarkit.Meta.none }
  in
  (* Stage 4: on_vault + Render *)
  let final_vault : Vault.t = pipeline.on_vault vault_ctx in
  let sidebar_paths : string list =
    List.filter_map final_vault.docs ~f:(fun (p, _) ->
      if String.is_suffix p ~suffix:".md" then Some p else None)
  in
  let sidebar : string =
    Component.toc_html
      ~dir_href_f:(fun dir -> Some (Html.note_url_path (dir ^ "/index.md")))
      ~leaf_href_f:Html.file_url_path
      ~collapsible:true
      ~collapsed_by_default:true
      sidebar_paths
  in
  List.filter_map final_vault.docs ~f:(fun (rel_path, final) ->
    if String.is_suffix rel_path ~suffix:".md"
    then (
      let body = Html.of_doc ~backend_blocks ~safe final in
      let url_path = Html.note_url_path rel_path in
      let title : string = Component.title_of_path rel_path in
      let nav : string = Component.nav_of_url_path url_path in
      let sidebar : string = if String.equal rel_path "home.md" then "" else sidebar in
      let page = Theme.{ title; body; url_path; nav; sidebar } in
      let html = theme page in
      Some (Html.note_output_path rel_path, html))
    else None)
;;
