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

(** Build and render a vault through the pipeline.

    Stages:
    1. List files and dirs, apply [on_discover].
    2. Parse (frontmatter + cmark + wikilinks + block IDs), apply [on_parse].
       Dirs get a synthetic empty doc.
    3. Build index (dirs excluded), resolve links, apply [on_vault].
    4. Render to HTML. *)
let render_vault
      ?(pipeline : Pipeline.t = Pipeline.default)
      ~(backend_blocks : bool)
      ~(safe : bool)
      (vault_root : string)
  : (string * string) list
  =
  let all_files = Vault.list_files vault_root in
  let all_dirs = Vault.list_dirs vault_root in
  let all_entries = all_files @ all_dirs in
  (* Stage 1: discover *)
  let discovered =
    List.filter all_entries ~f:(fun p -> pipeline.on_discover p all_entries)
  in
  let is_dir (p : string) : bool = String.is_suffix p ~suffix:"/" in
  (* Stage 2: parse *)
  let parsed : (string * Cmarkit.Doc.t) list =
    List.concat_map discovered ~f:(fun rel_path ->
      if is_dir rel_path
      then (
        (* Synthetic empty doc for directories *)
        let empty_doc = Cmarkit.Doc.of_string ~strict:false "" in
        pipeline.on_parse rel_path empty_doc)
      else if String.is_suffix rel_path ~suffix:".md"
      then (
        let full_path = Filename.concat vault_root rel_path in
        let content = In_channel.read_all full_path in
        let doc = Parse.of_string content in
        pipeline.on_parse rel_path doc)
      else [])
  in
  (* Stage 3: Build index, resolve links — dirs excluded from index *)
  let md_docs =
    List.filter parsed ~f:(fun (p, _) ->
      String.is_suffix p ~suffix:".md" && not (is_dir p))
  in
  let other_files =
    List.filter discovered ~f:(fun p ->
      (not (String.is_suffix p ~suffix:".md")) && not (is_dir p))
  in
  let index = Vault.build_index ~md_docs ~other_files in
  let resolved : (string * Cmarkit.Doc.t) list =
    Vault.Resolve.resolve_docs md_docs index
  in
  (* Re-combine: resolved md docs + any non-md outputs from parse (including dirs) *)
  let non_md_parsed =
    List.filter parsed ~f:(fun (p, _) ->
      (not (String.is_suffix p ~suffix:".md")) || is_dir p)
  in
  let all_docs = resolved @ non_md_parsed in
  let vault_ctx : Vault.t =
    { vault_root; index; docs = all_docs; vault_meta = Cmarkit.Meta.none }
  in
  (* Stage 4: on_vault + Render.
     Only emit entries ending in .md — directory entries that weren't
     transformed into concrete .md paths by a pipeline stage are dropped. *)
  List.concat_map all_docs ~f:(fun (rel_path, doc) ->
    let outputs = pipeline.on_vault vault_ctx rel_path doc in
    List.filter_map outputs ~f:(fun (out_path, final) ->
      if String.is_suffix out_path ~suffix:".md"
      then (
        let html = Html.of_doc ~backend_blocks ~safe final in
        Some (out_path, html))
      else None))
;;
