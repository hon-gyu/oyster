(** Oystermark extends CommonMark with Obsidian-style wikilinks and block
    references.

    Processing uses a staged {!Pipeline.t}:
    1. {b discover} — filter paths before reading
    2. {b frontmatter} — filter/transform after frontmatter extraction
    3. {b parse} — transform after full parse, before indexing
    4. {b vault} — transform with full vault context (after link resolution) *)

open Core
module Parse = Parse
module Vault = Vault
module Html = Html
module Pipeline = Pipeline

(** Build and render a vault through the pipeline.

    Stages:
    1. List files, apply [on_discover].
    2. Read + extract frontmatter, apply [on_frontmatter].
    3. Full parse, apply [on_parse].
    4. Build index, resolve links, apply [on_vault] with vault context.
    5. Render to HTML. *)
let render_vault
      ?(pipeline : Pipeline.t = Pipeline.default)
      ~(backend_blocks : bool)
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
          let pdoc : Parse.doc = { doc; frontmatter = yaml'; meta = Cmarkit.Meta.none } in
          (match pipeline.on_parse rel_path pdoc with
           | None -> None
           | Some pdoc' -> Some (rel_path, pdoc'))))
  in
  (* Stage 4: Build index, resolve links *)
  let other_files =
    List.filter discovered ~f:(fun p -> not (String.is_suffix p ~suffix:".md"))
  in
  let index = Vault.build_index ~md_docs:parsed ~other_files in
  let resolved : (string * Parse.doc) list = Vault.Resolve.resolve_docs parsed index in
  let vault_ctx : Vault.t =
    { vault_root; index; docs=resolved; vault_meta = Cmarkit.Meta.none }
  in
  (* TODO: current we only have body.  *)
  List.filter_map resolved ~f:(fun (rel_path, pdoc) ->
    match pipeline.on_vault vault_ctx rel_path pdoc with
    | None -> None
    | Some final ->
      let (html_body : string) =
        Html.of_doc ~backend_blocks ~safe final.frontmatter final.doc
      in
      let html = html_body in
      Some (rel_path, html))
;;
