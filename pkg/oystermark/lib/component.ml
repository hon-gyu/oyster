(** UI components.

  Usage:
  - Used by {Pipeline} to add components as HTML blocks using [backend_block].
  - Used in {Vault.render_vault}'s last step to render non-body content (WIP).
*)

open Core

type html = string
type doc_component = string * Parse.doc -> html
type vault_component = Vault.t -> html

(** Strip .md extension for SSG-friendly URLs. *)
let strip_md_ext (path : string) : string =
  match String.chop_suffix path ~suffix:".md" with
  | Some p -> p
  | None -> path
;;

(** Render a table of contents as a [<ul>] from a list of relative paths. *)
let toc (paths : string list) : html =
  let items =
    List.map paths ~f:(fun path ->
      let href = strip_md_ext path in
      let label = Filename.chop_extension (Filename.basename path) in
      Printf.sprintf "<li><a href=\"%s\">%s</a></li>" href label)
  in
  "<ul>\n" ^ String.concat ~sep:"\n" items ^ "\n</ul>"
;;

(** Extract all file paths that a resolved doc links to. *)
let extract_outgoing_paths (doc : Cmarkit.Doc.t) : string list =
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun _f acc (i : Cmarkit.Inline.t) ->
        let target_of_meta (meta : Cmarkit.Meta.t) : string option =
          match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
          | Some (Vault.Resolve.File { path }) -> Some path
          | Some (Vault.Resolve.Heading { path; _ }) -> Some path
          | Some (Vault.Resolve.Block { path; _ }) -> Some path
          | _ -> None
        in
        match i with
        | Cmarkit.Inline.Link (_, meta) | Cmarkit.Inline.Image (_, meta) ->
          (match target_of_meta meta with
           | Some path -> Cmarkit.Folder.ret (path :: acc)
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Parse.Wikilink.Ext_wikilink (_, meta) ->
          (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
           | Some (Vault.Resolve.File { path }) -> path :: acc
           | Some (Vault.Resolve.Heading { path; _ }) -> path :: acc
           | Some (Vault.Resolve.Block { path; _ }) -> path :: acc
           | _ -> acc)
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc |> List.dedup_and_sort ~compare:String.compare
;;

(** Render backlinks for [rel_path]: a [<ul>] of all vault docs that link to it. *)
let backlinks (rel_path : string) : vault_component =
  fun (vault : Vault.t) ->
  let linking_paths =
    List.filter_map vault.docs ~f:(fun (src_path, pdoc) ->
      let targets = extract_outgoing_paths pdoc.doc in
      if List.mem targets rel_path ~equal:String.equal then Some src_path else None)
  in
  match linking_paths with
  | [] -> ""
  | paths -> toc (List.sort paths ~compare:String.compare)
;;

(** Render a file explorer as a nested [<ul>] tree from the vault index. *)
let file_explorer : vault_component =
  fun (vault : Vault.t) ->
  let paths =
    List.map vault.index.files ~f:(fun (e : Vault.Index.file_entry) -> e.rel_path)
    |> List.sort ~compare:String.compare
  in
  (* Build a tree: (dir_name, subtree) or (file_name, leaf). *)
  let rec build_tree (entries : (string list * string) list) : html =
    (* Group by first path component *)
    let groups =
      List.map entries ~f:(fun (components, full_path) ->
        match components with
        | [] -> "", [], full_path
        | hd :: tl -> hd, tl, full_path)
      |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> String.compare a b)
    in
    let grouped =
      List.group groups ~break:(fun (a, _, _) (b, _, _) -> not (String.equal a b))
    in
    let items =
      List.map grouped ~f:(fun group ->
        match group with
        | [ (name, [], full_path) ] ->
          (* Leaf file *)
          let href = strip_md_ext full_path in
          Printf.sprintf "<li><a href=\"%s\">%s</a></li>" href name
        | (dir_name, _, _) :: _ ->
          (* Check if this is a directory or a single file with deeper path *)
          let children =
            List.map group ~f:(fun (_, rest, full_path) -> rest, full_path)
          in
          let has_subtree =
            List.exists children ~f:(fun (rest, _) -> not (List.is_empty rest))
          in
          if has_subtree
          then Printf.sprintf "<li>%s\n%s</li>" dir_name (build_tree children)
          else (
            (* Single file at this level *)
            match children with
            | [ ([], full_path) ] ->
              let href = strip_md_ext full_path in
              Printf.sprintf "<li><a href=\"%s\">%s</a></li>" href dir_name
            | _ -> Printf.sprintf "<li>%s\n%s</li>" dir_name (build_tree children))
        | [] -> "")
    in
    "<ul>\n" ^ String.concat ~sep:"\n" items ^ "\n</ul>"
  in
  let entries = List.map paths ~f:(fun p -> String.split p ~on:'/', p) in
  build_tree entries
;;
