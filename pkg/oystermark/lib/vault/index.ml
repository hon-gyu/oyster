(** Vault index: walks a vault directory and indexes files, headings, and block IDs. *)

open Core
open Oystermark_base

type heading_entry =
  { text : string
  ; level : int
  }

type file_entry =
  { rel_path : string
  ; headings : heading_entry list
  ; block_ids : string list
  }

type t = { files : file_entry list }

let inline_to_plain_text (inline : Cmarkit.Inline.t) : string =
  let lines = Cmarkit.Inline.to_plain_text ~break_on_soft:false inline in
  String.concat ~sep:"\n" (List.map lines ~f:(String.concat ~sep:""))
;;

(* Use Cmarkit.Folder to extract headings from a document. *)
let extract_headings (doc : Cmarkit.Doc.t) : heading_entry list =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc block ->
        match block with
        | Cmarkit.Block.Heading (h, _meta) ->
          let level = Cmarkit.Block.Heading.level h in
          let inline = Cmarkit.Block.Heading.inline h in
          let text = inline_to_plain_text inline in
          Cmarkit.Folder.ret (acc @ [ { text; level } ])
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc
;;

(* Use Cmarkit.Folder to extract block IDs from a document. *)
let extract_block_ids (doc : Cmarkit.Doc.t) : string list =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc block ->
        match block with
        | Cmarkit.Block.Paragraph (_p, meta) ->
          (* TODO: block id itself points to the block that contains
             its previous sibling inline.
             We should get it.
             *)
          (match Cmarkit.Meta.find Block_id.meta_key meta with
           | Some (bid : Block_id.t) -> Cmarkit.Folder.ret (acc @ [ bid.id ])
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc
;;

(* Recursively list all files, returning relative paths. *)
let rec list_files_recursive ~(root : string) ~(rel_prefix : string) : string list =
  let entries =
    try Sys_unix.ls_dir root with
    | _ -> []
  in
  List.concat_map entries ~f:(fun name ->
    let is_hidden (name : string) : bool = String.is_prefix name ~prefix:"." in
    if is_hidden name
    then []
    else (
      let full_path = Filename.concat root name in
      let rel_path =
        if String.is_empty rel_prefix then name else Filename.concat rel_prefix name
      in
      match Sys_unix.is_directory full_path with
      | `Yes -> list_files_recursive ~root:full_path ~rel_prefix:rel_path
      | _ -> [ rel_path ]))
;;

(** Build a vault index from a root directory. *)
let build (vault_root : string) : t =
  let all_files = list_files_recursive ~root:vault_root ~rel_prefix:"" in
  let files =
    List.map all_files ~f:(fun rel_path ->
      if String.is_suffix rel_path ~suffix:".md"
      then (
        let full_path = Filename.concat vault_root rel_path in
        let content = In_channel.read_all full_path in
        let doc = Cmarkit.Doc.of_string ~strict:false content in
        let mapper =
          Cmarkit.Mapper.make
            ~inline_ext_default:(fun _m i -> Some i)
            ~block:Block_id.tag_block_id_meta
            ()
        in
        let doc = Cmarkit.Mapper.map_doc mapper doc in
        let headings = extract_headings doc in
        let block_ids = extract_block_ids doc in
        { rel_path; headings; block_ids })
      else { rel_path; headings = []; block_ids = [] })
  in
  { files }
;;
