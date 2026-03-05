module Resolve = Resolve
module Index = Index
module Link_ref = Link_ref
open Core

type t = Index.t * (string * Cmarkit.Doc.t) list

(** Build a vault index from a root directory.
    Returns the index and a list of [(rel_path, doc)] pairs for each markdown file,
    where each [doc] has already been through pass-1 parsing (wikilinks + block IDs). *)
let build (vault_root : string) : t =
  let all_files = Index.list_files_recursive ~root:vault_root ~rel_prefix:"" in
  let files_and_docs =
    List.filter_map all_files ~f:(fun rel_path ->
      if String.is_suffix rel_path ~suffix:".md"
      then (
        let full_path = Filename.concat vault_root rel_path in
        let content = In_channel.read_all full_path in
        let doc = Oystermark_base.of_string content in
        let headings = Index.extract_headings doc in
        let block_ids = Index.extract_block_ids doc in
        Some (({ rel_path; headings; block_ids } : Index.file_entry), (rel_path, doc)))
      else None)
  in
  let files =
    let non_md =
      List.filter_map all_files ~f:(fun rel_path ->
        if String.is_suffix rel_path ~suffix:".md"
        then None
        else Some ({ rel_path; headings = []; block_ids = [] } : Index.file_entry))
    in
    List.map files_and_docs ~f:fst @ non_md
  in
  { files }, List.map files_and_docs ~f:snd
;;
