module Index = Index
module Link_ref = Link_ref
module Resolve = Resolve
open Core

type t = Index.t * (string * Parse.doc) list

(** List all files in the vault (relative paths, hidden dirs excluded). *)
let list_files (vault_root : string) : string list =
  Index.list_files_recursive ~root:vault_root ~rel_prefix:""
;;

(** Build an index from a list of [(rel_path, parsed_doc)] pairs
    plus a list of non-md relative paths. *)
let build_index
  ~(md_docs : (string * Parse.doc) list)
  ~(other_files : string list)
  : Index.t
  =
  let md_entries =
    List.map md_docs ~f:(fun (rel_path, (pdoc : Parse.doc)) ->
      let headings = Index.extract_headings pdoc.doc in
      let block_ids = Index.extract_block_ids pdoc.doc in
      ({ rel_path; headings; block_ids } : Index.file_entry))
  in
  let non_md =
    List.map other_files ~f:(fun rel_path ->
      ({ rel_path; headings = []; block_ids = [] } : Index.file_entry))
  in
  { files = md_entries @ non_md }
;;

(** Simple build: read all .md files, optionally filter, build index.
    For pipeline-aware builds, use the lower-level functions directly. *)
let build (vault_root : string)
  : t
  =
  let all_files = list_files vault_root in
  let files_and_docs =
    List.filter_map all_files ~f:(fun rel_path ->
      if String.is_suffix rel_path ~suffix:".md"
      then (
        let full_path = Filename.concat vault_root rel_path in
        let content = In_channel.read_all full_path in
        let parsed = Parse.of_string content in
        Some (rel_path, parsed))
      else None)
  in
  let other_files =
    List.filter all_files ~f:(fun p -> not (String.is_suffix p ~suffix:".md"))
  in
  let index = build_index ~md_docs:files_and_docs ~other_files in
  index, files_and_docs
;;
