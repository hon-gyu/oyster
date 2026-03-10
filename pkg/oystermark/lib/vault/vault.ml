module Index = Index
module Link_ref = Link_ref
module Resolve = Resolve
module Embed = Embed
open Core

type t =
  { vault_root : string
  ; index : Index.t (** index of all files in the vault *)
  ; docs : (string * Cmarkit.Doc.t) list
  ; vault_meta : Cmarkit.Meta.t
  }

let all_entry_paths (vault : t) : string list =
  let doc_paths : string list = List.map vault.docs ~f:fst in
  doc_paths @ vault.index.dirs
;;

(** List all entries in the vault (files and directories, relative paths).
    Directories have a trailing [/].  Hidden entries are excluded. *)
let list_entries (vault_root : string) : string list =
  Index.list_entries_recursive ~root:vault_root ~rel_prefix:""
;;

(** Build an index from a list of [(rel_path, doc)] pairs
    plus a list of non-md relative paths. *)
let build_index
      ~(md_docs : (string * Cmarkit.Doc.t) list)
      ~(other_files : string list)
      ~(dirs : string list)
  : Index.t
  =
  let md_entries =
    List.map md_docs ~f:(fun (rel_path, doc) ->
      let headings = Index.extract_headings doc in
      let block_ids = Index.extract_block_ids doc in
      ({ rel_path; headings; block_ids } : Index.file_entry))
  in
  let non_md =
    List.map other_files ~f:(fun rel_path ->
      ({ rel_path; headings = []; block_ids = [] } : Index.file_entry))
  in
  { files = md_entries @ non_md; dirs }
;;

(** Simple build: read all .md files, optionally filter, build index.
    For pipeline-aware builds, use the lower-level functions directly. *)
let of_root_path (vault_root : string) : t =
  (* Scan files *)
  let all_files =
    List.filter (list_entries vault_root) ~f:(fun p ->
      not (String.is_suffix p ~suffix:"/"))
  in
  let (docs : (string * Cmarkit.Doc.t) list) =
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
  (* Build index *)
  let index = build_index ~md_docs:docs ~other_files ~dirs:[] in
  (* Resolve *)
  let resolved_docs : (string * Cmarkit.Doc.t) list = Resolve.resolve_docs docs index in
  (* Expand note embeds *)
  let expanded_docs : (string * Cmarkit.Doc.t) list = Embed.expand_docs resolved_docs in
  { vault_root; index; docs = expanded_docs; vault_meta = Cmarkit.Meta.none }
;;
