module Index = Index
module Link_ref = Link_ref
module Embed = Embed
module Rename = Rename
open Core

type t =
  { vault_root : string
  ; index : Index.t
  ; documents : Cmarkit.Doc.t String.Map.t
  ; vault_meta : Cmarkit.Meta.t
  }

let docs : t -> (string * Cmarkit.Doc.t) list = fun vault -> Map.to_alist vault.documents
let find_doc (vault : t) path = Map.find vault.documents path

open struct
  let file_stat ?(birthtime = None) ?(mtime = None) rel_path : Index.file_stat =
    { rel_path; birthtime; mtime }
  ;;
end

(** @param stat_of_path how each vault-relative path becomes a {!Index.file_stat};
    defaults to a dateless stat, for callers that do no IO. *)
let build_index
      ?(stat_of_path : Index.Path.t -> Index.file_stat = fun path -> file_stat path)
      ~(md_docs : (string * Cmarkit.Doc.t) list)
      ~(other_files : string list)
      ()
  : Index.t
  =
  let index =
    List.fold md_docs ~init:Index.empty ~f:(fun index (path, doc) ->
      Index.set_note index (Index.Note.of_doc_exn (stat_of_path path) doc))
  in
  List.fold other_files ~init:index ~f:(fun index path ->
    Index.set_asset index (Index.Asset.create (stat_of_path path)))
;;

let set_doc (vault : t) path doc : t =
  let stat =
    Index.find_note vault.index path
    |> Option.value_map ~default:(file_stat path) ~f:Index.Note.file_stat
  in
  { vault with
    index = Index.set_note vault.index (Index.Note.of_doc_exn stat doc)
  ; documents = Map.set vault.documents ~key:path ~data:doc
  }
;;

let remove_path (vault : t) path : t =
  { vault with
    index = Index.remove_asset (Index.remove_note vault.index path) path
  ; documents = Map.remove vault.documents path
  }
;;

(** Move every document and asset from its path [p] to [f p], unchanged. See
    {!Index.map_paths}. *)
let map_paths (vault : t) ~(f : string -> string) : t =
  { vault with
    index = Index.map_paths vault.index ~f
  ; documents =
      Map.to_alist vault.documents
      |> List.map ~f:(fun (path, doc) -> f path, doc)
      |> String.Map.of_alist_reduce ~f:(fun _ last -> last)
  }
;;

(** Construct a vault from transformed documents, retaining the base vault's file dates,
    non-note assets, metadata, and root. *)
let of_docs ~(base : t) (docs : (string * Cmarkit.Doc.t) list) : t =
  let index =
    List.fold docs ~init:Index.empty ~f:(fun index (path, doc) ->
      let stat =
        Index.find_note base.index path
        |> Option.value_map ~default:(file_stat path) ~f:Index.Note.file_stat
      in
      Index.set_note index (Index.Note.of_doc_exn stat doc))
  in
  let index = List.fold (Index.assets base.index) ~init:index ~f:Index.set_asset in
  { base with index; documents = String.Map.of_alist_exn docs }
;;

(** Construct a vault from Markdown contents and asset paths without performing IO.
    Links are resolved; embeds are not expanded. *)
let of_files
      ~(vault_root : string)
      ~(md_files : (string * string) list)
      ~(other_files : string list)
  : t
  =
  let parsed_docs =
    List.map md_files ~f:(fun (path, content) -> path, Parse.of_string ~locs:true content)
  in
  let index = build_index ~md_docs:parsed_docs ~other_files () in
  { vault_root
  ; index
  ; documents = String.Map.of_alist_exn parsed_docs
  ; vault_meta = Cmarkit.Meta.none
  }
;;
