module Index = Index
module Embed = Embed
module Rename = Rename
open Core

type t =
  { vault_root : string
  ; index : Index.t
  ; documents : Cmarkit.Doc.t String.Map.t
  ; vault_meta : Cmarkit.Meta.t
  }

val docs : t -> (string * Cmarkit.Doc.t) list
val find_doc : t -> string -> Cmarkit.Doc.t option

(** @param stat_of_path how each vault-relative path becomes a {!Index.file_stat};
    defaults to a dateless stat, for callers that do no IO. *)
val build_index
  :  ?stat_of_path:(Index.Path.t -> Index.file_stat)
  -> md_docs:(string * Cmarkit.Doc.t) list
  -> other_files:string list
  -> unit
  -> Index.t

val set_doc : t -> string -> Cmarkit.Doc.t -> t
val remove_path : t -> string -> t

(** Move every document and asset from its path [p] to [f p], unchanged. See
    {!Index.map_paths}. *)
val map_paths : t -> f:(string -> string) -> t

(** Construct a vault from transformed documents, retaining the base vault's file dates,
    non-note assets, metadata, and root. *)
val of_docs : base:t -> (string * Cmarkit.Doc.t) list -> t

(** Construct a vault from Markdown contents and asset paths without performing IO.
    Links are resolved; embeds are not expanded. *)
val of_files
  :  vault_root:string
  -> md_files:(string * string) list
  -> other_files:string list
  -> t
