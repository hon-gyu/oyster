module Index = Index
module Embed = Embed
module Rename = Rename
open Core

type t =
  { vault_root : string
  ; index : Index.t
  ; documents : Note.t String.Map.t
  }

val docs : t -> (string * Note.t) list
val find_doc : t -> string -> Note.t option

(** @param stat_of_path how each vault-relative path becomes a {!Index.file_stat};
    defaults to a dateless stat, for callers that do no IO. *)
val build_index
  :  ?stat_of_path:(Index.Path.t -> Index.file_stat)
  -> md_docs:(string * Note.t) list
  -> other_files:string list
  -> unit
  -> Index.t

val set_doc : t -> string -> Note.t -> t
val remove_path : t -> string -> t

(** Move every document and asset from its path [p] to [f p], unchanged. See
    {!Index.map_paths}. *)
val map_paths : t -> f:(string -> string) -> t

(** Construct a vault from Markdown contents and asset paths without performing IO.
    Links are resolved; embeds are not expanded. *)
val of_files
  :  vault_root:string
  -> md_files:(string * string) list
  -> other_files:string list
  -> t

(** [vault] with each document's embeds expanded, see {!Embed.expand_docs}.
    The index is [vault]'s: an entry describes what its file says, so an
    embed's anchors and links stay indexed in the note that wrote them, at
    locations in that note. *)
val expand : ?max_depth:int -> t -> t
