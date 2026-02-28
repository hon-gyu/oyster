(** Obsidian block identifier types and detection. *)

(** The type for block identifiers (without the [^] prefix). *)
type t = string

(** Meta key to tag blocks with a block identifier. *)
val meta_key : t Cmarkit.Meta.key

(** [extract_trailing s] checks if [s] ends with a block identifier pattern.
    Returns [Some (text_before, block_id)] or [None]. *)
val extract_trailing : string -> (string * t) option
