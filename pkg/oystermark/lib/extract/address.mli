(** What a link fragment names inside one note.

    The note itself has no address: where either is meant, an [Address.t option]
    is [None] for the note. *)

type t =
  | Heading of string
  (** The identifier the parser gave the heading. See {!Parse.Common.heading_id}. *)
  | Caret of string (** An Obsidian block identifier [ ^id ]. *)
  | Attr of string (** A djot attribute id [ {#id} ], on a block or on inlines. *)
[@@deriving sexp, equal, compare]

(** Heading identifiers, caret ids and attribute ids share one namespace per
    note, so the id alone does not say which kind of anchor it names. *)
val id : t -> string
