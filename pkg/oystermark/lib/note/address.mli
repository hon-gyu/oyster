(** A location in a note that a link fragment can name.

    Where the whole note is also possible, [Address.t option] is used, with
    [None] for the note. *)

type t =
  | Heading of string
  (** The identifier the parser gave the heading. See {!Parse.Common.heading_id}. *)
  | Caret of string (** An Obsidian block identifier [ ^id ]. *)
  | Attr of string (** A djot attribute id [ {#id} ], on a block or on inlines. *)
[@@deriving sexp, equal, compare]

(** The id without its kind. Heading, caret and attribute ids share one
    namespace per note, so an id alone can be ambiguous. *)
val id : t -> string
