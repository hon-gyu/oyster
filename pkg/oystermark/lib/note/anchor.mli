(** An anchor in a note, with its location. *)

(** A location in a note that a link fragment can name.

    Where the whole note is also possible, [Address.t option] is used, with
    [None] for the note. *)
module Address : sig
  type t =
    | Heading of string
    (** The heading's identifier: its section's [id] attribute, or its own for
        a heading that opens no section. *)
    | Attr of string (** A djot attribute id [ {#id} ], on a block or on inlines. *)
  [@@deriving sexp, equal, compare]

  (** The id without its kind. Heading and attribute ids share one namespace
      per note, so an id alone can be ambiguous. *)
  val id : t -> string
end

type heading =
  { text : string (** The heading as plain text. *)
  ; level : int
  ; slug : string (** See {!Address.Heading}. *)
  }
[@@deriving sexp, equal, compare]

(** An anchor as its note defines it. Its {!val-address} is the name a link resolves
    to it by. *)
type definition =
  | Heading of heading
  | Attr of
      { id : string
      ; inline : bool (** Whether [ {#id} ] is on inlines rather than a block. *)
      }
[@@deriving sexp, equal, compare]

type t =
  { definition : definition
  ; loc : Djot.Textloc.t
    (** The heading's, not its section's, for a heading. For an attribute,
        the node's with its [ \{...\} ] spec.
        [Djot.Textloc.none] when the document was parsed without locations. *)
  }
[@@deriving sexp, equal, compare]

val address : definition -> Address.t

(** The location of [node] in [doc] extended over its attribute specs
    [ \{...\} ], which a block's precede and an inline's follow. *)
val extent : Djot.Doc.t -> _ Djot.node -> Djot.Textloc.t

(** Every anchor of [doc] in document order, duplicates included. An id on a
    section or a heading is a heading anchor only. *)
val of_doc : Djot.Doc.t -> t list
