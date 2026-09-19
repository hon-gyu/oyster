(** An anchor in a note, with its location. *)

(** A location in a note that a link fragment can name.

    Where the whole note is also possible, [Address.t option] is used, with
    [None] for the note. *)
module Address : sig
  type t =
    | Heading of string
    (** The identifier the parser gave the heading. See {!Parse.Common.heading_id}. *)
    | Caret of string (** An Obsidian block identifier [ ^id ]. *)
    | Attr of string (** A djot attribute id [ {#id} ], on a block or on inlines. *)
  [@@deriving sexp, equal, compare]

  (** The id without its kind. Heading, caret and attribute ids share one
      namespace per note, so an id alone can be ambiguous. *)
  val id : t -> string
end

type heading =
  { text : string (** The heading as plain text. *)
  ; level : int
  ; slug : string
    (** The identifier the parser gave the heading. See {!Parse.Common.heading_id}. *)
  }
[@@deriving sexp, equal, compare]

(** An anchor as its note defines it. Its {!address} is the name a link resolves
    to it by. *)
type definition =
  | Heading of heading
  | Caret of string (** [ ^id ] on a paragraph or a keyed block. *)
  | Attr of
      { id : string
      ; inline : bool (** Whether [ {#id} ] is on inlines rather than a block. *)
      }
[@@deriving sexp, equal, compare]

type t =
  { definition : definition
  ; loc : Cmarkit.Textloc.t
    (** [Cmarkit.Textloc.none] when the document was parsed without locations. *)
  }
[@@deriving sexp, equal, compare]

val address : definition -> Address.t

(** Every anchor of [doc] in document order, duplicates included.

    @raise when a heading has no identifier: parse with {!Parse.of_string}. *)
val of_doc : Cmarkit.Doc.t -> t list
