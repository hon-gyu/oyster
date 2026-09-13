(** An anchor as it occurs in a note: what an {!Address.t} resolves to. *)

type heading =
  { text : string (** The heading as plain text. *)
  ; level : int
  ; slug : string
    (** The identifier the parser gave the heading. See {!Parse.Common.heading_id}. *)
  }
[@@deriving sexp, equal, compare]

type value =
  | Heading of heading
  | Caret of string (** [ ^id ] on a paragraph or a keyed block. *)
  | Attr of
      { id : string
      ; inline : bool (** Whether [ {#id} ] is on inlines rather than a block. *)
      }
[@@deriving sexp, equal, compare]

type t =
  { value : value
  ; loc : Cmarkit.Textloc.t
    (** [Cmarkit.Textloc.none] when the document was parsed without locations. *)
  }
[@@deriving sexp, equal, compare]

val address : value -> Address.t

(** Every anchor of [doc] in document order, duplicates included.

    Raises when a heading has no identifier: parse with {!Parse.of_string}. *)
val of_doc : Cmarkit.Doc.t -> t list
