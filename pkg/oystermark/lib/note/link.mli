(** A link in a note: its {!Link_ref.t}, how it uses its target, and its
    location.

    External destinations (HTTP, mail) are not included. *)

(** How the authored syntax uses its target. Whether an embed transcludes a
    note or displays an asset is determined after resolution. *)
type kind =
  | Link
  | Embed
[@@deriving sexp, equal, compare]

type t =
  { reference : Link_ref.t
  ; kind : kind
  ; loc : Cmarkit.Textloc.t
    (** [Cmarkit.Textloc.none] when the document was parsed without locations. *)
  }
[@@deriving sexp, equal, compare]

(** Every link of [doc] in document order: wikilinks, markdown links, and
    markdown images, resolved or not. *)
val of_doc : Cmarkit.Doc.t -> t list
