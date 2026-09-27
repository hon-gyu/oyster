(** Link representation (normalized from markdown link and wikilink),
    containing link reference itself and the location of the link in the note.

    This is the "source" part of a resolved edge.

    External links (HTTP, mail) are excluded. *)

(** A link reference before resolution, from a wikilink or a markdown link. *)
module Ref : sig
  (** [#a#b]: a path of heading texts, [ ["a"; "b"] ]. A single segment may
      also name a heading identifier or an attribute id. Non-empty. *)
  type fragment = string list [@@deriving sexp, equal, compare]

  (**
    | Target | Fragment | Meaning |
    |---|---|---|
    | `Some target` | `None` | Another note or asset |
    | `Some target` | `Some fragment` | An anchor in another note |
    | `None` | `Some fragment` | An anchor in the current note |
    | `None` | `None` | Current note or normalized empty reference |
  *)
  type t =
    { target : string option
      (** Authored target name or path. [None] means the current note. *)
    ; fragment : fragment option
    }
  [@@deriving sexp, equal, compare]

  (** A wikilink target as written, [ note#h1#h2 ] in [ [[note#h1#h2|alias]] ],
      split at its first [#] into a path and the [#]-separated heading texts
      after it, as in Obsidian. Each part is stripped of blanks, and empty
      segments are dropped. *)
  val of_wikilink_target : string -> t

  (** Inverse of {!of_wikilink_target}, up to blanks and empty segments. *)
  val to_wikilink_target : t -> string

  (** A markdown link's destination, percent-decoded, split as a wikilink
      target. [None] for an external destination (HTTP, HTTPS, mail, FTP). An
      empty destination becomes [().md], as in Obsidian. *)
  val of_destination : string -> t option

  (** The reference a link or image [target] makes in [doc]. A reference link
      is looked up in [doc]: [None] when its label is not defined, and a label
      djot resolves to a heading gives a fragment of that heading's id. *)
  val of_link_target : Djot.Doc.t -> Djot.Inline.target -> t option

  (** A reference to [address] in the note at [target]. *)
  val of_target_address : target:string -> Anchor.Address.t -> t

  (** [fragment] in wikilink syntax: [#a#b]. *)
  val string_of_fragment : fragment -> string

  (** The anchor in [anchors] that [fragment] names, or [None].

      A fragment names a heading: the last segment matches the heading and the
      earlier segments match its ancestor headings, in order. Levels must increase
      along the path but may skip. A segment matches a heading by its identifier
      or by its {!Parse.Common.heading_key}. A single segment that matches no
      heading can match an attribute id. With duplicates, the first in document
      order is returned. *)
  val resolve_fragment : Anchor.t list -> fragment -> Anchor.t option

  (** [s] with its [%XX] escapes decoded. A malformed escape is kept as written. *)
  val percent_decode : string -> string
end

(** How the authored syntax uses its target. Whether an embed transcludes a
    note or displays an asset is determined after resolution. *)
type kind =
  | Link
  | Embed
[@@deriving sexp, equal, compare]

type t =
  { reference : Ref.t
  ; kind : kind
  ; loc : Djot.Textloc.t
    (** [Djot.Textloc.none] when the document was parsed without locations. *)
  }
[@@deriving sexp, equal, compare]

(** Every link of [doc] in document order: wikilinks, markdown links, and
    markdown images, resolved or not. *)
val of_doc : Djot.Doc.t -> t list
