(** A link reference before resolution, from a wikilink or a markdown link. *)

type fragment =
  | Hash_path of string list
  (** [#a#b]: a path of heading texts. A single segment may also name a
      heading identifier or an attribute id. Non-empty. *)
  | Caret_id of string (** [#^id]: an Obsidian block identifier. *)
[@@deriving sexp, equal, compare]

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

val of_wikilink : Cmarkit.Inline.Wikilink.t -> t

(** [None] for an external destination (HTTP, HTTPS, mail, FTP) and for
    reference-style links, which are not supported. An empty destination
    becomes [().md], as in Obsidian. *)
val of_cmark_reference : Cmarkit.Inline.Link.reference -> t option

(** A reference to [address] in the note at [target]. *)
val of_address : target:string -> Address.t -> t

(** [fragment] in wikilink syntax: [#a#b] or [#^id]. *)
val string_of_fragment : fragment -> string

(** The anchor in [anchors] that [fragment] names, or [None].

    A hash path names a heading: the last segment matches the heading and the
    earlier segments match its ancestor headings, in order. Levels must increase
    along the path but may skip. A segment matches a heading by its text or by
    the identifier the parser would give that text. A single segment that
    matches no heading can match an attribute id. A caret id matches the caret
    anchor with that id. With duplicates, the first in document order is
    returned. *)
val resolve_fragment : Anchor.t list -> fragment -> Anchor.t option

(** [s] with its [%XX] escapes decoded. A malformed escape is kept as written. *)
val percent_decode : string -> string
