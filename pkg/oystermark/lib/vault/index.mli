(** A unified vault index: a persistent snapshot of every note, asset, anchor
    and link in a vault, with the queries that clients (LSP, publishing,
    command line) share.

    A snapshot is immutable. {!set_note}, {!remove_note}, {!set_asset} and
    {!remove_asset} return a new snapshot and leave the old one valid, so a
    client can update one file at a time. *)

open! Core

type loc = Cmarkit.Textloc.t

val sexp_of_loc : loc -> Sexp.t
val loc_of_sexp : Sexp.t -> loc
val compare_loc : loc -> loc -> int
val equal_loc : loc -> loc -> bool

(** A canonical vault path:
    - non-empty
    - vault-relative
    - separated by [/]
    - free of leading [./]
    - no empty components
    - no [.] or [..] components
    - case-preserving
    - no Unicode normalization *)
module Path : sig
  type t = string

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t

  (** @return [Error] when [s] violates any of the constraints above. *)
  val of_string : string -> (t, Error.t) result

  val to_string : t -> string

  (** The parent directory, or [None] for a path at the vault root. *)
  val dirname : t -> t option

  val basename : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

type heading = Note.Anchor.heading =
  { text : string
  ; level : int
  ; slug : string
  }
[@@deriving sexp, equal, compare]

type file_stat =
  { rel_path : Path.t
  ; birthtime : (int * int * int) option (** created date YYYY/MM/DD, when available *)
  ; mtime : (int * int * int) option (** modified date YYYY/MM/DD, when available *)
  }

(** See {!Note.Anchor.value}. *)
type anchor_value = Note.Anchor.value =
  | Heading of heading
  | Caret of string
  | Attr of
      { id : string
      ; inline : bool
      }
[@@deriving sexp, equal, compare]

module Anchor : sig
  type t = Note.Anchor.t =
    { value : anchor_value
    ; loc : loc (** non-none loc *)
    }
  [@@deriving sexp, equal, compare]
end

(** Authored link; a note can compute this without an index *)
module Link : sig
  (** How the authored syntax uses its target. Whether an embed transcludes a
      note or displays an asset is determined after resolution. *)
  type kind = Note.Link.kind =
    | Link
    | Embed
  [@@deriving sexp, equal, compare]

  type t = Note.Link.t =
    { reference : Note.Link_ref.t
    ; kind : kind
    ; loc : loc (** Non-none loc *)
    }
  [@@deriving sexp, equal, compare]
end

module Entry : sig
  type t

  (** @return [Error] if the document is not parsed with source locations enabled (missing location information) *)
  val of_doc : file_stat -> Cmarkit.Doc.t -> (t, string) result

  (** Like {!of_doc}, raising on a document missing source locations. *)
  val of_doc_exn : file_stat -> Cmarkit.Doc.t -> t

  val path : t -> Path.t
  val file_stat : t -> file_stat

  (** Parsed YAML frontmatter, when the note opens with a frontmatter block. *)
  val frontmatter : t -> Yaml.value option

  val frontmatter_field : t -> string -> Yaml.value option

  (** Tags declared in the frontmatter [tags] key, with duplicates removed. *)
  val tags : t -> string list

  (** The note's creation date, as [(year, month, day)].

      Read from the frontmatter [created] key, then [date], then whatever the
      loader recorded as {!type-file_stat}'s [birthtime]. *)
  val created : t -> (int * int * int) option

  (** The note's modification date, as [(year, month, day)].

      Read from the frontmatter [updated] key, then whatever the loader
      recorded as {!type-file_stat}'s [mtime]. *)
  val modified : t -> (int * int * int) option

  (** The note's display title.

      Resolved in order of authorial intent:
        explicit frontmatter [title]
        > first level-1 heading
        > basename without its extension *)
  val title : t -> string

  (** all authored anchor occurrence (including duplicates) in document order *)
  val anchors : t -> Anchor.t list

  (** The note's headings, in document order. *)
  val headings : t -> (heading * loc) list

  (** All link references in a note, returned in document order.
      - includes both resolved and unresolved links
      - includes both markdown and wiki links
      - does not include external links (HTTP/mail link) *)
  val links : t -> Link.t list
end

(** Non-note assets (images, etc.) *)
module Asset : sig
  type t

  val create : file_stat -> t
  val path : t -> Path.t
end

type target =
  | Note of Path.t
  | Asset of Path.t
  | Anchor of
      { note_path : Path.t
      ; anchor : Anchor.t
      }
[@@deriving sexp, equal, compare]

type resolution_error =
  | Missing_path
  | Missing_anchor of Path.t
  (** the base path exists and the fragment does not resolve, whether that base target is a note or asset *)

(** Result of resolving an authored reference. *)
type resolution = (target, resolution_error) result

(** An authored link viewed through a resolved target *)
type backlink =
  { source : Path.t
  ; link : Link.t
  }

val target_path : target -> Path.t

(** A vault snapshot. *)
type t

val empty : t

(** Returned in ascending canonical path order. *)
val notes : t -> Entry.t list

(** Returned in ascending canonical path order. *)
val assets : t -> Asset.t list

val find_note : t -> Path.t -> Entry.t option
val find_asset : t -> Path.t -> Asset.t option

(** Insert or replace the note at its canonical path, removing any asset at the same path.

    - atomically update its anchors and outgoing occurrences;
    - update all reverse-reference queries affected by the change;
    - return a new index snapshot;
    - leave the old snapshot valid and unchanged.

    The result of any update sequence is observationally equivalent to rebuilding the index
    from its resulting notes and assets. *)
val set_note : t -> Entry.t -> t

(** remove a note from the index. no-op when absent *)
val remove_note : t -> Path.t -> t

(** Insert or replace the asset at its canonical path, removing any note at the same path. *)
val set_asset : t -> Asset.t -> t

(** remove an asset from the index. no-op when absent *)
val remove_asset : t -> Path.t -> t

(** Move every note and asset from its path [p] to [f p], keeping its content.
    Links are not rewritten, so they resolve from the moved sources as authored.
    When [f] maps two paths to one, the later in path order is kept. *)
val map_paths : t -> f:(Path.t -> Path.t) -> t

(** Resolve an authored reference related to a [source] note.

    On duplicated anchors, the first one in document order is returned.

    @param source
      The path of the note containing the link reference. It doesn't need to be
      present in the index. *)
val resolve : t -> Path.t -> Note.Link_ref.t -> resolution

(** The links of the note at the given path that fail to resolve, in document
    order. [ [] ] when the note is absent from the index. *)
val unresolved_links : t -> Path.t -> (Link.t * resolution_error) list

(** Every resolved edge in the vault, in ascending source path order and then
    document order. *)
val all_backlinks : t -> (target * backlink) list

(** The resolved edges landing on [path], whatever target kind they name. *)
val backlinks_at_path : t -> Path.t -> (target * backlink) list

(** @param include_anchors If true, also includes edge pointing to any anchor owned by the note
    @return incoming links {b to} a note of [tgt_path], in document order *)
val backlinks_of_note : ?include_anchors:bool -> t -> Path.t -> backlink list

(** @return incoming links {b to} [target], in document order *)
val backlinks_of_target : t -> target -> backlink list

(** A note is orphaned when no successfully resolved ordinary link or embed from a different
    note targets either the note or one of its anchors. *)
val is_orphan : t -> Path.t -> bool

(** Returned in ascending canonical path order. *)
val orphans : t -> Path.t list
