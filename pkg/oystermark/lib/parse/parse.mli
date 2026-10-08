(** {1 File-level parsing}

    A note is djot ({!Djot}) in the {!profile} syntax, after an optional YAML
    frontmatter block ({!Djot.Frontmatter}). *)

module Common = Common
module Textloc_conv = Textloc_conv

(** [Djot.Profile.markdown_like] with wikilinks, keyed blocks and callouts. *)
val profile : Djot.Profile.t

(** A note: its blocks, and the frontmatter it started with. *)
type t = Djot.Doc.t

(** [of_string ?locs s] parses [s], frontmatter included. Locations are
    positions in [s]. [locs] defaults to [true]. *)
val of_string : ?locs:bool -> string -> t

(** The frontmatter's mapping as a YAML value. *)
val frontmatter : t -> Yaml.value option

(** A document of [blocks] in {!profile}, without locations. *)
val doc_of_blocks : ?frontmatter:Djot.Frontmatter.t -> Djot.Block.t Djot.node list -> t

(** {!Djot.Doc.to_string} of {!doc_of_blocks}. *)
val source_of_blocks : Djot.Block.t Djot.node list -> string

(** [blocks], which are among the blocks of [doc], without the identifiers in
    {!Djot.Doc.auto_identifiers}. {!doc_of_blocks} keeps an identifier it is
    given as a written one, and derives the ones removed here again. *)
val without_derived_ids
  :  Djot.Doc.t
  -> Djot.Block.t Djot.node list
  -> Djot.Block.t Djot.node list

(** Folds the blocks of a document in source order. Footnote definitions are
    among them, where they were written. *)
val fold_doc : 'a Djot.Folder.t -> 'a -> Djot.Doc.t -> 'a

(** {1 Sexp}

    For inspecting trees in tests. Attributes are printed after the node they
    are on. *)

val sexp_of_inline : Djot.Inline.t Djot.node -> Sexplib0.Sexp.t
val sexp_of_block : Djot.Block.t Djot.node -> Sexplib0.Sexp.t
val sexp_of_doc : Djot.Doc.t -> Sexplib0.Sexp.t

module For_test : sig
  val pp_doc : Format.formatter -> Djot.Doc.t -> unit
end
