(** {1 File-level parsing}

    A note is djot ({!Djot}) in the {!profile} syntax, after an optional YAML
    frontmatter block. {!Frontmatter} splits the frontmatter off the raw
    content; everything else is djot's parser. *)

module Common = Common
module Frontmatter = Frontmatter
module Textloc_conv = Textloc_conv

(** [Djot.Profile.markdown_like] with wikilinks, keyed blocks and callouts. *)
val profile : Djot.Profile.t

type t =
  { frontmatter : Yaml.value option
  ; doc : Djot.Doc.t (** Without the frontmatter. *)
  }

(** [of_string ?locs s] parses [s]. The frontmatter is blanked rather than
    stripped before parsing (see {!Frontmatter.blank_frontmatter}), so the
    locations in [doc] are positions in [s]. [locs] defaults to [true]. *)
val of_string : ?locs:bool -> string -> t

(** [t] as source text: the frontmatter, then {!Djot.Source.of_doc}. *)
val to_string : t -> string

(** A document of [blocks] in {!profile}, without locations. *)
val doc_of_blocks : Djot.Block.t Djot.node list -> Djot.Doc.t

(** {!Djot.Source.of_doc} of {!doc_of_blocks}. *)
val source_of_blocks : Djot.Block.t Djot.node list -> string

(** {1 Sexp}

    For inspecting trees in tests. Attributes are printed after the node they
    are on. *)

val sexp_of_inline : Djot.Inline.t Djot.node -> Sexplib0.Sexp.t
val sexp_of_block : Djot.Block.t Djot.node -> Sexplib0.Sexp.t
val sexp_of_doc : Djot.Doc.t -> Sexplib0.Sexp.t

module For_test : sig
  val pp_doc : Format.formatter -> Djot.Doc.t -> unit
end
