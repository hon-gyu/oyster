(** {1 File-level parsing}

Each module provides a single-pass mapper that might
- introduce new inline or block extensions
- rewrite Cmarkit.Doc AST
- add metadata to AST nodes
- {!Frontmatter} operates on the raw file content before Cmarkit.Doc parsing.
  Every other mapper operates on the Cmarkit.Doc AST.
- Some mappers operate on node of Cmarkit AST, i.e. [Block.t] or [Inline.t]. Their
  provided mapper follows the signature of [Cmarkit.Inline.t Cmarkit.Mapper.mapper]
  or [Cmarkit.Block.t Cmarkit.Mapper.mapper]
- `-> other mappers rely on multiple nodes as input, thus operates on the whole
  [Cmarkit.Doc.t]. E.g., div fences (see [Cmarkit.Block.Ext_div]) and {!Struct}

*)

module Common = Common
module Frontmatter = Frontmatter
module Textloc_conv = Textloc_conv
module Struct = Struct

type block_id =
  | Caret of Cmarkit.Block.Block_id.t
  | Heading of string

(** [of_string ?strict ?layout ?enable_struct s] parses markdown string [s] into a
    [Cmarkit.Doc.t] with frontmatter embedded as a {!Frontmatter.Frontmatter}
    block and wikilinks/block IDs parsed. Heading identifiers are assigned by the
    parser ([~heading_auto_ids:true]) and read via {!Common.heading_id}.
    [enable_struct] controls the post-parse structured-list rewrite. *)
val of_string
  :  ?strict:bool
  -> ?layout:bool
  -> ?locs:bool
  -> ?enable_struct:bool
  -> string
  -> Cmarkit.Doc.t

val commonmark_of_doc : Cmarkit.Doc.t -> string

(** {1 Sexp}

    Sexp converters covering every extension this library parses. *)

val sexp_of_inline : Cmarkit.Inline.t -> Sexplib0.Sexp.t
val sexp_of_block : Cmarkit.Block.t -> Sexplib0.Sexp.t
val sexp_of_meta : Cmarkit.Meta.t -> Sexplib0.Sexp.t list
val sexp_of_doc : Cmarkit.Doc.t -> Sexplib0.Sexp.t

(** {1:test Test} *)

module For_test : sig
  val make_block : string -> Cmarkit.Block.t
  val pp_doc : Format.formatter -> Cmarkit.Doc.t -> unit
end
