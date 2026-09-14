(** Querying the blocks of a note by position, kind, id and enclosing heading.

    Unlike {!Read}, which returns the blocks an {!Anchor.Address.t} names, a
    query selects over every block of the note, including blocks no address can
    name. This is what [oyster block] is built on. *)

(** {1 Walk} *)

(** A block returned by {!walk}, with the fields used to select it. Properties
    that can be computed from [block], such as its kind or info string, are
    functions rather than fields. *)
type located_block =
  { block : Cmarkit.Block.t
  ; index : int (** 1-based position in the walk, over the whole note *)
  ; addresses : Anchor.Address.t list
    (** The caret and attribute addresses that name this block: those
        {!Read.read} resolves to it. A [ ^id ] on a line of its own names the
        previous block, not the paragraph it is written in. Sorted, without
        duplicates. *)
  ; heading_path : string list (** enclosing heading ids, outermost first *)
  ; heading_text : string list (** the same headings as plain text *)
  }

(** A stable name for the block's kind, matched by {!field-kind} and printed by
    [oyster block -json]. A callout is named [callout], not [block_quote]. *)
val kind_of_block : Cmarkit.Block.t -> string

(** Every block of [blocks] in document order, containers before their
    contents.

    Blank lines, link reference definitions, [Blocks] groupings and
    [Ext_attributes] wrappers are skipped, and the block a wrapper wraps is
    reported instead. List items are skipped because they have no
    [Cmarkit.Block.t] of their own, but their contents are walked. Blocks of a
    kind {!kind_of_block} does not name, such as frontmatter, are skipped.

    @raise when a heading has no identifier: parse with {!Parse.of_string}. *)
val walk : Cmarkit.Block.t list -> located_block list

(** {1 Query} *)

(** A filter over {!walk}. Every [Some] field narrows the result; [None] keeps
    every block. *)
type t =
  { under : string option
    (** Only blocks under the heading with this id or text. The value is
        converted to an id with {!Parse.Common.heading_id_of_text}. *)
  ; direct : bool
    (** With [under], only blocks whose innermost enclosing heading is that
        heading, excluding nested subsections. Ignored without [under]. *)
  ; kind : string option (** Only blocks whose {!kind_of_block} is this. *)
  ; lang : string option (** Only code blocks whose info string is exactly this. *)
  ; attr_id : string option
    (** Only the block a link to [ #id ] names, for a [ {#id} ] attribute. *)
  ; caret_id : string option
    (** Only the block a link to [ #^id ] names, for a [ ^id ] identifier. *)
  ; nth : int option (** Of the blocks the other fields keep, only the [n]th, 1-based. *)
  }

(** The blocks of [blocks] that [query] keeps, in walk order. *)
val run : t -> Cmarkit.Block.t list -> located_block list

(** {1 Content} *)

(** The contents of a container, without the container's own syntax.

    [Literal] is text whose fence and indentation the parser already removed,
    exactly as written. [Markdown] is the inner blocks of a container such as a
    block quote; {!content_string} renders them, so the result can differ from
    the source in formatting (fences, list markers, wrapping). *)
type content =
  | Literal of string
  | Markdown of Cmarkit.Block.t
  | Not_a_container

(** The contents of the block in [located]. [Not_a_container] for blocks without
    a single inner value: paragraphs, headings, tables, thematic breaks and
    lists. *)
val content_of_located_block : located_block -> content

(** The contents of [located] as a string. [defs] are the document's label
    definitions, used to render reference links. [Error kind] when the block is
    not a container. *)
val content_string : defs:Cmarkit.Label.defs -> located_block -> (string, string) Result.t
