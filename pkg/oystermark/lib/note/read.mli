(** Reading blocks of a note: the blocks an address names, their source text,
    and a walk over all addressable blocks. *)

(** The blocks in [blocks] named by [address], or [ [] ] if there are none.

    - {b Heading}: the heading and the blocks after it in the same container, up
      to (not including) the next heading of the same or a higher level. The
      heading can be in any container (block quote, list item, div, keyed block,
      footnote), and the section stops at the end of that container. A container
      that follows the heading is included whole, even if it has headings inside.
      For a heading with a block attribute, the section starts at the heading,
      not at the attribute.
    - {b Caret}: one block. If the [^id] ends a paragraph with other content,
      that paragraph. If the [^id] is the whole paragraph, the previous non-blank
      block. If the id is on a {!Cmarkit.Block.Ext_keyed} node (the parser moves
      it there from the paragraph or list item the node replaced), that node,
      including its label and children.
    - {b Attr}: one block. For a block attribute, the block it wraps. For an
      inline attribute, the paragraph or heading that contains it. See
      {!page-"feature-attribute-anchors"}.

    Containers are searched recursively; the first match in document order
    wins. *)
val read : Cmarkit.Block.t list -> Anchor.Address.t -> Cmarkit.Block.t list

(** The source text of [blocks]: [content] from the start of the first block to
    the end of the last, with trailing whitespace removed. [None] if no block
    has a location.

    [content] must be the text [blocks] were parsed from, with locations. The
    slice is taken byte for byte, so inside a container that prefixes each line
    (a block quote's [>], a list item's indentation) every line but the first
    keeps that prefix. *)
val source_text : string -> Cmarkit.Block.t list -> string option

module For_test : sig
  val example_headings : string
  val example_inline_caret_id : string
  val example_blockquote_caret_id : string
  val example_not_found : string
  val example_list_caret_id : string
  val example_nested_list_caret_id : string
end

(* CR: the content below this line feels weird *)

(** {1 Walk} *)

(** A block returned by {!walk}, with the fields used to select it. Properties
    that can be computed from [block], such as its kind or info string, are
    functions rather than fields. *)
type located_block =
  { block : Cmarkit.Block.t
  ; index : int (** 1-based position in the walk, over the whole note *)
  ; attr_id : string option
    (** The djot [ {#id} ] attribute id. Stored here because the walk unwraps
        the [Ext_attributes] node that carries it; a caret id can be read from
        [block] with {!Parse.Common.caret_id_of_block}. *)
  ; heading_path : string list (** enclosing heading ids, outermost first *)
  ; heading_text : string list (** the same headings as plain text *)
  }

(** A stable name for the block's kind, used by [-kind] and in JSON. A callout
    is named [callout], not [block_quote]. *)
val kind_of_block : Cmarkit.Block.t -> string

(** Every addressable block of [blocks] in document order, containers before
    their contents.

    Blank lines, [Blocks] groupings and [Ext_attributes] wrappers are skipped; a
    wrapper's id is reported on the block it wraps. List items are skipped
    because they have no [Cmarkit.Block.t] of their own, but their contents are
    walked. *)
val walk : Cmarkit.Block.t list -> located_block list

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
