(** Read-only queries over parsed blocks: the blocks a link target denotes, used
    by embedding and hover, and a walk over every addressable block of a note. *)

module Address = Address
module Anchor = Anchor

(** The blocks [address] names in [blocks], [ [] ] when it names none.

    - {b Heading}: the heading's section: the heading and the blocks after it
      in the same container, up to (but not including) the next heading of
      equal or lesser level. The heading is found in any container (block
      quote, list item, div, keyed block, footnote); its section ends with that
      container, and a container after the heading belongs to the section
      whole, headings inside it included. A heading carrying a block attribute
      starts the section itself, without the attribute.
    - {b Caret}: one block. When the [^id] ends a paragraph with other content,
      that paragraph. When the [^id] is the entire paragraph, the previous
      non-blank block. When the id is on a {!Cmarkit.Block.Ext_keyed} node,
      having been forwarded from the paragraph or list item the node supplanted,
      the node itself: the label {e and} everything the key claimed as children.
    - {b Attr}: one block. For a block attribute, the block it wraps. For an
      inline attribute, the paragraph or heading containing it. See
      {!page-"feature-attribute-anchors"}.

    Containers (block quotes, list items, [Blocks]) are searched recursively;
    the first match in document order wins. *)
val read : Cmarkit.Block.t list -> Address.t -> Cmarkit.Block.t list

(** The text [blocks] were parsed from: [content] from where the first block
    starts to where the last one ends, trailing whitespace dropped. [None] when
    no block has a location.

    [content] must be the source the blocks were parsed from, with locations.
    The slice is exact bytes, so within a container whose syntax prefixes
    every line (a block quote's [>], a list item's indentation), the lines
    after the first keep that prefix and the first does not. *)
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

(** A block in document order, with everything needed to select it. No field is
    derivable from another. What is derivable from [block], such as its kind or a
    code block's info string, is a function of the block instead. *)
type located_block =
  { block : Cmarkit.Block.t
  ; index : int (** 1-based position in the walk, over the whole note *)
  ; attr_id : string option
    (** a djot [ {#id} ] attribute. Unlike an [ ^id ] (see
        {!Parse.Common.caret_id_of_block}) this is not recoverable from [block]: the walk
        unwraps the [Ext_attributes] node that carries it. *)
  ; heading_path : string list (** enclosing heading ids, outermost first *)
  ; heading_text : string list (** the same headings as plain text *)
  }

(** The block's kind, as a stable name for [-kind] and for JSON. A callout is
    reported as [callout] rather than [block_quote]: it is a quote only in
    representation, and an author selecting one is not asking for quotes. *)
val kind_of_block : Cmarkit.Block.t -> string

(** Every addressable block of [blocks] in document order, containers before
    their contents.

    Blank lines, [Blocks] groupings and [Ext_attributes] wrappers are not blocks
    an author would name, so they are not reported: an attributes wrapper
    forwards its id to the block it wraps, which is what [ {#id} ] denotes.

    A list item is not reported: it has no [Cmarkit.Block.t] of its own, its
    syntax being the marker. Its contents are walked as the blocks they are. *)
val walk : Cmarkit.Block.t list -> located_block list

(** {1 Content} *)

(** What a container holds, once its own syntax is taken off.

    A code block holds {e text}, which the parser has already stripped of its
    fence and indentation, so its content is exact. A block quote holds
    {e blocks}, whose source still carries the [>] marker on every line, so the
    content is a markdown value, written back out by rendering. Rendering
    normalizes (fences, list markers, wrapping), so [Markdown] content is not
    byte-for-byte what the author typed, while [Literal] content is. *)
type content =
  | Literal of string
  | Markdown of Cmarkit.Block.t
  | Not_a_container

(** The contents of the container [located] addresses.

    [Not_a_container] for a paragraph, heading, table or thematic break: they
    hold inlines, rows, or nothing at all, so there is no single value inside to
    ask for. A list is not a container either: its syntax lives in the item
    markers, and its items' blocks are walked in their own right. *)
val content_of_located_block : located_block -> content

(** The contents of [located] as a string, given the document's label
    definitions (a rendered container may hold reference links).

    [Error kind] names the kind that has no contents to give. *)
val content_string : defs:Cmarkit.Label.defs -> located_block -> (string, string) Result.t
