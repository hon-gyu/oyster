(** Read-only queries over parsed blocks: the blocks a link target denotes, used
    by embedding and hover, and a walk over every addressable block of a note. *)

(** Collect the section starting at the heading whose identifier (see
    {!Parse.Common.heading_id}) is [heading_id], up to (but not including) the
    next heading of equal or lesser level.  Returns [ [] ] when the heading is not
    found. *)
val get_heading_section : Cmarkit.Block.t list -> string -> Cmarkit.Block.t list

(** Extract the block that {!Cmarkit.Block.Block_id.t} points to.

    Three cases:
    - {b Inline}: the [^id] appears at the end of a paragraph with other content.
      The paragraph itself is the target.
    - {b Standalone}: the [^id] is the entire paragraph.
      It references the previous non-blank block.
    - {b Keyed}: the id is on a {!Cmarkit.Block.Ext_keyed} node, having been
      forwarded from the paragraph or list item the node supplanted. The node
      itself is the target, so the reference denotes the label {e and} everything
      the key claimed as children. It anchors a whole subtree. *)
val get_block_by_caret_id : Cmarkit.Block.t list -> string -> Cmarkit.Block.t option

(** Extract the block carrying an explicit djot attribute id ([{#id}]).

    Two cases (see {!page-"feature-attribute-anchors"}):
    - {b Block attribute}: a [Block.Ext_attributes] whose merged attribute has
      the id.  The {e wrapped} block is returned.
    - {b Inline attribute}: an [Inline.Ext_attributes] carrying the id somewhere
      in a paragraph's or heading's inline content.  The containing block is
      returned.

    Container blocks (block quotes, list items, [Blocks]) are searched
    recursively; the first match in document order wins. *)
val get_block_by_attr_id : Cmarkit.Block.t list -> string -> Cmarkit.Block.t option

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
