(** Reading blocks of a note: the blocks an address names and their source text.
    To select blocks by kind, position or enclosing heading, see
    {!Query}. *)

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
