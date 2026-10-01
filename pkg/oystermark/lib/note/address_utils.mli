(** The blocks of a note an address names, and their source text. *)

(** The blocks of [doc] named by [address], or [ [] ] if there are none.

    - {b Heading}: the section with that id, which holds the heading and the
      blocks under it. A heading that opens no section, inside a block quote,
      div, callout or list item, is found alone.
    - {b Attr}: one block. For an attribute on a block, that block. For an
      attribute on inlines, the block that holds them outside its child blocks:
      a paragraph or heading, a table for a caption or cell, a callout for its
      title, a keyed block for its label, a definition list for a term. See
      {!page-"feature-attribute-anchors"}.

    The blocks are searched, then the footnotes; the first match in document
    order wins. *)
val find : Djot.Doc.t -> Anchor.Address.t -> Djot.Block.t Djot.node list

(** The source text of [blocks]: [content] from the start of the first block,
    its attribute specs included, to the end of the last, with trailing
    whitespace removed. [None] if no block has a location.

    [content] must be the text [doc] was parsed from, with locations, and
    [blocks] nodes of [doc]. The slice is taken byte for byte, so inside a
    container that prefixes each line (a block quote's [>], a list item's
    indentation) every line but the first keeps that prefix. *)
val source_text : Djot.Doc.t -> string -> Djot.Block.t Djot.node list -> string option
