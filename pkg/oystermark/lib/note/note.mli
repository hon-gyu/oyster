(** A parsed note with sections, anchors, and links.

    {1 Links and anchors}

    A note provides a set of anchors that can be referenced by links. Anchors can
    headings, Obsidian caret IDs, or djot attribute IDs.

    A link comes from markdown link or wikilink syntax.

    - {!Link.t}: the link syntax as it's written.
    - {!Anchor.t}: an anchor as its note defines it, things that are referenceable.
    - {!Anchor.Address.t}: the address of an anchor, the actual key to be matched against.

    A link in one note can be resolved against another note's anchors, producing
    the address if there is a match.

    {1 Node}

    {!Node.t} in this module differs from Cmarkit.t node in:
    - it has section produced by heading
    - some non-content structures are ignored
    - some variants have different representation for convenience (e.g. list item)
    - ...

    {1 Query by path}

    Apart from referencing a specific anchor by address, {!Query} provides a path-like
    query for referencing nodes in a note.

    - {!Query.t} is composed of a sequence of {!Query.type-step}.
    - {!Query.t} can be constructed using {!Query.step} or parsed from a string following certain syntax.

    *)

module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion
module Node = Node
module Query = Query

module Private : sig
  module Address_utils = Address_utils
end
