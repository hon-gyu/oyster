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
    - {!Query.t} can be constructed from {!Query.type-step} using corresponding constructors or parsed from a string following certain syntax.

    *)

module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion
module Node = Node
module Query = Query

module Private : sig
  module Address_utils = Address_utils
end

(** {1 Note}

    (* ai-disclosure: ai-generated *)

    Notes are closed under two operations: {!select} takes part of a note, and
    {!expand} substitutes notes for the embeds of another. A note does not know
    its path. Expanding one happens at a path, since its embeds are resolved
    from there.

    {2 Laws}

    + [select Query.empty n = n].
    + [select b (select a n)] and [select (a @ b) n] hold the same nodes when [a]
      matches a single root, section, list item, block quote, callout, div,
      keyed node or footnote definition. See {!Query.extract}.
    + [(expand ~env ~path n).frontmatter = n.frontmatter].
    + [Transclusion.reverse_embed_doc (expand ~env ~path n).body] is [n.body],
      up to how each embed link is written, when no embed reaches the depth
      limit. *)

type t =
  { frontmatter : Yaml.value option
  ; body : Cmarkit.Doc.t (** Without the frontmatter block. *)
  }

(** [doc] split at its leading {!Parse.Frontmatter.Frontmatter} block. *)
val of_doc : Cmarkit.Doc.t -> t

(** Inverse of {!of_doc}. *)
val to_doc : t -> Cmarkit.Doc.t

(** The note whose root holds what [query] matches in [n], with [n]'s
    frontmatter. See {!Query.extract}. *)
val select : Query.t -> t -> t

(** What an embed resolves to. *)
type source =
  { path : string (** Of the note [note] is taken from. *)
  ; fragment : Cmarkit.Inline.Wikilink.fragment option
    (** Recorded on the transclusion, see {!Transclusion.embed_meta}. *)
  ; note : t (** The part of that note the embed shows. *)
  }

(** [n], the note at [path], with each embed replaced by a transclusion of what
    [env] resolves it to. [env ~from ref] resolves [ref] written in the note at
    [from]; an embed it gives [None] for is left as written.

    Each transcluded note is expanded in turn, at its own path, unless that
    path is already being expanded: a note embedding itself, directly or through
    others, shows itself unexpanded. From [max_depth] (default 5) transclusions
    deep, a wikilink embed becomes a plain link, see
    {!Transclusion.fallback_block}, and an image embed is left as written. *)
val expand
  :  ?max_depth:int
  -> env:(from:string -> Link.Ref.t -> source option)
  -> path:string
  -> t
  -> t
(* Open: an address of [n] should name the same node in [expand n], but
   [Query.of_address] and [Address_utils.find] look inside transclusions, so an
   embedded heading can shadow one of [n]'s. *)
