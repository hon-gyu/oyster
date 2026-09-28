(** A parsed note with sections, anchors, and links.

    {1 Links and anchors}

    A note provides a set of anchors that can be referenced by links. Anchors are
    headings or djot attribute IDs.

    A link comes from markdown link or wikilink syntax.

    - {!Link.t}: the link syntax as it's written.
    - {!Anchor.t}: an anchor as its note defines it, things that are referenceable.
    - {!Anchor.Address.t}: the address of an anchor, the actual key to be matched against.

    A link in one note can be resolved against another note's anchors, producing
    the address if there is a match.

    {1 XPath}

    {!Xml.of_doc} presents the parsed Djot document as XML for [Xpath]
    queries. The XML shape is specified by {!Xml}; XPath supplies the query
    syntax and evaluation semantics.

    *)

module Anchor = Anchor
module Link = Link
module Transclusion = Transclusion
module Xml = Xml

module Private : sig
  module Address_utils = Address_utils
end

(** {1 Note}

    (* ai-disclosure: ai-generated *)

    {!expand} substitutes notes for the embeds of another. A note does not know
    its path. Expanding one happens at a path, since its embeds are resolved
    from there.

    {2 Laws}

    + [(expand ~env ~path n).frontmatter = n.frontmatter].
    + [Transclusion.reverse_embed_doc (expand ~env ~path n).doc] is [n.doc],
      up to how each embed link is written, when no embed reaches the depth
      limit. *)

type t = Parse.t =
  { frontmatter : Yaml.value option
  ; doc : Djot.Doc.t (** Without the frontmatter. *)
  }

(** What an embed resolves to. *)
type source =
  { path : string (** Of the note [note] is taken from. *)
  ; fragment : Link.Ref.fragment option
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
   [Address_utils.find] looks inside transclusions, so an
   embedded heading can shadow one of [n]'s. *)
