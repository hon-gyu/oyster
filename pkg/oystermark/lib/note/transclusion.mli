(** {1 Transclusion: note-local embedding operations}

    {@meta[
    ai-disclosure: autonomous
    ]}

    Recognizes embeds in a note, wraps blocks as a transclusion, and turns a
    transclusion back into embed syntax. Link resolution and reading other notes
    happen in [Vault.Embed].

    Supported embed sources:
    - {b Cmarkit.Inline.Wikilink embeds}: [!\[\[NOTE\]\]] syntax (parsed as {!Cmarkit.Inline.Wikilink.t}
      with [embed = true]).
    - {b Markdown image embeds}: [!\[alt\](note.md)] syntax. Only expanded when
      the image's resolved target is a note (i.e. a [.md] file). Non-note
      images (e.g. PNG, JPG) are left untouched for the HTML renderer.

    For media embedding (non-note images, audio, video), see [Html].

    - rule: an embed can only be expanded if it's in a container block that
      has no other children or blank children only. Not enforced:
      {!is_expandable_embed_paragraph} implements the rule, but [Vault.Embed]
      recognizes an embed with {!embed_source_of_inline} alone, so an embed
      paragraph among other blocks is expanded too.
    - future TODO: we allow embed Inline.t to violate the above rule. But at
      the moment we have no way to specify whether an embed is Inline.t or
      Block.t *)

(* CR: can we encode embed meta in block attribute? and let the downstream *)

(** Metadata attached to the [Cmarkit.Block.Blocks] node that wraps
    transcluded content. Consumers (e.g. the HTML renderer) can use this to
    style embedded blocks differently, and {!reverse_embed_doc} uses it to
    reconstruct the original embed syntax. *)
type embed_meta =
  { depth : int
    (** Transclusion depth: 1 for a direct embed, 2 for an embed within an
      embed, etc. *)
  ; source_path : string
    (** Vault-relative path of the note whose blocks were transcluded. *)
  ; fragment : Cmarkit.Inline.Wikilink.fragment option
    (** The heading or block-ref fragment, if the embed targeted a sub-section
        rather than the full note. *)
  }

val embed_meta_key : embed_meta Cmarkit.Meta.key

(** Top-level content blocks of a doc, without leading frontmatter. If the doc's
    top block is a transclusion, it is returned as a single block so the
    transclusion boundary is kept. *)
val non_fm_blocks : Cmarkit.Doc.t -> Cmarkit.Block.t list

(** The inlines that can trigger block-level transclusion. *)
type embed_source =
  | Wikilink_embed of Cmarkit.Inline.Wikilink.t * Cmarkit.Meta.t
  (** [!\[\[NOTE\]\]], with its meta for {!fallback_block}. *)
  | Image_embed of Link.Ref.t
  (** [!\[alt\](note.md)]. Transcluded only if the target resolves to a note. *)

(** The embed source that [inline] consists of, if it is exactly one. A
    one-element [Inlines] wrapper is ignored. *)
val embed_source_of_inline : Cmarkit.Inline.t -> embed_source option

(** The embed source of [block] if [block] is a paragraph with exactly one embed
    source and every other block in [siblings] is a blank line. *)
val is_expandable_embed_paragraph
  :  Cmarkit.Block.t
  -> siblings:Cmarkit.Block.t list
  -> embed_source option

(** A paragraph with [wl] as a plain, non-embed link. Used when an embed reaches
    the depth limit. *)
val fallback_block : Cmarkit.Inline.Wikilink.t -> Cmarkit.Meta.t -> Cmarkit.Block.t

(** The fragment stored in {!embed_meta} for a transclusion of [anchor]. [None]
    for an attribute anchor. *)
val fragment : Anchor.value -> Cmarkit.Inline.Wikilink.fragment option

(** [blocks] wrapped in a [Cmarkit.Block.Blocks] node that carries {!embed_meta}. *)
val transclude
  :  depth:int
  -> source_path:string
  -> fragment:Cmarkit.Inline.Wikilink.fragment option
  -> Cmarkit.Block.t list
  -> Cmarkit.Block.t

(** Reverse transclusion: replace each [Block.Blocks] carrying {!embed_meta}
    with a paragraph containing an embed wikilink [!\[\[source_path#fragment\]\]].

    This restores the original embedding syntax (up to the difference between
    wikilink and commonmark inline link, as noted in {!Spec.reverse_embed}).

    The [.md] extension is removed from [source_path]. Nested embeds are reversed
    innermost first. *)
val reverse_embed_doc : Cmarkit.Doc.t -> Cmarkit.Doc.t

(** {2 Tests} *)

module For_test : sig
  val parse_blocks : string -> Cmarkit.Block.t list
  val doc_of_blocks : Cmarkit.Block.t list -> Cmarkit.Doc.t
  val print_blocks : Cmarkit.Block.t list -> unit
end
