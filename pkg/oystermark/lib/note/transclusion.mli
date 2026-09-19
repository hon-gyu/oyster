(** {1 Transclusion: note-local embedding operations}

    {@meta[
    ai-disclosure: ai-generated
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

(** Transclusion data

    Carried two ways:
    - on the [Cmarkit.Meta.t] of the div {!transclude} wraps the content in
    - as the djot attribute written on that div. *)
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

(** The class of the div {!transclude} wraps transcluded content in. *)
val embed_class : string

(** The metadata of the transclusion [block], or [None] if [block] is not
    a div of {!embed_class}, looking through any number of attribute wrappers.

    The metadata comes from the div's {!embed_meta_key} meta when present.
    A document parsed back from text carries no meta, so the metadata is then
    read from the div's attribute, where a missing [depth] means [1]. *)
val embed_meta_of_block : Cmarkit.Block.t -> embed_meta option

(** Top-level content blocks of a doc, without leading frontmatter. If the doc's
    top block is a transclusion, it is returned as a single block so the
    transclusion boundary is kept. *)
val non_fm_blocks : Cmarkit.Doc.t -> Cmarkit.Block.t list

(** The inlines that can trigger block-level transclusion. *)
type embed_source =
  | Wikilink_embed of Cmarkit.Inline.Wikilink.t * Cmarkit.Meta.t
  (** [ ![[NOTE]] ], with its meta for {!fallback_block}. *)
  | Image_embed of Link.Ref.t
  (** [ ![alt](note.md) ]. Transcluded only if the target resolves to a note. *)

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

(** [blocks] wrapped in a div of {!embed_class} carrying {!embed_meta}, both on
    the div's meta and as the djot attribute written above its opening fence:

    {v
    {source="notes/a.md" fragment=Intro depth=1}
    ::: embed
    ...blocks...
    :::
    v} *)
val transclude
  :  depth:int
  -> source_path:string
  -> fragment:Cmarkit.Inline.Wikilink.fragment option
  -> Cmarkit.Block.t list
  -> Cmarkit.Block.t

(** Reverse transclusion: replace each transclusion div (see
    {!embed_meta_of_block}) with a paragraph containing an embed wikilink
    [! [[source_path#fragment]] ].

    This restores the original embedding syntax (up to the difference between
    wikilink and commonmark inline link).

    The [.md] extension is removed from [source_path]. Nested embeds are reversed
    innermost first. *)
val reverse_embed_doc : Cmarkit.Doc.t -> Cmarkit.Doc.t

(** {2 Tests} *)

module For_test : sig
  val parse_blocks : string -> Cmarkit.Block.t list
  val doc_of_blocks : Cmarkit.Block.t list -> Cmarkit.Doc.t
  val print_blocks : Cmarkit.Block.t list -> unit
end
