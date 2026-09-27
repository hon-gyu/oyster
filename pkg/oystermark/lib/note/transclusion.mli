(** {1 Transclusion: note embedding operations}

    an embedding link (`![[noteb#title_b]]`) --embed--> a div with the note's content with metadata as block attributes

    - For media embedding (non-note images, audio, video), see [Html].

    - rule: an embed can only be expanded if it's in a container block that
    has no other children. Not enforced:
    {!is_expandable_embed_paragraph} implements the rule, but [Vault.Embed]
    recognizes an embed with {!embed_source_of_inlines} alone, so an embed
    paragraph among other blocks is expanded too.

    - future TODO: we allow embed Inline.t to violate the above rule. But at
    the moment we have no way to specify whether an embed is Inline.t or
    Block.t *)

type block = Djot.Block.t Djot.node

(** Embed data, carried as the attributes of the div {!transclude} wraps the
    content in. *)
type embed_meta =
  { depth : int
    (** Transclusion depth: 1 for a direct embed, 2 for an embed within an
      embed, etc. *)
  ; source_path : string
    (** Vault-relative path of the note whose blocks were transcluded. *)
  ; fragment : Link.Ref.fragment option
    (** The heading fragment, if the embed targeted a section rather than the
        full note. *)
  }

(** The class of the div {!transclude} wraps transcluded content in. *)
val embed_class : string

(** The metadata of the transclusion [block], or [None] if [block] is not a div
    of {!embed_class} with a [source] attribute. A missing [depth] means [1]. *)
val embed_meta_of_block : block -> embed_meta option

(** The inlines that can trigger block-level transclusion. *)
type embed_source =
  | Wikilink_embed of string (** [ ![[NOTE]] ], its target as written. *)
  | Image_embed of Link.Ref.t
  (** [ ![alt](note.md) ]. Transcluded only if the target resolves to a note. *)

(** The embed source that [inlines], in [doc], consist of, if they are exactly
    one. *)
val embed_source_of_inlines
  :  Djot.Doc.t
  -> Djot.Inline.t Djot.node list
  -> embed_source option

(** The embed source of [block] if [block] is a paragraph with exactly one embed
    source and [siblings] holds no other block. *)
val is_expandable_embed_paragraph
  :  Djot.Doc.t
  -> block
  -> siblings:block list
  -> embed_source option

(** A paragraph with a plain, non-embed wikilink to [target]. Used when an embed
    reaches the depth limit. *)
val fallback_block : string -> block

(** The fragment stored in {!embed_meta} for a transclusion of [anchor]: a
    heading's text, or an attribute's id. *)
val fragment : Anchor.definition -> Link.Ref.fragment

(** [blocks] wrapped in a div of {!embed_class} carrying {!embed_meta} as
    attributes:

    {v
    {source="notes/a.md" fragment=Intro depth=1}
    ::: embed
    ...blocks...
    :::
    v} *)
val transclude
  :  depth:int
  -> source_path:string
  -> fragment:Link.Ref.fragment option
  -> block list
  -> block

(** Reverse transclusion: replace each transclusion div (see
    {!embed_meta_of_block}) with a paragraph containing an embed wikilink
    [! [[source_path#fragment]] ].

    This restores the original embedding syntax (up to the difference between
    wikilink and markdown inline link).

    The [.md] extension is removed from [source_path]. *)
val reverse_embed_doc : Djot.Doc.t -> Djot.Doc.t
