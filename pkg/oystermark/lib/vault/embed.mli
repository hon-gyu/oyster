(** {1 Note embedding: expand embed wikilinks and markdown image links as AST transclusion}

    {@meta[
    ai-disclosure: autonomous
    ]}

    Supported embed sources:
    - {b Cmarkit.Inline.Wikilink embeds}: [!\[\[NOTE\]\]] syntax (parsed as {!Cmarkit.Inline.Wikilink.t}
      with [embed = true]).
    - {b Markdown image embeds}: [!\[alt\](note.md)] syntax. Only expanded when
      the image's resolved target is a note (i.e. a [.md] file). Non-note
      images (e.g. PNG, JPG) are left untouched for the HTML renderer.

    For media embedding (non-note images, audio, video), see [Html].

    This is a post-resolution, pre-render transformation. Each paragraph
    containing a single embed source is replaced by a [Block.Blocks] whose
    meta carries {!embed_meta}.

    Frontmatter is never embedded: {!non_fm_blocks} strips it before extraction.

    - rule: an embed can only be expanded if it's in a container block that
      has no other children or blank children only
    - future TODO: we allow embed Inline.t to violate the above rule. But at
      the moment we have no way to specify whether an embed is Inline.t or
      Block.t

    Depth limiting: embedding is allowed up to [max_depth] levels deep.
    When [embed_depth >= max_depth] the wikilink is replaced with a plain
    fallback link instead; image embeds are left as-is. *)

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

(** Top-level content blocks of a doc, stripping leading frontmatter. *)
val non_fm_blocks : Cmarkit.Doc.t -> Cmarkit.Block.t list

(** Expand all embed wikilinks and image links in a list of resolved docs.
    [max_depth] (default 5) controls how many transclusion levels are allowed
    before falling back to a plain link (wikilinks) or keeping the original
    image (image links). *)
val expand_docs
  :  ?max_depth:int
  -> index:Index.t
  -> (string * Cmarkit.Doc.t) list
  -> (string * Cmarkit.Doc.t) list

(** Reverse transclusion: replace each [Block.Blocks] carrying {!embed_meta}
    with a paragraph containing an embed wikilink [!\[\[source_path#fragment\]\]].

    This restores the original embedding syntax (up to the difference between
    wikilink and commonmark inline link, as noted in {!Spec.reverse_embed}).

    The [.md] extension is stripped from [source_path] to produce idiomatic
    wikilink targets.  Nested embeds are reversed recursively — innermost
    first, since the mapper walks depth-first. *)
val reverse_embed_doc : Cmarkit.Doc.t -> Cmarkit.Doc.t

(** {2 Tests} *)

module For_test : sig
  val parse_blocks : string -> Cmarkit.Block.t list
  val doc_of_blocks : Cmarkit.Block.t list -> Cmarkit.Doc.t
  val print_blocks : Cmarkit.Block.t list -> unit
end
