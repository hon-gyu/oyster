(** {1 Note embedding: expand embed wikilinks and markdown image links as AST transclusion}

    {@meta[
    ai-disclosure: autonomous
    ]}

    A post-resolution, pre-render transformation over a whole vault. Each
    paragraph holding a single embed source (see {!Transclusion}) whose link
    resolves to a note, or to an anchor in one, is replaced by a
    {!Transclusion.transclude} of the blocks that target denotes (see
    {!Extract.read}). Frontmatter is never embedded.

    A transcluded note is expanded first, so the embeds inside it are
    transcluded too. A self-reference reads the note as it stands, without
    expanding it again.

    An embed whose link does not resolve, or resolves to an asset, is left
    as-is.

    Depth limiting: embedding is allowed up to [max_depth] levels deep.
    When [embed_depth >= max_depth] the wikilink is replaced with a plain
    fallback link ({!Transclusion.fallback_block}) instead; image embeds are
    left as-is. *)

(** Expand all embed wikilinks and image links in a list of resolved docs.
    [max_depth] (default 5) controls how many transclusion levels are allowed
    before falling back to a plain link (wikilinks) or keeping the original
    image (image links). *)
val expand_docs
  :  ?max_depth:int
  -> index:Index.t
  -> (string * Cmarkit.Doc.t) list
  -> (string * Cmarkit.Doc.t) list
