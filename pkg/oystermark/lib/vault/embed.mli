(** {1 Note embedding: expand embed wikilinks and markdown image links as AST transclusion}

    {@meta[
    ai-disclosure: autonomous
    ]}

    Runs over a whole vault, after link resolution and before rendering. A
    paragraph with a single embed source (see {!Note.Transclusion}) is replaced
    by a transclusion ({!Note.Transclusion.transclude}) of the blocks its link
    target names ({!Note.read}), when the link resolves to a note or to an
    anchor in a note. Frontmatter is never embedded.

    The target note is expanded before it is transcluded, so nested embeds are
    expanded too. A self-reference uses the note unexpanded.

    An embed whose link does not resolve, or resolves to an asset, is left
    unchanged.

    Depth limiting: embedding is allowed up to [max_depth] levels deep.
    When [embed_depth >= max_depth] the wikilink is replaced with a plain
    fallback link ({!Note.Transclusion.fallback_block}) instead; image embeds are
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
