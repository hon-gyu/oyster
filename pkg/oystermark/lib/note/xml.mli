(** An XML view of a parsed Djot document for XPath 1.0 queries.

    Element names describe Djot constructors in lower-case snake case: [Para]
    becomes [paragraph], [Str] becomes [text], and the list variants share
    [list]. The root is [doc]. Sections contain their heading as their first
    child. Lists have [kind] ([bullet], [ordered], or [task]) and [spacing]
    attributes, containing [item] elements. Ordered lists also carry [start],
    [style], and [delimiter]; task items carry [task]. Definition lists contain [item]
    elements with [term] and [definition] children. Tables contain [row] and
    [cell] elements. Inlines are elements too: [text], [emph], [strong],
    [link], [wikilink], and the other {!Djot.Inline.t} constructors.

    Keyed blocks have a [key] attribute, a [label] child containing the label
    inlines, then their value block. Callouts have [type] and optional [fold]
    attributes, a [title] child, then body blocks. Code blocks have [lang]
    and text content. Footnotes and reference definitions are under [footnotes]
    and [references] at the end of [doc], once each, wherever they were
    written: a definition nested in a footnote or another block is not also a
    child of that block.

    For example, [# Top] followed by a Python fence produces a [doc] with a
    [section] containing [heading] and [code_block] children. The XPath
    expression [//section\[heading="Top"\]/code_block\[@lang="python"\]]
    selects that block. Parsed frontmatter is outside this Djot view.

    Djot's [id] and classes become [id] and [class] attributes. Other authored
    attributes become [attribute] children with [name] and [value] attributes,
    so they cannot collide with structural attributes. When locations are
    available, elements derived from Djot nodes have [start-byte], [end-byte],
    [start-line], and [end-line] attributes. Bytes are zero-based and inclusive;
    lines are one-based. This view represents the parse tree, not rendered HTML. *)

val of_doc : Djot.Doc.t -> Simple_xml.element
