(* In CommonMark the two levels (block and inline) are strictly segregated

Block structure is parsed first, as a tree of block nodes. Container blocks (blockquotes, list items) can contain other blocks recursively. Leaf blocks (paragraphs, headings, code blocks, etc.) cannot contain blocks.
Inline content lives only inside leaf blocks. Once the block structure is fully resolved, the raw text of each leaf block is parsed for inline constructs (emphasis, links, code spans, etc.).

==> So, adding new inline extension should not affect block structure in any way.

*)
