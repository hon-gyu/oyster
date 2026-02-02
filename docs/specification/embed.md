# Embed Specification

Embeds (transclusions) allow including content from one note into another. An embed uses the same target syntax as wikilinks but is prefixed with `!`.

Related: [[wikilink]], [[link-resolution]]

## Syntax

```ebnf
embed = "![[" , content , "]]" ;
```

The `content` follows the same grammar as wikilinks. See [[wikilink]] for details.

## Embed Types

### Note Embed

Embeds the entire content of another note.

```markdown
![[Note]]
```

### Heading Embed

Embeds content starting from a heading until the next heading of the same or higher level.

```markdown
![[Note#Heading]]
![[Note#H1#H2]]
```

### Block Embed

Embeds a specific block (paragraph, list, table, callout, etc.).

```markdown
![[Note#^blockid]]
```

### Asset Embed

Embeds media files (images, videos, audio, PDFs).

```markdown
![[image.png]]
![[video.mp4]]
![[document.pdf]]
```

#### Image Sizing

Images can be resized using the pipe syntax:

| Syntax | Effect |
|--------|--------|
| `![[image.png\|100]]` | Width 100px, height scales proportionally |
| `![[image.png\|100x150]]` | Width 100px, height 150px |

## Recursion and Circular References

### Maximum Embed Depth

Embeds can be nested (an embedded note may itself contain embeds). To prevent infinite loops from circular references, there is a maximum embed depth.

**Maximum depth: 5**

When an embed would exceed this depth, it is rendered as a regular link instead of transcluding the content.

### Circular Reference Example

Consider two notes that embed each other:

**Note A:**
```markdown
Some content in A
![[Note B]]
```

**Note B:**
```markdown
Some content in B
![[Note A]]
```

When rendering Note A:
- Depth 0: Note A content
- Depth 1: Note B content embedded
- Depth 2: Note A content embedded (from B's embed)
- Depth 3: Note B content embedded (from A's embed)
- Depth 4: Note A content embedded
- Depth 5: Note B would be embedded, but max depth reached → rendered as link

### Self-Reference

A note embedding itself follows the same rules:

```markdown
# My Note
![[My Note]]
```

This would recurse up to depth 5, then render as a link.

## Embed Resolution

Embeds follow the same resolution rules as wikilinks:

1. Target file is resolved using [[link-resolution]] rules
2. If heading/block is specified, that portion is extracted
3. If target cannot be resolved, the embed is rendered as unresolved

## Content Extraction

### Note Content

The entire note content is included, excluding frontmatter.

### Heading Content

Content from the heading until:
- The next heading of the same level or higher
- End of document

```markdown
## Section A      ← Start of embed
Content here
### Subsection    ← Included
More content
## Section B      ← End of embed (same level)
```

### Block Content

Only the specific block is included:
- For `InlineParagraph` / `InlineListItem`: the containing element
- For separate-line block IDs: the preceding block

## Edge Cases

### Empty Embed Target

`![[]]` or `![[#]]` embeds the current note (self-reference).

### Unresolved Embed

If the target cannot be resolved, display as unresolved embed (similar to broken links).

### Embed in Embed

Nested embeds are processed recursively until max depth is reached.

```markdown
![[A]]           ← A contains ![[B]]
                 ← B contains ![[C]]
                 ← All resolved up to depth 5
```
