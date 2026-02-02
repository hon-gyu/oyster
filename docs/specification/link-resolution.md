# Wikilink Resolution Semantics

This document defines how links in Obsidian (wikilinks and markdown links) resolve to their targets.

## Overview

Resolution happens in stages:

1. **Parse** the wikilink into components (target, headings, block, display)
2. **Resolve file** - find matching note or asset
3. **Resolve in-note target** - find heading or block within the file
4. **Fallback** - if in-note target not found, fall back to file

## File Resolution

### Extension Handling

```
Input target → Normalized for matching
─────────────────────────────────────────
"Note"        → "Note.md"         (add .md if no extension)
"Note.md"     → "Note.md"         (keep as-is)
"image.png"   → "image.png"       (keep as-is, matches asset)
"Figure.jpg.md" → "Figure.jpg.md" (explicit .md, matches note)
```

Rule: If target has no `.`, append `.md`. Otherwise, match literally.

### Resolution Order (Precedence)

1. Exact match: path matches exactly
2. Subsequence match: path components form ancestor-descendant relationship

```
Vault structure:
├── indir_same_name.md       (root level)
├── dir/
│   ├── indir_same_name.md   (in subdirectory)
│   ├── indir2.md
│   └── inner_dir/
│       └── note_in_inner_dir.md
```

| Input                                 | Matches                              | Reason                      |
| ------------------------------------- | ------------------------------------ | --------------------------- |
| `[[indir_same_name]]`                 | `indir_same_name.md` (root)          | Exact match at root         |
| `[[dir/indir_same_name]]`             | `dir/indir_same_name.md`             | Exact match with path       |
| `[[indir2]]`                          | `dir/indir2.md`                      | Subsequence (no root match) |
| `[[dir/inner_dir/note_in_inner_dir]]` | `dir/inner_dir/note_in_inner_dir.md` | Exact                       |
| `[[inner_dir/note_in_inner_dir]]`     | `dir/inner_dir/note_in_inner_dir.md` | Subsequence                 |
| `[[dir/note_in_inner_dir]]`           | `dir/inner_dir/note_in_inner_dir.md` | Subsequence                 |
| `[[random/note_in_inner_dir]]`        | X unresolved                         | No `random` ancestor        |

### Subsequence Matching Algorithm

Path components must appear in order (ancestor-descendant relationship):

```
match_subsequence(haystack, needle):
    haystack_idx = 0
    for each component in needle:
        found = false
        while haystack_idx < len(haystack):
            if haystack[haystack_idx] == component:
                found = true
                haystack_idx++
                break
            haystack_idx++
        if not found:
            return false
    return true
```

Examples:

- `["dir", "note"]` matches `["dir", "inner_dir", "note.md"]` ✓
- `["inner_dir", "note"]` matches `["dir", "inner_dir", "note.md"]` ✓
- `["random", "note"]` does NOT match `["dir", "inner_dir", "note.md"]` ✗

### Asset vs Note Priority

When both an asset and a note could match:

| Input               | Asset exists         | Note exists     | Matches      |
| ------------------- | -------------------- | --------------- | ------------ |
| `[[Figure.jpg]]`    | `Figure.jpg`         | `Figure.jpg.md` | Asset        |
| `[[Figure.jpg.md]]` | `Figure.jpg`         | `Figure.jpg.md` | Note         |
| `[[Note 1]]`        | `Note 1` (file)      | `Note 1.md`     | Note         |
| `[[Something]]`     | `Something` (no ext) | -               | X unresolved |

Rule:

- Targets with non-`.md` extension match assets first
- Targets without extension get `.md` appended → match notes
- Files without extension are never matched (no way to reference them)

### Empty Target

- `[[]]` → current note
- `[[#Heading]]` → current note's heading
- `[[#^block]]` → current note's block

## Heading Resolution

### Nested Heading Semantics

Headings form a tree based on their levels:

```markdown
## Chapter 1 (H2)

### Section 1.1 (H3)

#### Detail 1.1.1 (H4)

### Section 1.2 (H3)

## Chapter 2 (H2)

### Section 2.1 (H3)
```

Tree structure:

```
H2: Chapter 1
├── H3: Section 1.1
│   └── H4: Detail 1.1.1
└── H3: Section 1.2
H2: Chapter 2
└── H3: Section 2.1
```

### Resolution Algorithm

For `[[Note#H1#H2#H3]]`, find headings where:

1. Each heading text matches in sequence
2. Each subsequent heading has a strictly greater level (deeper nesting)

```
resolve_nested_headings(doc_headings, query):
    // Uses backtracking to handle duplicate heading names
    for each possible subsequence match:
        if all matched headings form valid hierarchy:
            return last matched heading
    return None (fallback to note)
```

Valid hierarchy: `level[i+1] > level[i]` for all consecutive matches.

### Examples

Document headings (in order):

```
H2: L2
H3: L3
H4: L4
H3: Another L3
```

| Query                   | Matches     | Reason                           |
| ----------------------- | ----------- | -------------------------------- |
| `[[#L2]]`               | H2: L2      | Direct match                     |
| `[[#L3]]`               | H3: L3      | Direct match                     |
| `[[#L4]]`               | H4: L4      | Direct match                     |
| `[[#L2#L3]]`            | H3: L3      | L2→L3 valid (H2→H3)              |
| `[[#L2#L4]]`            | H4: L4      | L2→L4 valid (H2→H4)              |
| `[[#L2#L3#L4]]`         | H4: L4      | L2→L3→L4 valid (H2→H3→H4)        |
| `[[#L2#L4#L3]]`         | ❌ fallback | Invalid: L4→L3 (H4→H3) decreases |
| `[[#L2#L4#Another L3]]` | ❌ fallback | Invalid: L4→Another L3 decreases |

### Hash Normalization

Multiple `#` characters are collapsed before matching:

| Input                | Parsed headings      |
| -------------------- | -------------------- |
| `[[#L2]]`            | `["L2"]`             |
| `[[###L2#L4]]`       | `["L2", "L4"]`       |
| `[[##L2######L4]]`   | `["L2", "L4"]`       |
| `[[##L2#####L4#L3]]` | `["L2", "L4", "L3"]` |

### Invalid Heading Fallback

If heading resolution fails, fall back to the note itself:

- `[[Note1#random]]` → Note1 (if `random` heading doesn't exist)
- `[[Note1#L2#L4#L3]]` → Note1 (invalid hierarchy)
- `[[Note1#]]` → Note1 (empty heading)
- `[[Note1##]]` → Note1 (empty heading after normalization)
- `[[#]]` → current note

## Block Resolution

### Block Identifier Locations

Block identifiers (`^id`) can appear in markdown source:

#### 1. Inline at end of paragraph

```markdown
This is paragraph text ^paragraphid
```

References the paragraph itself.

#### 2. Inline at end of list item

```markdown
- List item content ^itemid
  - Nested item ^nestedid
```

References the list item.

#### 3. Separate line after block

```markdown
| Header |
| ------ |
| Cell   |

^tableid
```

References the previous block (table, list, blockquote, callout).

### Block Type Summary

| Type              | Example            | Reference Target    |
| ----------------- | ------------------ | ------------------- |
| `InlineParagraph` | `text ^id`         | The paragraph       |
| `InlineListItem`  | `- item ^id`       | The list item       |
| `Paragraph`       | `text\n\n^id`      | Previous paragraph  |
| `List`            | `- a\n- b\n\n^id`  | Previous list       |
| `Table`           | `\| ... \|\n\n^id` | Previous table      |
| `BlockQuote`      | `> quote\n\n^id`   | Previous blockquote |
| `Callout`         | `> [!info]\n\n^id` | Previous callout    |

### Block Resolution Rules

1. Parse `#^blockid` from the wikilink
2. Search for block with matching identifier in target note
3. If found, link resolves to that block
4. If not found, fall back to note

### Edge Case: Conflicting Identifiers

If a block has both inline and separate-line identifiers:

```markdown
- List item
  - Inner item ^innerid

^listid
```

The separate-line `^listid` creates a **new** block reference for the entire list.
The inline `^innerid` still references just the inner item.

However, if the outer list gets a block ID, inner references may become invalid:

```markdown
- a nested list ^firstline
  - item ^inneritem

^fulllist

- [[#^firstline]] → points to first item (inline wins)
- [[#^inneritem]] → points to inner item
- [[#^fulllist]] → points to full list (overrides)
```

When `^fulllist` references the list, `^inneritem` may become unreachable depending on implementation.

### Edge Case: Multiple Identifiers on Separate Lines

```markdown
| Table |
| ----- |
| Cell  |

^id1

^id2
```

Only the **first** identifier (`^id1`) references the table.
`^id2` becomes its own paragraph identifier (references `^id1` paragraph).

## Display Text

### Parsing

First `|` separates target from display; remaining content is literal:

| Input             | Target Part | Display  |
| ----------------- | ----------- | -------- |
| `[[Note\|text]]`  | `Note`      | `text`   |
| `[[Note\|a\|b]]`  | `Note`      | `a\|b`   |
| `[[#H1\|custom]]` | `#H1`       | `custom` |
| `[[#H1 \| #H2]]`  | `#H1`       | `#H2`    |

### Whitespace Trimming

- Target: leading/trailing whitespace trimmed
- Display: leading/trailing whitespace trimmed
- `[[Note 2 \| text ]]` → target: `Note 2`, display: `text`

### Default Display Text (Obsidian)

If no `|` is present, display text equals the raw content by default:

- `[[Note]]` → display: `Note`
- `[[Note#Heading]]` → display: `Note#Heading`
- `[[#^blockid]]` → display: `#^blockid`

The default display is implementation-dependent.

## Embeds

Embeds (`![[...]]`) follow the same resolution rules as wikilinks.

The embed content replaces the embed marker:

- `![[Note]]` → entire note content
- `![[Note#Heading]]` → content from heading to next same-level heading
- `![[Note#^block]]` → just that block
- `![[image.png]]` → rendered image

## Unresolved Links

A link is unresolved when:

1. Target file doesn't exist
2. Target path doesn't match any file (even with subsequence)
3. (Heading/block failure → falls back to note, not unresolved)

Unresolved link examples:

- `[[Non-existing note]]`
- `[[random/note]]` (no `random` directory)
- `[[dir/]]` (trailing slash, no file)
- `[[Something]]` (file exists but has no extension)
