# Wikilink Syntax Specification

This document defines the syntax of Obsidian wikilinks.

## Grammar

```ebnf
wikilink     = "[[" , content , "]]" ;
embed        = "![[" , content , "]]" ;

content      = [ target ] , [ "#" , heading_path ] , [ "|" , display ]
             | [ target ] , "#^" , block_id , [ "|" , display ] ;

target       = path_segment , { "/" , path_segment } ;
path_segment = { any_char - ( "#" | "^" | "|" | "[" | "]" | "/" ) }+ ;

heading_path = heading , { "#" , heading } ;
heading      = { any_char - ( "#" | "|" | "[" | "]" ) }+ ;

block_id     = ( letter | digit ) , { letter | digit | "-" } ;

display      = { any_char - ( "[" | "]" ) }+ ;
```

- Target: file path without heading/block/display parts
- Heading path: one or more headings separated by #
- Block identifier: alphanumeric and hyphens
- Display text: everything after first pipe

## Tokenization Rules

### Wikilink Start

- `[[` opens a wikilink
- `![[` opens an embed (transclusion)
- Must not be escaped (preceded by `\`)

### Wikilink End

- `]]` closes the wikilink
- First `]]` encountered closes (no nesting)

### Special Characters

| Char | Role                   | Notes                                  |
| ---- | ---------------------- | -------------------------------------- |
| `#`  | Heading separator      | Multiple consecutive `#` are collapsed |
| `^`  | Block reference prefix | Only valid after `#` or at start       |
| `\|` | Display text separator | First `\|` wins; rest is literal       |
| `/`  | Path separator         | For directory paths                    |

### Hash Normalization

Multiple consecutive `#` are treated as a single separator:

- `[[##A###B]]` → headings: `["A", "B"]`
- `[[A##B]]` → headings: `["A", "B"]`
- `[[##]]` → empty heading (points to note)

### Heading vs Block Reference

A wikilink contains either heading references OR a block reference, never both.

Detection rule: After the first `#`, if the content:

1. Starts with `^`, AND
2. The remainder is a valid block identifier (alphanumeric and hyphens only)

Then it's a block reference. Otherwise, the entire fragment is parsed as a heading path.

Examples:

| Input                  | Fragment after first `#` | Interpretation                                               |
| ---------------------- | ------------------------ | ------------------------------------------------------------ |
| `[[Note#^blockid]]`    | `^blockid`               | Block reference: `blockid`                                   |
| `[[Note#H1]]`          | `H1`                     | Heading: `["H1"]`                                            |
| `[[Note#H1#H2]]`       | `H1#H2`                  | Headings: `["H1", "H2"]`                                     |
| `[[Note#H1#^blockid]]` | `H1#^blockid`            | Headings: `["H1", "^blockid"]` (literal)                     |
| `[[Note#^blockid#H1]]` | `^blockid#H1`            | Headings: `["^blockid", "H1"]` (invalid block ID due to `#`) |
| `[[Note#^block-id]]`   | `^block-id`              | Block reference: `block-id`                                  |
| `[[Note#^block_id]]`   | `^block_id`              | Headings: `["^block_id"]` (underscore invalid in block ID)   |

When the fragment doesn't match a valid block reference pattern, `^` is treated as a literal character in heading names.

NOTE: this needs to be double-checked.

## Syntactic Forms

### Basic Forms

| Form             | Interpretation                    |
| ---------------- | --------------------------------- |
| `[[Note]]`       | Link to note                      |
| `[[Note.md]]`    | Link to note (explicit extension) |
| `[[dir/Note]]`   | Link to note in directory         |
| `[[Note\|text]]` | Link with display text            |

### Heading References

| Form               | Interpretation               |
| ------------------ | ---------------------------- |
| `[[#Heading]]`     | Heading in current note      |
| `[[Note#Heading]]` | Heading in another note      |
| `[[Note#H1#H2]]`   | Nested heading (H2 under H1) |
| `[[#H1#H2\|text]]` | Nested heading with display  |

### Block References

| Form                  | Interpretation          |
| --------------------- | ----------------------- |
| `[[#^blockid]]`       | Block in current note   |
| `[[Note#^blockid]]`   | Block in another note   |
| `[[#^blockid\|text]]` | Block with display text |

### Embeds

| Form                | Interpretation       |
| ------------------- | -------------------- |
| `![[Note]]`         | Embed entire note    |
| `![[Note#Heading]]` | Embed from heading   |
| `![[image.png]]`    | Embed image          |
| `![[Note#^block]]`  | Embed specific block |
