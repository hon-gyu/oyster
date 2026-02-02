# Wikilink Syntax Specification

This document defines the syntax of Obsidian wikilinks.
For resolution semantics, see [[wikilink-semantics]].

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

| Char | Role | Notes |
|------|------|-------|
| `#`  | Heading separator | Multiple consecutive `#` are collapsed |
| `^`  | Block reference prefix | Only valid after `#` or at start |
| `\|` | Display text separator | First `\|` wins; rest is literal |
| `/`  | Path separator | For directory paths |

### Hash Normalization

Multiple consecutive `#` are treated as a single separator:
- `[[##A###B]]` → headings: `["A", "B"]`
- `[[A##B]]` → headings: `["A", "B"]`
- `[[##]]` → empty heading (points to note)

## Syntactic Forms

### Basic Forms

| Form | Interpretation |
|------|----------------|
| `[[Note]]` | Link to note |
| `[[Note.md]]` | Link to note (explicit extension) |
| `[[dir/Note]]` | Link to note in directory |
| `[[Note\|text]]` | Link with display text |

### Heading References

| Form | Interpretation |
|------|----------------|
| `[[#Heading]]` | Heading in current note |
| `[[Note#Heading]]` | Heading in another note |
| `[[Note#H1#H2]]` | Nested heading (H2 under H1) |
| `[[#H1#H2\|text]]` | Nested heading with display |

### Block References

| Form | Interpretation |
|------|----------------|
| `[[#^blockid]]` | Block in current note |
| `[[Note#^blockid]]` | Block in another note |
| `[[#^blockid\|text]]` | Block with display text |

### Embeds

| Form | Interpretation |
|------|----------------|
| `![[Note]]` | Embed entire note |
| `![[Note#Heading]]` | Embed from heading |
| `![[image.png]]` | Embed image |
| `![[Note#^block]]` | Embed specific block |

```
