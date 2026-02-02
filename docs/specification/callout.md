# Callout Specification

Callouts are a special type of blockquote used to highlight information. 

## Relationship to Blockquotes

A callout is a blockquote whose first line matches the callout declaration syntax. A blockquote that does not match this syntax is simply a blockquote.

```
Blockquote:              Callout:
> Some text              > [!NOTE]
> More text              > Some text
```

## Syntax

### Grammar

```ebnf
callout      = ">" , [ space ] , "[!" , type , "]" , [ foldable ] , [ " " , title ] , newline , content ;

type         = letter , { letter } ;           (* case-insensitive *)
foldable     = "+" | "-" ;                      (* expanded or collapsed *)
title        = { any_char } ;                   (* until newline *)
content      = { ">" , [ " " ] , line } ;       (* subsequent blockquote lines *)

space        = " " ;                            (* exactly 0 or 1 space allowed *)
```

### Declaration Line Pattern

```
> [!TYPE](+|-) optional title
│ │    │  │    │
│ │    │  │    └─ Custom title (optional, rest of line)
│ │    │  └────── Foldable marker (optional)
│ │    └───────── Callout type (required, letters only)
│ └────────────── Exclamation mark (required)
└──────────────── Blockquote marker with 0-1 spaces
```

### Spacing Rules

The space between `>` and `[` is critical:

| Syntax | Result |
|--------|--------|
| `>[!NOTE]` | Valid callout (0 spaces) |
| `> [!NOTE]` | Valid callout (1 space) |
| `>  [!NOTE]` | Regular blockquote (2+ spaces) |

## Callout Types

### GFM Alert Types (Standard)

These are recognized by GitHub and other GFM-compatible renderers:

| Type | Description |
|------|-------------|
| `NOTE` | Highlights information |
| `TIP` | Helpful advice |
| `IMPORTANT` | Crucial information |
| `WARNING` | Needs attention |
| `CAUTION` | Risk/negative outcomes |

### Obsidian Types

Extended types specific to Obsidian:

| Type | Aliases | Description |
|------|---------|-------------|
| `ABSTRACT` | `SUMMARY`, `TLDR` | Summary/abstract |
| `INFO` | - | Informational |
| `TODO` | - | Task/todo item |
| `SUCCESS` | `CHECK`, `DONE` | Success/completion |
| `QUESTION` | `HELP`, `FAQ` | Question/help |
| `FAILURE` | `FAIL`, `MISSING` | Failure/error |
| `DANGER` | `ERROR` | Dangerous/error |
| `BUG` | - | Bug report |
| `EXAMPLE` | - | Example |
| `QUOTE` | `CITE` | Citation/quote |

### Custom/Unknown Types

Any unrecognized type becomes an `Unknown` callout:
- Preserves the original type string (lowercased)
- Can be styled via CSS using the type name
- Default title is the capitalized type name

```markdown
> [!CUSTOM] My Custom Callout
> This uses a custom type
```

## Foldable Callouts

Callouts can be made collapsible with `+` or `-` after the type:

| Marker | Meaning |
|--------|---------|
| `+` | Foldable, expanded by default |
| `-` | Foldable, collapsed by default |
| (none) | Not foldable |

```markdown
> [!FAQ]+ Expanded by default
> This content is visible initially

> [!FAQ]- Collapsed by default
> This content is hidden initially
```

## Title

### Custom Title

Text after `[!TYPE]` (and optional foldable marker) becomes the title:

```markdown
> [!NOTE] Custom Title Here
> Content
```

- Title extends to the end of the first line (before newline/softbreak)
- Leading space after `]` or `+`/`-` is trimmed
- If no title provided, uses default (capitalized type name)

### Default Titles

| Type | Default Title |
|------|---------------|
| `NOTE` | Note |
| `TIP` | Tip |
| `WARNING` | Warning |
| `ABSTRACT` | Abstract |
| `INFO` | Info |
| `QUESTION` | Question |
| `CUSTOM` | Custom |
| (unknown) | Capitalized type |

## Content Structure

A callout consists of:

1. Declaration - First line with `[!TYPE]` syntax
2. Content - Remaining blockquote lines (optional)

### AST Structure

```
Callout
├── CalloutDeclaration
│   └── Paragraph (contains [, !TYPE, ], title text)
└── CalloutContent (optional)
    ├── Paragraph
    ├── List
    ├── CodeBlock
    └── ... (any block elements)
```

### Content Types

Callout content can contain any block elements:
- Paragraphs
- Lists
- Code blocks
- Tables
- Even nested callouts

```markdown
> [!NOTE]
> A paragraph
> - List item 1
> - List item 2
> ```python
> code block
> ```
```

## Nested Callouts

Callouts can be nested using additional `>` markers:

```markdown
> [!question] Can callouts be nested?
> > [!todo] Yes!, they can.
> > > [!example] You can even use multiple layers of nesting.
```

Each nesting level adds another `>` prefix.

## Edge Cases

### Title-Only Callout (No Content)

```markdown
> [!INFO] Just a title, no content body
```

This is valid - the callout has only a declaration, no content.

### Empty Content Lines

```markdown
> [!NOTE]
>
> Content after blank line
```

The blank line `>` is part of the content.

### Code Blocks in Callouts

```markdown
> [!NOTE]
> ```python
> def hello():
>     print("Hello")
> ```
```

Code blocks work normally within callouts.

### Callout as Block Reference Target

Callouts can have block identifiers:

```markdown
> [!info] Important callout

^callout-id
```

The `^callout-id` on a separate line references the entire callout block.

## Non-Callout Patterns

These blockquote patterns do **not** constitute callouts:

| Pattern | Reason |
|---------|--------|
| `>  [!NOTE]` | 2+ spaces after `>` |
| `> [NOTE]` | Missing `!` |
| `> !NOTE` | Missing brackets |
| `> [!NOTE123]` | Numbers in type |
| `> [!NO-TE]` | Hyphen in type |
| `> [!NO_TE]` | Underscore in type |
| `> [!]` | Empty type |
| ` > [!NOTE]` | Space before `>` |

## Implementation Reference

This section describes one possible implementation approach.

1. Identify blockquote node
2. Get first paragraph child
3. Check spacing after `>` (must be 0-1 spaces)
4. Match pattern: `[` + `!TYPE` + `]` in first three text nodes
5. Extract foldable marker if present (`+` or `-`)
6. Extract custom title (rest of first line)
7. Separate declaration from content at first line break
8. Create Callout node with Declaration and Content children

## Examples

### Basic Callout

```markdown
> [!NOTE]
> This is a note callout
```

### With Custom Title

```markdown
> [!WARNING] Security Advisory
> Please update your dependencies immediately.
```

### Foldable with Title

```markdown
> [!FAQ]- Frequently Asked Questions
> **Q: What is Obsidian?**
> A: A knowledge management tool.
```

### Nested Structure

```markdown
> [!EXAMPLE] Code Example
> Here's how to use it:
> ```javascript
> const x = 1;
> ```
> - Point 1
> - Point 2
```

### Using Aliases

```markdown
> [!TLDR]
> This is an abstract/summary callout (alias for ABSTRACT)

> [!CITE]
> This is a quote callout (alias for QUOTE)
```


## Rendering

Callouts render as a container with:
1. A declaration/header showing type and title
2. Content body

### Foldable Callouts

Foldable callouts use HTML disclosure elements (`<details>`/`<summary>`):

| Marker | Behavior |
|--------|----------|
| `+` | Expanded by default (`open` attribute) |
| `-` | Collapsed by default |
| (none) | Not foldable (no disclosure element) |
