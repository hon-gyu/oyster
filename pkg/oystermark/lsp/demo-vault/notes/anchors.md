---
title: Anchors
---

# Anchors

The target note: most links in this vault point somewhere inside it. Jumping
from them is [[links]].

## Headings

Reachable by its text, `[[anchors#Headings]]`, or by the identifier djot
gives it.

A heading is exactly its own line. Only a heading of the note opens a
section: one inside a block quote or a list item is a plain heading.

### A nested heading

Nesting is what document outline renders as a tree.

## Attribute ids

{#stable-id}
An attribute id is written on the line above the block it names, and is the
only anchor you control: heading text changes with the prose.

The [key term]{#key-term} is an inline span — `[[anchors#key-term]]` lands on
the phrase, at the character. Headings and blocks are line-granular.

{#aside}
> Any block takes an id, callouts included.

{#custom-heading-id}
## An explicit id on a heading

Still reachable by text, and now also as `[[anchors#custom-heading-id]]`.

Pandoc's trailing form (`## Heading {#id}`) is not read here: no anchor is
recorded and the heading text keeps a trailing space, so the by-text link
breaks too.

## Here

Find references on any heading above. Inlay hints count them, per heading and
for the whole file at line 0. Code actions offer *Insert oysterlsp command
block*, since this note has none.
