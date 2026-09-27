---
title: Links
---

# Links

Every link form, all pointing into [[anchors]]. Go to definition on each;
hover for a preview of what it lands on.

## Wikilinks

- Whole note: [[anchors]]
- Heading: [[anchors#Headings]]
- Nested heading: [[anchors#A nested heading]]
- Attribute id: [[anchors#stable-id]]
- Inline span: [[anchors#key-term]] — lands mid-line
- Explicit heading id: [[anchors#custom-heading-id]]
- Display text: [[anchors#Attribute ids|the attribute form]]

## Markdown links

The same anchors, no wikilink syntax involved: [note](notes/anchors.md),
[heading](notes/anchors.md#Headings), [span](notes/anchors.md#key-term).

## Within this note

`[[#Wikilinks]]` reaches [[#Wikilinks]]; the markdown form reaches
[this section](#Markdown links) — a destination may hold spaces.

Both carry an inlay arrow saying which way their target is and how many lines
away — `↑` back up this note, `↓` further down. Only intra-note links get one:
everything under [[#Wikilinks]] points at [[anchors]] and stays bare.
[[hints-and-lenses]] is the note about that.

## Unresolved

[[no-such-note]] is a diagnostic, and the code action on it creates the note.

[[anchors#no-such-heading]] is an unresolved fragment: under the default
`fallback` it lands on the file and stays quiet, under `strict` it is a
diagnostic. Set `goToDefinition.unresolvedFragment` in `oysterlsp.json` to see
both.

## Completion

Type `[[` on the line below for note names, then `#` for that note's anchors.
Inside `[[anchors#` the whole anchor namespace is offered — headings and
attribute ids.

## Also here

[[rename-me]], [[command-panel]], [[journal-index]].

These two links are what the lenses in [[hints-and-lenses]] count:
[[hints-and-lenses#Reference counts, as lenses]] and
[[hints-and-lenses#Link direction, as hints]].
