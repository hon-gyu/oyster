---
title: Hints and lenses
---

# Hints and lenses

Two annotations the editor draws rather than stores — neither is in the file,
and neither can be selected. They differ in where they go, and that is the
whole of the difference:

- a **lens** takes a line of its own, above what it is about
- an **inlay hint** is squeezed into the line, beside what it is about

## Reference counts, as lenses

Above the top of this note sits `N backlinks`: every link in the vault that
resolves here, whatever fragment it named. Above each heading something points
at sits `N references`, counting only the links that land on _that heading_.

This is the shape every other language server gives this — VS Code writes
`3 references` over a TypeScript symbol, rust-analyzer and the Java server do
the same. Clicking one opens the references it counted.

A heading nothing points at gets no lens: a column of `0 references` would be
the loudest thing on the page.

Clients keep their own switch for lenses, and usually ship it off. In Zed,
`"code_lens": "on"`. If the arrows below show up but nothing sits above the
headings, that switch is why.

## Link direction, as hints

A link whose target is in this same note gets an arrow and the number of lines
to it — the two things a list of near-identical `[[#…]]` tokens otherwise
hides:

- [[#Reference counts, as lenses]] points back up
- [[#Where the arrows stop]] points down
- [[#block-line]] points at a block, not a heading
- [[#pivot]] points at an inline span, further down

The arrow carries the direction, so the number is always a plain distance:
`↑6`, never `↑-6`.

Two links on one line each get their own arrow, beside their own link:
[[#Reference counts, as lenses]] and [[#Where the arrows stop]]. That is why
the hint sits after the link rather than at the end of the line — one arrow at
the end of this line could belong to either.

A target on the _same line_ is the one case with no distance to report, so the
arrow turns sideways instead. The line below holds a span and a link on either
side of it:

[[#pivot]] before the [pivot]{#pivot} and [[#pivot]] after it.

Only an inline span can be a same-line target — a heading or a block owns
its whole line.

{#block-line}
This paragraph is named by an attribute instead.

## Where the arrows stop

Silence, not an empty hint, wherever there is no direction to give:

- cross-note links — [[anchors]], [[links#Wikilinks]] — the arrow would be
  about a file you are not looking at
- unresolved links: [[no-such-note-here]]
- a link to this note as a whole: [[hints-and-lenses]]

## Turning them off

Two keys in `oysterlsp.json`, one per annotation:

- `codeLens.references` — the counts
- `inlayHints.linkDirection` — the arrows

A third, `codeLens.showReferencesCommand`, is the name your client answers to
when a lens is clicked. There is no standard one: the default is VS Code's
name, which Zed also implements. Set it to `""` and the lens becomes a label
that does nothing when clicked.

Set any of them and restart the server.
