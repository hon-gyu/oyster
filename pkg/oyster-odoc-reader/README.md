# oyster-odoc-reader

Read odoc documentation into an OysterMark vault: one note per odoc page.

```bash
dune build @doc
dune exec oyster-odoc-reader -- ./vault $(find _build/default/_doc/_odocls -name '*.odocl')
```

## Where it enters

By the time odoc writes a `.odocl` it has parsed the doc comments, resolved
every cross-reference, rendered the signatures and assigned an anchor to each
declaration. The file is nevertheless an internal OCaml `Marshal` value, not a
portable interchange format. The reader asks the `odoc` executable on `PATH`
to consume it and emit embeddable JSON. It then translates the JSON metadata
and HTML fragments, so the installed reader may be used from a different OCaml
switch than the one which built the documentation. Nothing parses `.ml`,
`.mli` or `.mld` source.

Run the command in the same active environment as `dune build @doc`: the
matching `odoc` process owns the internal file representation. The supported
cross-process boundary is the textual output of `odoc html-generate --as-json`.

```
.odocl ──▶ active odoc ──▶ embeddable JSON + HTML ──▶ note (+ subpages)
```

## What the output looks like

````markdown
{#val-build_index}
```ocaml
val build_index :
  ?stat_of_path:(Index.Path.t -> Index.file_stat) ->
  md_docs:(string * Cmarkit.Doc.t) list ->
  unit ->
  Index.t
```

Refs: [[oymarkit/Cmarkit/Doc#type-t]] [[oystermark/Vault/Index#type-t]]

- parameter: `stat_of_path`: how each vault-relative path becomes a
  [[oystermark/Vault/Index#type-file_stat|Index.file_stat]].
````

A signature stays in a fenced code block, because a fence's content is literal
and cannot hold a link; the references odoc resolved inside it are emitted as
wikilinks on a `Refs:` line beneath it. The edges reach the vault graph, their
position within the signature does not survive.

A documented member — a record field, a constructor — keeps its code line in
that same fence. No anchor can attach to a line inside a code block, so the
anchor and prose follow the fence as a keyed item. Tags (`@param`, `@raise`,
`@since`) become keyed items the same way. Each expansion becomes a note of
its own and the fence that held it collapses to `sig ... end`.

## Addressing

Notes mirror odoc's own page tree — a directory per module, carrying odoc's
disambiguating `kind-` prefix, so a module and a module type of the same name
do not collide:

```
oystermark/Vault.md
oystermark/Vault/Index.md
oystermark/Vault/Embed/module-type-Spec.md
```

Anchor identifiers admit only `[A-Za-z0-9_-]`. A `.` would read as a djot class
separator, so odoc's field and constructor anchors are rewritten and the
original kept alongside: `{#type-t-field odoc-anchor="type-t.field"}`.

## Known gaps

- Per-token syntax classes are lost when a signature flattens into a fence.
  Presentational; a highlighter regenerates them.
- Links from a declaration back to its source listing are not emitted. This
  build renders no source pages at all, so there is nothing to point at.
- A page's `@short_title`, `@children_order`, `@toc_status` and
  `@order_category` are dropped. Unused in this repo.

The JSON schema is experimental upstream. Its fields and the odoc HTML shapes
used here are covered by reader fixtures, making changes fail visibly instead
of reaching an unsafe deserializer.

See `docs/index.mld`.
