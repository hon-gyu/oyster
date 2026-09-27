# oystermark-lsp

Language server for Oystermark vaults.

## Markdown flavor

Notes are [djot](https://djot.net), parsed by djot.v's extracted parser
(`vendor/djot`) in its `markdown_like` profile with three extensions:
wikilinks, keyed blocks (`label: value`) and Obsidian callouts. Markdown
spellings that djot lacks are added: `**strong**`, setext headings, sublists
without a blank line, and one-line ATX headings. It is not CommonMark:

| Markdown | In a note |
|---|---|
| `*a*` | plain text; write `_a_` |
| `$x$`, `$$...$$` | text; write `` $`x` `` and `` $$`...` `` |
| `snake_case_name` outside code | `case` is emphasized; use backticks |
| `~~a~~` | nested subscript; write `{-a-}` |
| indented code | a paragraph; use a fence |
| raw HTML | text; write `` `<b>`{=html} `` |
| two trailing spaces | nothing; use a backslash break |
| `___` | text; write `***` or `---` |
| `` ```lang arg=1 `` | not a code block: an info string is one word |

A heading is exactly its own line. Only a heading of the document opens a
section; one inside a block quote, div, callout or list item is a plain
heading.

## Anchors

A `#fragment` is matched against one namespace, in this order; first match wins:

| # | Kind | Written as | Referenced by | Granularity |
|---|---|---|---|---|
| 1 | Heading | `# My Heading` | `[[note#My Heading]]` (heading **text**, or its identifier `My-Heading`) | line |
| 2 | Attribute id | `{#id}` / `[text]{#id}` | `[[note#id]]` | block, or **inline span** |

Attribute ids are author-controlled and stable, and are the only kind that can
pin an arbitrary inline span:

```markdown
{#custom-h}
# My Heading

The [key term]{#kt} is defined here.

{#aside}
> An aside worth linking to.
```

```markdown
See [[my-note#kt]] and [[my-note#aside]]; within the note, [[#My Heading]].
A markdown link reaches the same anchors: [the key term](#kt), [aside](#aside).
```

Both link syntaxes reach both anchor kinds — nothing in resolution is
specific to wikilinks, and nothing is specific to headings. The full 2×3 matrix
is pinned by the expect test *"intra-note: {wikilink, markdown link} x
{heading, block attribute, inline attribute}"* in
[go_to_definition.ml](go_to_definition.ml).

Note: an explicit `{#id}` on a heading *replaces* its generated identifier. The
heading is still reachable by its text (`[[note#My Heading]]`), but not by the
derived identifier.

Anchors are collected and resolved in the core (`lib/vault/index.ml`,
`lib/vault/resolve.ml`); the LSP consumes resolved targets and adds no anchor
logic of its own. Spec: [feature-attribute-anchors.mld](docs/feature-attribute-anchors.mld).

## Features

| Feature | Behavior | Spec |
|---|---|---|
| Go to definition | Jumps to a link's target. Inline attribute anchors resolve to line *and* character. | [go-to-definition](docs/feature-go-to-definition.mld) |
| Find references | From a link or an anchor, lists every link resolving to the same target. | [find-references](docs/feature-find-references.mld) |
| Completion | Note names after `[[`; heading texts and attribute ids after `#`. | [completion](docs/feature-completion.mld) |
| Diagnostics | Unresolved links and fragments; duplicate ids in one file. | [diagnostics](docs/feature-diagnostics.mld) |
| Hover | Preview of the target note or section. | [hover](docs/feature-hover.mld) |
| Document outline | Headings and struct keys as a symbol tree. | [document-outline](docs/feature-document-outline.mld) |
| Code lens (reference counts) | `3 references` above a heading, `12 backlinks` above the note; clicking opens them. | [codelens-reference-counts](docs/feature-codelens-reference-counts.mld) |
| Inlay hints | `[[#setup]]↓8` — which way an intra-note link's target is, and how far. | [inlay-hints-link-direction](docs/feature-inlay-hints-link-direction.mld) |
| Rename | Renames a note and rewrites the links pointing at it. | [rename](docs/feature-rename.mld) |
| Code action | Creates the note behind an unresolved link. | [codeaction](docs/feature-codeaction-create-unresolved-link.mld) |
| Daily notes | Open or create today's / yesterday's / tomorrow's note, and jump to the previous or next existing one. | [daily-notes](docs/feature-daily-notes.mld) |
| Command block | A fenced `oysterlsp` block whose lines become clickable code lenses — a small control panel inside a note. | [command-block](docs/feature-command-block.mld) |

## Demo vault

[demo-vault/](demo-vault/) is a vault where every feature in the table above
has somewhere to be tried. Open the directory as the workspace root — it
carries its own `oysterlsp.json` — and start from `start-here.md`.

Its control panel, `command-panel.md`, is generated from
`Command_block.all_of_command` by a dune rule and promoted into the source
tree, so a new command appears there on the next build rather than the note
quietly going out of date. Regenerate it with:

```bash
dune build @demo-vault
```

The vault is a demo, not a test: nothing asserts against it. Two of its notes
are wrong on purpose — `notes/diagnostics.md` and the unresolved section of
`notes/links.md` — and everything else in it should be quiet.

## Adapter smoke check

`tests/lsp/` drives `Lsp_lib.Server` in process, so `main.ml` — capability
advertisement and request dispatch — is not covered by `dune runtest`. The gap
is not theoretical: `workspace/executeCommand` was once answered from
`on_request_unhandled`, which linol never consults for `ExecuteCommand`, so the
daily-note command silently returned `null` in every client while the suite
stayed green.

After changing `main.ml`, run the session-level check by hand:

```bash
dune build pkg/oystermark/lsp/main.exe
uv run --no-project pkg/oystermark/lsp/scripts/smoke.py
```

It opens a real stdio session against a bare vault (no `oysterlsp.json`, no
`initializationOptions`) and asserts the effects an editor observes: the
advertised capabilities, the code actions offered, that a creating action
carries the edit that opens its note, that a command reaches its handler and
produces `window/showDocument` — plus `workspace/applyEdit` when it is the one
asked to create — and that a command-block line gets a lens carrying a command. Pass a path to check
another binary — `… scripts/smoke.py $(which oystermark-lsp)` verifies what is
installed, which is what your editor actually runs.

## Configuration

Settings come from `oysterlsp.json` at the vault root, falling back to the
client's `initializationOptions` key by key — full schema in
[configuration](docs/feature-configuration.mld).

For a starting file with every key at its default:

```bash
oystermark-lsp --print-default-config > oysterlsp.json
```

It includes a `"$schema"` line pointing at the raw
[oysterlsp.schema.json](oysterlsp.schema.json) on `main`, which any JSON
language server picks up — that is where key completion, value validation and per-key
documentation come from, so prefer it over memorizing the table.

```json
{
  "dailyNotes": {
    "format": "YYYY/MM/YYYY-MM-DD",
    "folder": "journal",
    "linkAction": false
  },
  "hover": { "maxChars": 400 },
  "goToDefinition": { "unresolvedFragment": "strict" },
  "diagnostics": { "unresolvedFragment": "strict" }
}
```

`"disable": true` turns the server off for that vault: it still starts — a
client spawns it, and this file is only read afterwards — but it indexes
nothing and answers every request emptily, saying so once at startup. It is for
the directory of Markdown that is a vault only by accident, where wikilink
diagnostics are noise; the verdict travels with the directory rather than
living in each contributor's editor settings.

`linkAction` (default `true`) governs the one daily-note action offered in
every menu — *Insert link to today's daily note* — so it can be withdrawn
without disabling daily notes.

The daily-note `format` is a subset of the
[moment.js](https://momentjs.com/docs/#/displaying/format/) tokens and may
contain `/` to nest notes in folders; `folder` defaults to the vault root.

Nothing here can stop the server from starting: a missing file, bad JSON, an
unknown key or an unusable value falls back to the default. Every such fallback
is reported as a warning message at startup, so an ignored setting is never
silent. Obsidian's own `.obsidian/daily-notes.json` is not read — see
[the reference spec](../../../specification/obsidian/daily-notes.md) for how
Obsidian behaves.

Positions are UTF-16 code units, per the LSP spec — see
[feature-utf16-positions](docs/feature-utf16-positions.mld).
