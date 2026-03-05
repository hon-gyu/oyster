Focus on pkg/oystermark/, other things don't matter too much at the moment.

The goal of this project is extends CommonMark with additional extenstions using cmarkit.

Additional notes:
- see pkg/cmarkit/src/cmarkit.mli for information about how to use cmarkit
- cmarkit extensions (inline/block) need explicit handling everywhere they appear — e.g. `Cmarkit.Inline.to_plain_text` requires an `~ext` callback to handle custom inline types like `Ext_wikilink`, otherwise it raises `Invalid_argument`
- `Cmarkit.Doc.t` is opaque with no `Meta.t` — you cannot attach arbitrary metadata to it. Use wrapper types (like `Parse.doc`) to carry side-channel data alongside the doc.


Processing pipeline:
1. Raw string → `Parse.of_string` strips YAML frontmatter, then parses markdown via cmarkit, then maps wikilinks + block IDs. Returns `Parse.doc = { doc : Cmarkit.Doc.t; frontmatter : Yaml.value option }`.
2. `Vault.build` walks the directory, calls `Parse.of_string` per `.md` file, builds an index of headings/block IDs.
3. `resolve` uses the vault index to resolve wikilink targets in a single doc.
4. `Html.of_doc` renders to HTML (frontmatter + body).


Project structure (pkg/oystermark/):
- lib/parse/ — pre-resolution file-level parsing (depends on: cmarkit, core, yaml)
  - frontmatter.ml — strips `---` delimited YAML frontmatter, parses via `Yaml.of_string`, renders to HTML
  - wikilink.ml — `[[target#fragment|display]]` syntax, registered as `Inline.t` extension (`Ext_wikilink`)
  - block_id.ml — `^blockid` detection, stored as metadata on blocks
  - parse.ml — ties parsers together into a cmarkit Mapper, defines `Parse.doc` wrapper type
- lib/vault/ — vault-level operations (Obsidian vault as a directory of markdown files)
  - index.ml — walks a vault dir, extracts headings and block IDs per file
  - link_ref.ml — link reference extraction from docs
  - resolve.ml — resolves wikilinks to concrete targets using the vault index
  - vault.ml — top-level vault build orchestration
- lib/html.ml — HTML rendering (depends on: tyxml)
- lib/oystermark.ml — public API surface
- bin/main.ml — CLI (`oystermark file <vault> <file>`, `oystermark vault <path> [output-dir]`)


Pitfalls:
- When adding new cmarkit inline/block extensions, you must handle them in ALL consumers: mappers, folders, renderers, and `to_plain_text` `~ext` callbacks. Missing any one causes `Invalid_argument` at runtime.
- `parse/dune` has `ppx_jane` for inline expect tests. If you add a new `.ml` in `lib/parse/` with `%expect_test`, it will work. Other libraries may not have ppx — check their `dune` file.
- The `yaml` opam package exposes `Yaml.value` (JSON-compatible subset) and `Yaml.yaml` (full YAML with anchors). We use `Yaml.value` throughout.


Style guide:
- we like explicit type annotation
