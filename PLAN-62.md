# Plan: Support backlinks via LSP (#62)

## Context

Issue #62 asks for backlinks support in the LSP. The existing codebase already has:
- **Feature specs** for both `textDocument/references` (`feature-find-references.mld`) and `textDocument/inlayHint` (`feature-inlay-hints.mld`) — fully designed, not yet implemented
- **A rendering-side backlink component** (`component.ml:275-574`) that scans vault docs for links to a target path — operates on `Vault.t` with pre-resolved metadata

The issue lists two levels: display-only (diagnostics/inlay hints) and clickable (code actions). The specs already cover both via **find-references** (clickable — "go to references" in editors) and **inlay hints** (display-only — shows ref counts next to headings).

## Key insight: reuse resolved docs from vault building

Currently, the LSP's `build_vault_index` (`main.ml:7-28`) parses all docs but only keeps the `Index.t`, throwing away the parsed+resolved docs. But `Vault.of_root_path` already does parse → index → resolve links (attaching `Resolve.resolved_key` metadata to every link node) → expand embeds.

The backlink component (`component.ml`) shows the pattern: it walks `vault.docs` and reads the pre-attached `resolved_key` metadata — no re-parsing or re-resolving needed.

**Change**: Store the full `Vault.t` (or at least the resolved `(string * Cmarkit.Doc.t) list`) in the LSP server, not just the `Index.t`. This lets find-references scan pre-resolved docs directly.

## Approach: Implement find-references first, then inlay hints

Both features share a core operation: "scan resolved vault docs and find links whose target matches." Build that core once, then wire into both LSP handlers.

### Step 0: Upgrade LSP server to keep `Vault.t`

In `main.ml`:
- Change `val mutable index` to `val mutable vault` (of type `Vault.t`)
- Change `build_vault_index` to use `Vault.of_root_path` (or a variant that skips embed expansion, since the LSP doesn't need it)
- Update existing feature callsites to use `vault.index` where they currently use `index`

Note: We may want to skip the embed expansion step since the LSP doesn't render — just parse + build index + resolve. Check if `Vault.t` can be built without `Embed.expand_docs`, or just accept the small overhead.

### Step 1: Core module `find_references.ml`

New file: `pkg/oystermark/lsp/find_references.ml`

**Key function:**
```
val find_references
  :  docs:(string * Cmarkit.Doc.t) list   (* pre-resolved vault docs *)
  -> target_path:string
  -> target_fragment:fragment option       (* None = file-level, Some = heading/block *)
  -> (string * int * int) list             (* (rel_path, first_byte, last_byte) *)
```

Algorithm (following the spec, using pre-resolved docs):
1. Iterate over all `vault.docs`
2. For each doc, use `Link_collect.collect_links` to get links with byte ranges
3. For each link, read the pre-attached `Resolve.resolved_key` from its metadata
4. Check if the resolved target matches `target_path` (and optionally fragment)
5. Collect matching `(rel_path, first_byte, last_byte)` triples
6. Sort by (path, first_byte) for determinism

**Cursor-position detection** — the spec has two activation modes:
- **Cursor on a link**: resolve the link to determine target path + fragment, then find all refs to that target
- **Cursor on a heading/block ID**: target = (current file, heading slug / block id)

For heading detection, reuse `Hover.heading_level_of_line` + `Parse.Heading_slug.slugify`. For block ID detection, check if the line ends with ` ^<id>`.

### Step 2: Wire `textDocument/references` into `main.ml`

- Override `config_modify_capabilities` to add `~referencesProvider:(\`Bool true)` to `ServerCapabilities`
- Override `on_request_unhandled` — linol routes `TextDocumentReferences` there (it's a GADT `r Lsp.Client_request.t`, so we pattern match on the constructor and return `Location.t list`)

### Step 3: Inlay hints module `inlay_hints.ml`

New file: `pkg/oystermark/lsp/inlay_hints.ml`

Reuses the same scanning logic from find_references but returns counts instead of locations:

```
val inlay_hints
  :  docs:(string * Cmarkit.Doc.t) list   (* pre-resolved vault docs *)
  -> rel_path:string
  -> content:string
  -> range_start_line:int
  -> range_end_line:int
  -> (int * int * string) list   (* line, character, label *)
```

Per spec:
- File-level hint at (0, 0) showing total incoming link count
- Per-heading hint at end of heading text showing heading-specific count
- Skip hints with count = 0

Internally, call `find_references` with different target params for each heading, and count results. Or build a single pass that counts per-target.

### Step 4: Wire `textDocument/inlayHint` into `main.ml`

- Override `config_inlay_hints` to return `Some (\`Bool true)`
- Override `on_req_inlay_hint` — linol already has this method (line 365 in server.ml)

## Files to create/modify

| File | Action |
|------|--------|
| `pkg/oystermark/lsp/find_references.ml` | **Create** — core scanning + references handler |
| `pkg/oystermark/lsp/inlay_hints.ml` | **Create** — inlay hint computation |
| `pkg/oystermark/lsp/lsp_lib.ml` | **Modify** — export new modules |
| `pkg/oystermark/lsp/main.ml` | **Modify** — add config + handlers |
| `pkg/oystermark/lsp/dune` | **Modify** — add new modules if needed (check if auto) |
| `pkg/oystermark/tests/lsp/test_find_references.ml` | **Create** — unit + E2E tests |
| `pkg/oystermark/tests/lsp/test_inlay_hints.ml` | **Create** — unit + E2E tests |
| `pkg/oystermark/tests/lsp/lsp_helper.ml` | **Modify** — add `references` and `inlay_hint` request helpers |

## Reusable code

- `Vault.Resolve.resolved_key` — metadata key on resolved link nodes; read it to get `Resolve.target` without re-resolving
- `Component.Backlink` (`component.ml:275-574`) — algorithm reference for scanning vault docs and matching links by resolved target path
- `Link_collect.collect_links` / `find_at_offset` — link detection with byte ranges
- `Lsp_util.byte_offset_of_position` / `position_of_byte_offset` — coordinate conversion
- `Hover.heading_level_of_line` / `find_heading_in_content` — heading detection
- `Parse.Heading_slug.slugify` — slug generation

## Verification

1. `dune build` — compiles
2. `dune runtest pkg/oystermark/lsp pkg/oystermark/tests/lsp` — all tests pass
3. `dune build @doc-private @doc` — docs build
4. Manual: open vault in editor, verify "Find References" on a wikilink shows backlinks, verify inlay hints show ref counts next to headings
