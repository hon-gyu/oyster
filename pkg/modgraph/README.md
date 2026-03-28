# `modgraph` — OCaml Module Dependency Graph Extractor

## Overview

`modgraph` is a CLI tool that reads `.cmt` files produced by the OCaml
compiler and emits a DOT-format directed acyclic graph of module
dependencies, at both the compilation-unit level and the inline submodule
level.

It is intended for visualisation and codebase complexity analysis.

---

## Inputs

- One or more `.cmt` file paths, passed as positional CLI arguments
- `.cmt` files must have been produced with `-bin-annot` (dune default)

## Outputs

- A DOT-format graph on stdout
- Suitable for piping to `tred | dot -Tsvg`

---

## Node types

| Kind             | Example             | Description                                      |
|------------------|---------------------|--------------------------------------------------|
| Compilation unit | `Lsp_lib`           | Top-level node, one per `.cmt` file              |
| Inline submodule | `Lsp_lib.Handler`   | `module Foo = struct ... end` at top level of unit |

Nested submodules (submodules inside submodules) are out of scope for v1.

---

## Edge types

| Kind        | Example                          | Description                                  |
|-------------|----------------------------------|----------------------------------------------|
| Containment | `Lsp_lib -> Lsp_lib.Handler`     | Unit contains submodule                      |
| Reference   | `Lsp_lib.Handler -> Lsp_lib.Util`| Submodule body references another submodule  |
| Cross-unit  | `Main -> Lsp_lib`                | Compilation unit references another unit     |

---

## Reference resolution

A reference edge `A -> B` is emitted when:

- The body of module `A` contains a `Tmod_ident` or qualified `Texp_ident`
  (i.e. `Path.Pdot`) whose top-level path component resolves to `B`
- Unqualified value references within the same module are ignored (noise)

Resolution scope:

- **Local**: references to submodules within the same compilation unit
- **Cross-unit**: references to other compilation units passed as input
- **External**: references to modules not present in the input set —
  represented as leaf nodes with a distinct visual style (dashed border),
  controlled by `--external [show|hide|collapse]`

---

## CLI
```
modgraph [OPTIONS] <file.cmt> [<file.cmt> ...]

Options:
  --external show|hide|collapse
      Controls rendering of unresolvable external modules (default: hide)
      - show:     include as leaf nodes
      - hide:     omit entirely
      - collapse: merge all external deps into a single "External" node

  --unit-only
      Suppress inline submodule nodes; emit compilation-unit graph only
      (equivalent to codept/dune-deps output level)

  --no-containment
      Suppress containment edges; show only reference edges

  --tred
      Apply transitive reduction before emitting (shells out to `tred`)

  --output <file>
      Write DOT to file instead of stdout

  --format dot|json
      Output format (default: dot)
      JSON emits { nodes: [...], edges: [...] } for programmatic use
```

---

## DOT output conventions

- `rankdir=LR`
- Compilation unit nodes: `shape=box, style=bold`
- Inline submodule nodes: `shape=box`
- External nodes: `shape=box, style=dashed, color=grey`
- Containment edges: `style=dashed, color=grey` (structural, low visual weight)
- Reference edges: `style=solid` (the meaningful edges)
- Nodes within the same compilation unit grouped in a `subgraph cluster_*`

---

## Properties / invariants

- The emitted graph is a DAG (no self-edges, no duplicate edges)
- Node names are stable across runs for the same source tree
- Containment edges are never transitive (unit → immediate submodule only)
- `--unit-only` output must be equivalent to running `codept -dot` on the
  same source files (modulo external dep handling)

---

## Out of scope (v1)

- Nested submodules (depth > 1)
- Functor application edges
- `.cmti` / signature-level analysis
- Value-level (function) dependency graph within a module
- Incremental / watch mode
