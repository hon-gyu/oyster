# mlmdx

MDX for OCaml: prose-first `.mlmdx` pages with embedded, **type-checked** OCaml
expressions and components, rendered to HTML at build time (static site
generation).

A `.mlmdx` file compiles to an ordinary OCaml module exposing a `make` function —
the analog of MDX compiling Markdown to a JS component module. Because the target
is OCaml, everything embedded in a page is type-checked as part of the normal
build: a component used with the wrong prop type is a compile error, not a
runtime surprise. (Not to be confused with `ocaml-mdx`, which *executes* code
blocks in documentation — mlmdx is the opposite direction: markup embedded in a
program, compiled and rendered.)

## The pipeline

```
.mlmdx
  │  oymarkit.cmarkit  (parser only; native Ext nodes for {expr}, inline JSX,
  │                     and block JSX, storing raw source + Textloc)
  ▼
Cmarkit.Doc.t
  │  mlmdx-pp  ── structural nodes ────▶ plain JSX.node / string / list
  │            ── {expr} / prop values ─▶ compiler-libs Parse.expression
  │                                        on a position-primed lexbuf
  │            ── host JSX / fragments ─▶ JSX.node / JSX.list
  │            ── component JSX ────────▶ Component.make calls
  ▼
Parsetree.structure   (plain OCaml, [@JSX]-attributed, ghost-located wrappers)
  │  html_of_jsx.ppx   (per-library, lowers [@JSX])
  ▼
plain OCaml module  ──  JSX.render  ──▶  HTML string
```

Two thin binaries — `mlmdx-pp` (the dialect preprocessor) and `ocamlmerlin-mlmdx`
(the merlin reader) — share one `Codegen` core; they differ only in the output
wrapper.

## Dependencies and scope

mlmdx depends on the **parser** half of `oymarkit` (a cmarkit fork we own) plus
`compiler-libs` and the `html_of_jsx` runtime — and explicitly **not** on
`pkg/oystermark` (the vault: wikilinks, embeds, Tyxml renderer). Different
product, same underlying Markdown parser.

- **`oymarkit.cmarkit`** — parses `.mlmdx` prose to a `Cmarkit.Doc.t`. Native
  extension nodes (`Inline.Ext_jsx_expr`, `Inline.Ext_jsx_element`,
  `Block.Ext_jsx_block`, gated behind `?jsx_expr` / `?jsx_element` on
  `Doc.of_string`) capture raw source plus `Textloc` and do no OCaml parsing — no
  compiler-libs leaks into oymarkit.
- **`compiler-libs`** — `mlmdx-pp` runs `Parse.expression` on the leaf OCaml that
  authors write (`{expr}` bodies and component prop values). The load-bearing
  discipline: prime the lexbuf's absolute position from the stored `Textloc`, so
  type errors and hovers land on the right byte of the `.mlmdx`.
- **`mlx`** — used *stock*, for hand-written `.mlx` component files only, via the
  dune dialect declared in the root `dune-project`. `mlmdx-pp` does **not** link
  or invoke mlx; it reparses the raw JSX tags oymarkit identified and builds the
  `[@JSX]` parsetree directly. (mlx exposes no callable parser library — only its
  `mlx-pp` / `ocamlmerlin-mlx` binaries.)
- **`html_of_jsx`** — `html_of_jsx.ppx` lowers `[@JSX]`-attributed parsetree
  (from both `mlx-pp` on `.mlx` and `mlmdx-pp` on `.mlmdx`) into
  `JSX.node`/component-`make` calls; `JSX.render` produces the HTML string. This
  is the whole SSG runtime — inert HTML, no client JS in the toolchain.
- **`merlin-extend`** — `ocamlmerlin-mlmdx`, a reader wrapping the same codegen,
  gives `.mlmdx` files hovers and jumps; dispatch is per-extension via dune.

## Status

The full `.mlmdx` → HTML chain is working end-to-end:

- **`lib/codegen.ml`** (`mlmdx_codegen`): `Doc.t → Parsetree.structure` exposing
  `let make () = <element>`. Structural nodes → plain `JSX.node`/`string`/`list`;
  `{expr}` and JSX prop expressions → `Parse.expression` on a position-primed
  lexbuf; host JSX → `JSX.node`, fragments → `JSX.list`, component JSX →
  `Component.make`. Covered by inline expect tests (`ppx_expect`) that pin the
  generated parsetree.
- **Strict prelude**: top-of-file `open`/`let`/`module` blocks are parsed as OCaml
  structure items before the generated `make`; the first Markdown block switches
  permanently to Markdown.
- **`pp/mlmdx_pp.ml`** (`mlmdx-pp`): the dialect preprocessor, emitting the binary
  `-pp` AST protocol (magic number + filename + structure). Registered as the
  `mlmdx` dialect in the root `dune-project`.
- **`pp/ocamlmerlin_mlmdx.ml`** (`ocamlmerlin-mlmdx`): a merlin-extend reader over
  the same codegen core, wired via `(merlin_reader mlmdx)`.
- **`examples/hello/`**: the full-chain example (see its README), pinned by a cram
  test on the rendered HTML.

Key property proven: a type error inside `{expr}` (e.g. `{JSX.int "x"}`) reports
against the `.mlmdx` file at the exact byte of the offending code, quoting the
`.mlmdx` source line — the position-priming works.

```
dune exec pkg/mlmdx/examples/hello/render.exe
# <h1>4</h1><p>Some <strong>bold</strong> prose and an inline value: 42.</p>...
```

### Not yet done

- **The components table** — the overridable `_components` map (`# Hi` becomes
  `<_components.h1>`, not a hardcoded `<h1>`), so a consumer can restyle all
  headings or swap a component page-wide. This is what makes mlmdx *MDX*, not
  markdown-to-HTML; structural nodes currently lower to hardcoded elements.

Explicitly out of v1 scope: JSX inside embedded `{expr}`; client interactivity /
hydration; a `.mlmdx` formatter; integration with the oystermark vault.
