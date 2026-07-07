# hello

The end-to-end mlmdx example: one prose-first `.mlmdx` page that embeds OCaml
expressions and instantiates typed `.mlx` components, rendered to an HTML string
at build time.

## Files

- `hello.mlmdx` — the page. A top-of-file `let panel_title = "Panel"` prelude,
  then Markdown prose carrying every embedded construct: `{expr}` spans
  (`{JSX.int (2 + 2)}`), self-closing and container component calls
  (`<Greeting .../>`, `<Panel>...</Panel>`), and raw host JSX (`<b class="loud">`,
  `<div className="box">`) with Markdown nested inside.
- `greeting.mlx` — a typed component, `~name:string ~count:int`. Because it is
  lowered to plain OCaml before type-checking, `<Greeting count={1 + 1} />` in the
  page type-checks the prop at the call site; passing a string would be a compile
  error pointing at the exact byte.
- `panel.mlx` — a container component taking `~children`.
- `render.ml` — plain OCaml driving the page. Renders it twice: once with the
  default components table (`JSX.render (Hello.make ())`) and once with an
  overridden `h1` (`~components:{ Mlmdx.Components.default with h1 = … }`), to show
  that markdown headings route through the overridable table while literal JSX and
  `<Component/>` calls do not.
- `render.t` — a cram test pinning both rendered lines (the end-to-end regression
  check).

## Run

```
dune exec pkg/mlmdx/examples/hello/render.exe
```

```
<h1>4</h1><p>Some <strong>bold</strong> prose and an inline value: 42.</p>...
<h1 class="title">4</h1>...   (same page, h1 overridden via the components table)
```

## The chain

```
hello.mlmdx  ──mlmdx-pp (dialect)──▶  Parsetree ([@JSX])  ──html_of_jsx.ppx──▶  OCaml
   ▲ Greeting/Panel (.mlx, via mlx-pp)                                    │
   └──────────────────────────────────────────────  Hello.make ()  ──JSX.render──▶ HTML
```

`mlmdx-pp` lowers the `.mlmdx`; `mlx-pp` lowers the hand-written `.mlx`
components; both emit `[@JSX]`-attributed parsetree that `html_of_jsx.ppx`
turns into `JSX.node`/component-`make` calls. Everything collapses to one
plain-OCaml module graph that type-checks together.
