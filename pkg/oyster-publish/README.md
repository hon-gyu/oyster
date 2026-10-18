# oyster-publish

- Turn a vault, a plain directory of notes, into a static site, where the
folder structure is the navigation.
- It reads OysterMark, not plain Markdown. Notes are parsed by the `oystermark` OCaml parser, so the accepted syntax is CommonMark plus djot extensions, Obsidian's vault syntax, and a few constructs of our own. 
- The site it builds follows Obsidian Publish. Navigation, graph view,
  backlinks, hover previews, stacked pages, outline and search are modelled on
  that feature set, rebuilt on Astro. 

## Install

```bash
npm install
npm link          # puts `oyster-publish` on your PATH
```

Or with Nix, from the flake at the repository root:

```bash
nix run github:hon-gyu/oyster#oyster-publish -- build -v ~/notes/myblog
```

In a devenv, add the flake as an input in `devenv.yaml`:

```yaml
inputs:
  oyster:
    url: github:hon-gyu/oyster
    inputs:
      nixpkgs:
        follows: nixpkgs
```

and the package in `devenv.nix`:

```nix
packages = [ inputs.oyster.packages.${pkgs.stdenv.system}.oyster-publish ];
```

The Nix store is read-only and Astro writes `.astro/` into its project root,
so a Nix install runs Astro from a writable copy of the package in
`~/.cache/oyster-publish/` (or `$XDG_CACHE_HOME`), made once per install.
After an update, the copy for the previous version can be deleted.

When the npm dependencies change, update `npmDepsHash` in `package.nix`: set it
to `lib.fakeHash`, run `nix build .#oyster-publish`, and copy the hash from the
error.

The parser is vendored as `src/lib/oystermark/oystermark.cjs`, a js_of_ocaml
build of `pkg/oystermark/js`. To refresh it against the current OCaml sources:

```bash
OYSTERMARK_DIR=<repo root> npm run build:parser
```

## Usage

```bash
oyster-publish dev   -v ~/notes/myblog              # preview at localhost:4321
oyster-publish build -v ~/notes/myblog -o ./site    # static site in ./site
```

| Option | |
|---|---|
| `-v, --vault <path>` | Vault directory to publish (required) |
| `-o, --target <path>` | Output directory (default: `./dist`) |
| `--published-only` | Only notes with `publish: true` in frontmatter |
| `--title <name>` | Site name (default: the vault folder's name) |
| `--favicon <url>` | Favicon URL path, e.g. `/favicon.svg` |
| `--no-ancestor-initials` | Show folders in the browser tab in full (see below) |

Astro's own options go after a `--` separator:

```bash
oyster-publish dev -v ~/notes/myblog -- --host 0.0.0.0
```

## Site features

Modelled on Obsidian Publish:

| | |
|---|---|
| Navigation | The folder tree, in the sidebar |
| Graph view | Force-directed, drag/zoom, per-note and global |
| Backlinks | Contextual — the surrounding sentence, not just the note name |
| Page preview | Hover a wikilink to see the target's opening |
| Stacked pages | Alt-click a wikilink (desktop) to open it beside the current note instead of replacing it; the stack lives in `?stack=`, ctrl+←/→ moves through it |
| Outline | Per-note table of contents |
| Search | URL-backed `/search` page |

Beyond Obsidian Publish: chronological `/archive` and `/tags` pages, and a
mobile shell.

## Site config

Settings live in the vault:

```
myvault/
├── .oyster/
│   ├── config.json      { "title": "My Blog", "favicon": "/favicon.svg" }
│   └── public/
│       └── favicon.svg  → served at /favicon.svg
├── Field Notes/
│   └── Growing Tomatoes.md
└── index.md
```

| `config.json` key | |
|---|---|
| `title` | Site name, shown in the sidebar and the browser tab |
| `favicon` | URL path of the icon, served from `.oyster/public/` |
| `ancestorInitials` | Abbreviate folders in the browser tab (default: `true`) |
| `publication` | Optional folder selection and linked-note closure (below) |

Anything in `.oyster/public/` is served at the site root — the favicon, and any
other static asset. The `.oyster/` folder stays hidden in Obsidian and never
becomes a page. The CLI flags above override `config.json` for a one-off build.

## Names

A note is shown as its frontmatter `title:` if it has one, otherwise as its
**filename, exactly as it is on disk** — in the nav, the graph, search hits and
the browser tab alike. Nothing is prettified: `Modern AI` stays `Modern AI`, and
`getting-started` stays `getting-started`. Write a `title:` to change a label.

Browser tabs are narrow, and a deep note's name is often ambiguous alone
("Note 1"). So by default each ancestor folder is squeezed to its first
character in the tab — enough to place the note, cheap in pixels:

```
Pasd/Tasd/Note 1.md   →   P/T/Note 1 · My Blog
Modern AI.md          →   Modern AI · My Blog     (no ancestors: unchanged)
```

Only the ancestors shrink; the note keeps its full name. The nav and graph are
unaffected — there is room for full names there.

To show folders in full instead, set `"ancestorInitials": false` in the config,
or pass `--no-ancestor-initials` for a one-off build.

## Publication selection

`publish: false` always keeps a note private. In the normal mode, other notes
are visible unless folder rules select a narrower set. `--published-only` is
strict: only notes with `publish: true` receive routes.

```json
{
  "publication": {
    "includeFolders": ["blog"],
    "excludeFolders": ["blog/drafts"],
    "linkedNotes": true
  }
}
```

An explicit `publish: true` or `publish: false` overrides folder rules.
`linkedNotes` adds notes reached from the selected set, except explicitly
private notes. Referenced attachments follow the same closure; unreferenced
attachments are not copied.

## URLs

Every note's URL is derived from its file path, with each segment slugified:

```
index.md                         → /
Field Notes/Growing Tomatoes.md  → /field-notes/growing-tomatoes
Field Notes/index.md             → /field-notes          (index stands in for its folder)
```

Capitals, spaces, punctuation and non-Latin scripts in filenames are all fine.
A `slug:` in frontmatter is ignored. The filesystem path remains the note's
internal identity, while an optional full-path `permalink:` changes its public
route. The original path and every full-path entry in `aliases:` become static
HTML redirects:

```yaml
permalink: /garden/tomatoes
aliases:
  - /tomatoes
  - /old/growing-tomatoes
```

Duplicate routes and redirect loops fail the build. Generated wikilinks,
search results, graph nodes, previews, tags, and navigation all use the
canonical permalink.

## Blog metadata and discovery

Notes may set `date`, `updated`, `tags`, `author`, `status`, `navOrder`, and
`navHidden`. The site exposes chronological `/archive` and `/tags` pages plus
a URL-backed `/search` page. `navHidden` removes a page or folder subtree only
from navigation; it remains searchable, linkable, and present in the graph.

## Development

```bash
npm test              # unit tests (vitest)
npm run test:e2e      # end-to-end tests against a built site
npm run check         # astro check
npm run check:ts      # tsc --noEmit
npm run check:biome   # biome lint + format
```

Fixture vaults for the tests live in `tests/fixtures/`.
