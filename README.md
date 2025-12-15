# OysterMD

OysterMD is open-source alternative to [Obsidian.md](https://obsidian.md/), a local-first knowledge management system based on plain markdown files with bidirectional linking to build an interconnected knowledge graph. 

- AST generation for CommonMark specification, Github-flavored Markdown, and Obsidian-specific extensions[^1].
- Static site generator: publish vault to HTML (alternative to [Obsidian Publish](https://obsidian.md/publish))
- Optional extensions: Mermaid diagrams, TikZ, Quiver, HTML embeds
- More to come: markdown transclusion, jq-like query language for markdown, UI, LSP...

[^1]: wikilinks, embeded notes, callouts

## Usage

### Installation

Build from source:
```bash
cargo build --release
```

The binary will be available at `./target/release/oyster`.

### Generate Static Site

Generate a static website from your Obsidian vault:

```bash
oyster generate --output <OUTPUT_DIR> <VAULT_ROOT_DIR>
```

**Available options:**
- `-o, --output <OUTPUT>` - Output directory for the generated site (required)
- `-t, --theme <THEME>` - Theme to use (available themes: `dracula`, `tokyonight`, and `gruvbox`; default: `tokyonight`)
- `-f, --filter-publish` - Whether to only export notes with the publish flag set in the frontmatter
- `-p, --preserve-softbreak` - Render softbreaks as line breaks
- `-m, --mermaid-render-mode <MODE>` - Options for rendering [mermaid](https://mermaid.js.org/) diagrams:
  - `build-time`: Use `mmdc` to render at build time (default)
  - `client-side`: Use mermaid.js in the browser
- `--tikz-render-mode <MODE>` - Options for rendering TikZ diagrams:
  - `build-time`: Use `latex2pdf` and `pdf2svg` (default)
  - `client-side`: Use TikZTeX in browser
- `--quiver-render-mode <MODE>` - Options for rendering [Quiver](https://github.com/varkor/quiver) diagrams:
  - `build-time`: Use `latex2pdf` and `pdf2svg` (default)
  - `raw-latex`: Keep raw LaTeX
- `--custom-callout-css <FILE>` - Path to custom CSS file for callout customization

**Example with options:**
```bash
oyster generate \
  --output ./dist \
  --theme default \
  --mermaid-render-mode client-side \
  ./my-vault
```

### Local Development

Test your generated site locally with:
```bash
python3 -m http.server 8000 --directory ./output/site
```

Then open http://localhost:8000 in your browser.

## Progress (⬜ | 🚧 | ✅)
- ✅ Parse markdown using [pulldown-cmark](https://github.com/pulldown-cmark/pulldown-cmark/) and build a syntax tree
- ✅ [File / Note reference](https://help.obsidian.md/links#Link%20to%20a%20file)
- ✅ [Heading reference](https://help.obsidian.md/links#Link%20to%20a%20heading%20in%20a%20note)
- ✅ TOC generation
- ✅ [Block reference](https://help.obsidian.md/links#Link%20to%20a%20block%20in%20a%20note)
- SSG
  - ✅ v0: minijina + pulldown-cmarks's html writer; backlinks component; correct links
  - ✅ v1: type-safe ast-based html writer ([maud](https://maud.lambda.xyz/)); backlinks component;
  - ✅ more components
    - ✅ TOC
    - ✅ Explorer
    - ✅ Homepage
    - ✅ Sidebar explorer
  - and more components
    - ⬜ Graphview
  - ✅ filter by frontmatter
  - ⬜ Tag page
- Embed image
  - ✅ basic embed
  - ✅ resize embed
- ✅ Embed files: 
  - ✅ note, heading, and block 
  - pdf, video, audio
  - ✅ HTML
  - ✅ HTML transclusion
  - 🚧 Markdown transclusion
- LaTeX support
  - ✅ basic support (KaTeX)
  - ✅ TikZ; Quiver (extension)
- ✅ Mermaid diagram
- ⬜ [Bases](https://help.obsidian.md/bases)
- ✅ Callout
- ✅ Custom callout
- ⬜ CodeGen
- ⬜ Markdown to structured data (YAML / JSON)
  - ⬜ CHANGELOG validation
  - ⬜ query CLI
    - ⬜ filter by frontmatter 
    - ⬜ table to csv
- ⬜ LSP (inspired by [markdown-oxide](https://github.com/Feel-ix-343/markdown-oxide))

### Long-term
- UI ([neovim](https://neovim.io/)? [Zed](https://zed.dev/) fork?)
- Bi-directional sync with 
  - Github issues / PRs
  - Github Wiki
  - Linear

## Ideas & Explorations 
- Vault as some sort of database
- CRDT
