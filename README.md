# markdown-tools (WIP name)

Open-source [Obsidian.md](https://obsidian.md/)

## Progress
- ✅ Parse markdown using [pulldown-cmark](https://github.com/pulldown-cmark/pulldown-cmark/) and build a syntax tree
- ✅ [File / Note reference](https://help.obsidian.md/links#Link%20to%20a%20file)
- ✅ [Heading reference](https://help.obsidian.md/links#Link%20to%20a%20heading%20in%20a%20note)
- ✅ TOC generation
- ✅ [Block reference](https://help.obsidian.md/links#Link%20to%20a%20block%20in%20a%20note)
- 🚧 SSG
  - ✅ v0: minijina + pulldown-cmarks's html writer; backlinks component; correct links
  - ✅ v1: type-safe ast-based html writer; backlinks component;
  - 🚧 more components
    - TOC
    - Explorer
    - Graphview
- ⬜ query CLI
  - ⬜ fronmatter 
- ⬜ CodeGen
- ⬜ Markdown to structured data (YAML / JSON)
  - ⬜ CHANGELOG validation
- ⬜ Obsidian [base](https://help.obsidian.md/bases)
- ⬜ LSP (inspired by [markdown-oxide](https://github.com/Feel-ix-343/markdown-oxide))

### Long-term
- UI ([neovim](https://neovim.io/)? [Zed](https://zed.dev/) fork?)
- Bi-directional sync with 
  - Github issues / PRs
  - Github Wiki
  - Linear

## Ideas & Explorations 
- Vault as some sort of database
