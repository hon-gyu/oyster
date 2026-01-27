# OysterMD Architecture Document

## Project Overview

**oyster** is a Rust-based static site generator (SSG) for Obsidian.md vaults. It transforms markdown files with Obsidian-specific features (wikilinks, block references, embeds, etc.) into a fully-featured static HTML website with inter-note linking, table of contents, file explorer, and LaTeX support.

## High-Level Architecture

```
Input (Markdown Vault)
        ↓
┌─────────────────────────────────────┐
│  Vault Scanner & Link Extraction    │
│  (link/extract.rs)                  │
└─────────────────────────────────────┘
        ↓
    ┌───────────────────────────────────────┐
    │ Link Resolution                       │
    │ (link/resolve.rs)                     │
    │ - Match wiki links to files/headings  │
    │ - Resolve block references           │
    └───────────────────────────────────────┘
        ↓
    ┌───────────────────────────────────────┐
    │ AST Building & Processing             │
    │ (ast/tree.rs, ast/node.rs)            │
    │ - Parse markdown with pulldown-cmark  │
    │ - Build Markdown AST                  │
    └───────────────────────────────────────┘
        ↓
    ┌───────────────────────────────────────┐
    │ Content Rendering                     │
    │ (export/content.rs)                   │
    │ - Traverse AST                        │
    │ - Generate HTML with links, images    │
    │ - Render LaTeX with KaTeX             │
    └───────────────────────────────────────┘
        ↓
    ┌───────────────────────────────────────┐
    │ Page Assembly                         │
    │ (export/writer.rs)                    │
    │ - Build TOC                           │
    │ - Build backlinks                     │
    │ - Assemble frontmatter                │
    │ - Create page layout                  │
    └───────────────────────────────────────┘
        ↓
Output (HTML Static Site)
```

## Module Breakdown

### 1. Entry Point: `main.rs`

The CLI interface using `clap` for argument parsing.

**Command:**
```bash
oyster generate <vault_root_dir> --output <output_dir> [--theme default|dracula|gruvbox|tokyonight] [--no-filter-publish]
```

**Key Responsibilities:**
- Parse command-line arguments
- Invoke `render_vault()` from the export module
- Handle errors and display output

**Parameters:**
- `vault_root_dir`: Source markdown vault directory
- `output`: Output directory for generated HTML
- `theme`: Visual theme selection (default: "default")
- `no_filter_publish`: Whether to include unpublished notes

---

### 2. Link Module: `link/`

Handles the extraction, representation, and resolution of references in markdown files.

#### `link/types.rs` - Type Definitions

**Core Enums:**

```rust
pub enum Referenceable {
    Asset { path: PathBuf },
    Note { path: PathBuf, children: Vec<Referenceable> },
    Heading { path: PathBuf, level: HeadingLevel, text: String, range: Range<usize> },
    Block { path: PathBuf, identifier: String, kind: BlockReferenceableKind, range: Range<usize> },
}

pub enum ReferenceKind {
    WikiLink,      // [[file#heading]]
    MarkdownLink,  // [text](file#heading)
    Embed,         // ![[file]]
}

pub struct Reference {
    pub path: PathBuf,              // Source file
    pub range: Range<usize>,        // Byte range in source
    pub dest: String,               // Destination (file, heading, block id)
    pub kind: ReferenceKind,
    pub display_text: String,
}

pub struct Link {
    pub from: Referenceable,        // Source (reference + location)
    pub to: Referenceable,          // Target (what it points to)
}
```

**Referenceable Types:**
- **Asset**: Static files (images, PDFs, videos) that can be linked
- **Note**: Markdown files in the vault
- **Heading**: H1-H6 headings within notes (in-note referenceables)
- **Block**: Paragraphs, lists, blockquotes, tables with identifiers (in-note referenceables)

**Block Identifier Kinds:**
- `InlineParagraph`: Single-line paragraph
- `InlineListItem`: Inline list item
- `Paragraph`: Multi-line paragraph
- `List`: Ordered/unordered list
- `BlockQuote`: Block quote
- `Table`: Markdown table

#### `link/extract.rs` - Vault Scanning and Reference Extraction

**Main Functions:**

1. **`scan_vault(dir, root_dir, filter_publish)`**
   - Recursively scan vault directory
   - Return: `(frontmatters, referenceables, references)`
   - Filters by `publish: true` in frontmatter if `filter_publish=true`
   - Separates file-level referenceables (notes, assets) from in-note referenceables

2. **`scan_note(path)`**
   - Parse single markdown file
   - Extract frontmatter (YAML)
   - Extract references and in-note referenceables
   - Return: `(frontmatter_option, references, in_note_referenceables)`

3. **`scan_dir_for_assets_and_notes(dir)`**
   - Recursively find `.md`/`.markdown` files and asset files
   - Build initial referenceable list

**Reference Extraction Logic:**
- Scan AST nodes for `Link` and `Image` nodes
- Distinguish wiki links (`[[...]]`) from markdown links (`[...](...)`)
- For wikilinks with block identifiers (e.g., `[[note#^blockid]]`), extract the identifier
- Track byte ranges for later anchor ID generation

**In-Note Referenceable Extraction:**
- Headings: Extract text and heading level
- Blocks with identifiers: Look for `^identifier` syntax
- Store exact byte ranges for link resolution and anchor generation

#### `link/resolve.rs` - Link Resolution

**Main Function: `build_links(references, referenceables)`**

Matches extracted references to their target referenceables.

**Resolution Logic:**

1. **Destination Parsing**: Split reference destination (e.g., `"Note#Section#Subsection"`)
   - File name: `"Note"`
   - Nested headings: `["Section", "Subsection"]`
   - Block identifier: `"blockid"` (marked with `^`)

2. **File Matching**: Fuzzy subsequence matching
   - `["Note"]` can match file `sub/folders/My Note.md`
   - Uses `match_subsequence()` for ancestor-descendant path matching

3. **Heading Matching**: Nested heading resolution
   - Match sequence of heading texts in order
   - Handle heading nesting (H2 under H1)

4. **Block Matching**: Identifier-based resolution
   - Find block with matching `^identifier`

5. **Unresolved References**: Track for debugging
   - Links with no matching target

**Output:** Vector of `Link { from: Referenceable, to: Referenceable }`

---

### 3. AST Module: `ast/`

Abstract syntax tree building and node representation.

#### `ast/node.rs` - Node Definition

```rust
pub enum NodeKind<'a> {
    // Container nodes
    Document,
    Paragraph,
    Heading { level, id, classes, attrs },
    BlockQuote(Option<BlockQuoteKind>),
    CodeBlock(CodeBlockKind),
    List(Option<u64>),       // None = unordered, Some(n) = ordered starting at n
    Item,
    Table(Vec<Alignment>),
    TableHead, TableRow, TableCell,
    
    // Inline styling
    Emphasis, Strong, Strikethrough, Superscript, Subscript,
    
    // Links and embeds
    Link { link_type, dest_url, title, id },
    Image { link_type, dest_url, title, id },
    
    // Inline content
    Text(CowStr),
    Code(CowStr),
    InlineMath(CowStr),
    DisplayMath(CowStr),
    Html(CowStr), InlineHtml(CowStr),
    
    // Metadata and special
    MetadataBlock(MetadataBlockKind),  // YAML frontmatter
    FootnoteDefinition(CowStr),
    FootnoteReference(CowStr),
    
    // Lists and definitions
    DefinitionList, DefinitionListTitle, DefinitionListDefinition,
    
    // Other
    SoftBreak, HardBreak, Rule,
    TaskListMarker(bool),  // true = checked, false = unchecked
}

pub struct Node<'a> {
    pub kind: NodeKind<'a>,
    pub start_byte: usize,
    pub end_byte: usize,
    pub children: Vec<Node<'a>>,
    pub parent: Option<*const Node<'a>>,  // Unsafe pointer for tree navigation
}
```

#### `ast/tree.rs` - Tree Construction

**Main Struct: `Tree<'a>`**
```rust
pub struct Tree<'a> {
    pub root_node: Node<'a>,
    pub opts: Options,
}
```

**Construction Process:**

1. **Parsing**: Use `pulldown_cmark::Parser::new_ext()` with enabled options:
   - Tables, footnotes, strikethrough, task lists
   - Smart punctuation, YAML metadata blocks
   - Math (inline and display), GFM, definition lists
   - Superscript, subscript, wikilinks

2. **Event Collection**: Iterate parser to collect `(Event, Range<usize>)`
   - Events: `Start(Tag)`, `End(Tag)`, and leaf events

3. **Tree Building**: `build_ast()` function
   - Maintain stack of nodes representing tree depth
   - When encountering `Start(tag)`, push new node
   - When encountering `End(tag)`, pop and add children
   - Track byte offsets for each node

4. **Parent Pointer Setup**: `setup_parent_pointers()`
   - Link each node to its parent for tree traversal

**Key Features:**
- Zero-copy string references using `CowStr`
- Precise byte ranges for matching with references
- Full tree structure for recursive rendering

---

### 4. Export Module: `export/`

HTML generation and page assembly.

#### `export/writer.rs` - Main Rendering Orchestrator

**Entry Point: `render_vault(vault_root_dir, output_dir, theme, filter_publish)`**

**Processing Pipeline:**

```
1. Scan Vault
   └─ scan_vault() → (frontmatters, referenceables, references)

2. Build Link Maps
   └─ build_links() → resolved links
   └─ build_vault_paths_to_slug_map() → path → slug mapping
   └─ build_in_note_anchor_id_map() → path → range → anchor_id

3. Setup Output
   ├─ Create output directory
   ├─ setup_styles() → Copy CSS files
   └─ Copy matched asset files to output

4. Render Pages
   ├─ For each note:
   │  ├─ Parse markdown → AST
   │  ├─ render_content() → HTML
   │  ├─ render_toc() → Table of contents
   │  ├─ render_backlinks() → Incoming links
   │  ├─ Assemble page with layout
   │  └─ Write HTML file
   │
   └─ render_home_page() → Homepage with file tree

5. Copy Assets
   └─ cp_katex_assets() → Copy KaTeX math rendering assets
```

**Key Data Structures:**

```rust
vault_path_to_slug_map: HashMap<PathBuf, String>
  // Maps: /vault/notes/My Note.md → note/my-note.html

vault_path_to_title_map: HashMap<PathBuf, String>
  // Maps: /vault/notes/My Note.md → "My Custom Title" (from frontmatter)

vault_path_to_frontmatter_map: HashMap<&Path, Option<Y>>
  // Maps: /vault/notes/My Note.md → YAML frontmatter

innote_refable_anchor_id_map: HashMap<PathBuf, HashMap<Range<usize>, String>>
  // Maps: 
  // /vault/notes/My Note.md → {
  //   42..55 → "42-55",  // Heading or block anchor
  //   ...
  // }
```

#### `export/content.rs` - AST to HTML Rendering

**Entry Function: `render_content(tree, vault_path, resolved_links, vault_path_to_slug_map, innote_refable_anchor_id_map)`**

**Process:**
1. Build `ref_dest_map`: Maps reference byte range → resolved destination URL
2. Build `in_note_anchor_id_map`: Maps referable byte range → anchor ID
3. Recursively render AST nodes

**Node Rendering (`render_node()`):**

Each node type converts to appropriate HTML:

- **Container Nodes**: Recursively render children
  - Document → `<article>`
  - Paragraph → `<p>` (with optional anchor ID)
  - Heading → `<h1>-<h6>` (always has anchor ID)
  - List/Item → `<ul>/<ol>` and `<li>`
  - BlockQuote → `<blockquote>` with optional alert class
  - Table → `<table>`, `<thead>`, `<tbody>`, `<tr>`, `<td>`

- **Link/Embed Nodes**: Apply link resolution
  - Links → `<a>` with resolved href
    - Internal links wrap in `<span class="internal-link" id="...">` for styling
    - Unresolved links → `<span class="internal-link unresolved">`
  - Images/Embeds:
    - If image (`.png`, `.jpg`, `.jpeg`) → `<img>`
    - Other files → `<span class="embed-file">`
    - Parse size from text: `![[image.png|100x200]]` → width/height attributes

- **Inline Styling**: Text formatting
  - Strong → `<strong>`
  - Emphasis → `<em>`
  - Code → `<code>`
  - Strikethrough → `<s>`
  - Superscript/Subscript → `<sup>/<sub>`

- **Math**: LaTeX rendering
  - `InlineMath(latex)` → `render_latex(latex, false)`
  - `DisplayMath(latex)` → `render_latex(latex, true)`

- **Text Nodes**: Raw text content

#### `export/latex.rs` - LaTeX Rendering

**Function: `render_latex(latex, display_mode)`**

- Uses **KaTeX** library for math rendering
- Converts LaTeX strings to HTML
- `display_mode=true` → centered block math
- `display_mode=false` → inline math
- Error handling: Falls back to raw LaTeX in `<span class="math-error">`

**KaTeX Assets:** Copied to `output/katex/` with CSS for styling

#### `export/toc.rs` - Table of Contents Generation

**Function: `render_toc(vault_path, referenceables, innote_refable_anchor_id_map)`**

- Extracts all headings for a note
- Organizes hierarchically using `build_tree()`
- Renders as nested `<ul>` lists with links to anchor IDs
- Returns `Option<Markup>` (None if no headings)

#### `export/sidebar.rs` - File Explorer

**Function: `render_explorer(vault_slug, referenceables, vault_path_to_slug_map)`**

- Renders file tree in left sidebar
- Uses `build_file_tree()` from hierarchy module
- Shows folder structure with expandable `<details>` elements
- Links to individual notes

#### `export/file_tree_component.rs` - File Tree Rendering

**Function: `render_file_tree(vault_slug, referenceables, vault_path_to_slug_map)`**

Renders ASCII-style tree with Unicode connectors:
```
.
├── folder1/
│   ├── note1.md
│   └── note2.md
└── folder2/
    └── note3.md
```

#### `export/home.rs` - Homepage Generation

**Functions:**
- `render_simple_home_back_nav()`: Home link in breadcrumb
- `render_breadcrumb()`: Folder path breadcrumb
- `render_home_page()`: Full homepage with file tree

#### `export/frontmatter.rs` - YAML Frontmatter Rendering

**Function: `render_frontmatter(value)`**

Converts YAML metadata to HTML table:
- Special handling for `tags` (rendered as tag list)
- Special handling for `date` (rendered as `<time>` element)
- Recursive rendering for nested structures
- Ignores: `title`, `publish`, `draft` fields

#### `export/style.rs` - CSS Management

**Functions:**

1. **`setup_styles(output_dir, theme)`**
   - Create `output/styles/` directory
   - Copy base CSS (embedded in binary)
   - Copy theme CSS (dracula, gruvbox, tokyonight, or custom)

2. **`get_style_paths(page_path, output_dir, theme)`**
   - Calculate relative CSS paths from page location
   - Return paths for `<link rel="stylesheet">`

**CSS Files:**
```
src/export/styles/
├── base.css              # Structural styles (embedded)
└── themes/
    ├── dracula.css       # Dark theme (embedded)
    ├── gruvbox.css       # Retro groove theme (embedded)
    └── tokyonight.css    # Tokyo night theme (embedded)
```

Each theme CSS provides color variables and component-specific styling.

#### `export/utils.rs` - Utility Functions

**Path and URL Functions:**
- `slugify(s)`: Convert string to URL slug (lowercase, hyphens)
- `file_name_to_slug(path)`: Strip `.md` and slugify
- `file_path_to_slug(path)`: Full path → relative slug with HTML extension
- `build_vault_paths_to_slug_map(paths)`: Bulk slug generation with collision handling
- `get_relative_dest(from_path, to_path)`: Relative URL calculation

**Anchor ID Functions:**
- `text_to_anchor_id(text)`: Heading text → anchor ID
- `range_to_anchor_id(range)`: Byte range → anchor ID (format: `{start}-{end}`)

**Referenceable Mapping:**
- `build_in_note_anchor_id_map(referenceables)`: Create anchor ID mappings for all headings/blocks

**Utility Functions:**
- `parse_resize_spec(spec)`: Parse image size syntax `[[img.png|100x200]]` → (width, height)

---

### 5. Hierarchy Module: `hierarchy.rs`

Generic tree building utilities for hierarchical structures.

**Core Types:**

```rust
pub trait Hierarchical {
    fn level(&self) -> usize;
}

pub struct TreeNode<T> {
    pub value: T,
    pub children: Vec<TreeNode<T>>,
}
```

**Main Functions:**

1. **`build_tree<T: Hierarchical>(items: Vec<T>) -> Vec<TreeNode<T>>`**
   - Convert flat list of hierarchical items to tree
   - Example: headings [H1, H2, H3, H2] → tree with nesting
   - Uses stack-based algorithm

2. **File Tree Operations:**
   - `FileTreeItem`: Represents file or folder
   - `build_file_tree()`: Creates tree from referenceables
   - Used by sidebar and home page rendering

---

### 6. Parse Module: `parse.rs`

Markdown parsing configuration.

**Function: `default_opts() -> Options`**

Returns `pulldown_cmark::Options` with features enabled:
- `ENABLE_TABLES`
- `ENABLE_FOOTNOTES`
- `ENABLE_STRIKETHROUGH`
- `ENABLE_TASKLISTS`
- `ENABLE_SMART_PUNCTUATION`
- `ENABLE_HEADING_ATTRIBUTES`
- `ENABLE_YAML_STYLE_METADATA_BLOCKS`
- `ENABLE_MATH`
- `ENABLE_GFM`
- `ENABLE_DEFINITION_LIST`
- `ENABLE_SUPERSCRIPT`
- `ENABLE_SUBSCRIPT`
- `ENABLE_WIKILINKS`

---

### 7. Utility Modules

#### `value.rs` - Generic Value Type

Simple enum for serialization:
```rust
pub enum Value {
    Null,
    String(String),
    A(Vec<String>),           // Array
    O(Vec<(String, Value)>),  // Object/Map
}
```

#### `validate.rs` - Validation Framework

Borrowed from catlog project; provides `Validate` trait for composable validation with typed errors.

#### `snapshots.rs`

Testing snapshot utilities (likely using `insta` crate).

---

## Data Flow: Input to Output

### 1. Input: Markdown Vault Structure
```
vault/
├── notes/
│   ├── Note 1.md
│   ├── Note 2.md
│   └── folder/
│       └── Note 3.md
├── assets/
│   ├── image.png
│   └── diagram.svg
└── private/
    └── draft.md (publish: false)
```

### 2. Scanning Phase

```
scan_vault(vault_root)
├─ scan_dir_for_assets_and_notes()
│  └─ Find all .md files and assets → Referenceable::Note/Asset
│
├─ For each Note:
│  └─ scan_note(path)
│     ├─ Parse markdown → Tree
│     ├─ Extract frontmatter (YAML)
│     ├─ Extract references (wikilinks, markdown links)
│     └─ Extract in-note referenceables (headings, blocks)
│
└─ Filter by publish: true (if filter_publish=true)
```

**Output:** 
- `referenceables`: Vec of all notes (with in-note children), assets
- `references`: Vec of all links found
- `fms`: Vec of YAML frontmatters

### 3. Link Resolution Phase

```
build_links(references, referenceables)
├─ For each Reference:
│  └─ split_dest_string() → (file, nested_headings, block_id)
│     ├─ Match file name (subsequence matching)
│     ├─ Match heading path (nested heading sequence)
│     └─ Match block identifier
│
└─ Create Link { from: Reference's location, to: matched Referenceable }
```

**Output:** `links`: Vec of resolved Link objects

### 4. Slug Generation Phase

```
build_vault_paths_to_slug_map(file_paths)
├─ For each file path:
│  ├─ file_path_to_slug() 
│  │  ├─ Strip .md extension
│  │  ├─ Slugify (lowercase, replace special chars)
│  │  └─ Add .html extension
│  │
│  └─ Handle collisions (append -1, -2, etc.)
```

**Output:** `vault_path_to_slug_map`: Maps file paths to output slugs

### 5. Anchor ID Generation Phase

```
build_in_note_anchor_id_map(referenceables)
├─ For each note:
│  ├─ For each heading:
│  │  └─ anchor_id = range_to_anchor_id(byte_range)  // "42-55"
│  │
│  └─ For each block with identifier:
│     └─ anchor_id = range_to_anchor_id(byte_range)
```

**Output:** `innote_refable_anchor_id_map`: Maps (file, byte_range) → anchor_id

### 6. Content Rendering Phase

```
For each Note:
├─ Parse markdown → AST (Tree)
├─ render_content(tree, ...)
│  ├─ Traverse AST recursively
│  ├─ For each node:
│  │  ├─ Lookup in ref_dest_map if it's a link
│  │  ├─ Lookup in refable_anchor_id_map if it's referenceable
│  │  └─ Generate appropriate HTML
│  │
│  └─ Return HTML string
│
├─ render_toc() → extract headings, generate nested list
├─ render_backlinks() → find incoming links, generate list
└─ render_page() → assemble HTML with layout
```

### 7. Page Assembly Phase

```
render_page(
  title, frontmatter, toc, content, backlinks,
  home_nav, sidebar, css_paths, katex_css
)
├─ DOCTYPE declaration
├─ <head>
│  ├─ Meta tags (charset, viewport)
│  ├─ KaTeX CSS
│  ├─ Theme CSS links
│  └─ <title>
│
├─ <body>
│  ├─ <aside class="left-sidebar">
│  │  └─ (future: sidebar content)
│  │
│  ├─ <main class="main-content">
│  │  ├─ Home navigation breadcrumb
│  │  ├─ Article with content
│  │  ├─ Optional: frontmatter table
│  │  ├─ Optional: TOC (table of contents)
│  │  └─ Optional: backlinks section
│  │
│  └─ <aside class="right-sidebar">
│     └─ File explorer tree
│
└─ Write to output_dir/slug.html
```

### 8. Asset Copying Phase

```
For each Asset in links.to:
├─ Copy from vault_root/asset_path
└─ to output_dir/slug_path
```

### 9. Homepage Generation

```
render_home_page(referenceables, vault_path_to_slug_map, home_slug)
├─ render_file_tree() → ASCII tree with folder structure
└─ List all notes with links
```

### 10. Output Structure

```
output/
├── home.html                    # Homepage
├── styles/
│  ├── base.css                 # Structural styles
│  └── themes/
│     └── {theme}.css           # Color theme
├── katex/
│  ├── katex.min.css           # Math rendering styles
│  ├── katex.min.js            # Math rendering engine
│  └── fonts/                  # Font files
├── notes/
│  ├── note-1.html
│  ├── note-2.html
│  └── folder/
│     └── note-3.html
└── assets/
   ├── image.png
   └── diagram.svg
```

---

## Key Design Decisions

### 1. **Byte-Range-Based Linking**
Instead of regenerating heading/block anchors, oyster uses the exact byte ranges from the parser. This ensures:
- Unique, deterministic anchor IDs
- Direct correlation between reference byte range and target anchor range
- Robustness against changes in heading text

### 2. **Unsafe Parent Pointers**
The AST uses `*const Node` parent pointers for optional tree navigation:
- Avoids Rc/RefCell overhead
- Safe because pointers are only dereferenced within the lifetime of the tree
- Parent navigation is optional; most code doesn't use it

### 3. **CowStr for Strings**
Uses `pulldown_cmark::CowStr` (copy-on-write):
- Avoids unnecessary allocations for borrowed text
- Most text is borrowed from the original markdown source
- Only cloned when needed (e.g., for storage outside parsing scope)

### 4. **Subsequence Matching for File Resolution**
Wikilinks like `[[My Note]]` can match files at any depth:
- User types `[[My Note]]`
- Matches `vault/folder/subfolder/My Note.md`
- Based on path component subsequence, not full path match

### 5. **Separated Rendering Phases**
Distinct phases (scan → resolve → render) enable:
- Building global link maps before rendering any page
- Consistent slug generation across all pages
- Accurate backlink calculations
- Efficient asset copying

### 6. **Embedded CSS/Assets**
CSS, theme files, and KaTeX assets are embedded in the binary using `include_str!()`:
- No external file dependencies at runtime
- Single portable binary
- Consistent styling across deployments

### 7. **Frontmatter Filtering**
The `publish: true` field in YAML frontmatter:
- Enables draft mode (exclude unpublished notes)
- Can be disabled with `--no-filter-publish` flag
- Common in static site generators (Jekyll, Hugo)

---

## Customization Points

### 1. **Themes**
Add new CSS in `src/export/styles/themes/{name}.css` and update `get_theme_css()` in `style.rs`.

### 2. **Template/Layout**
The page layout is hardcoded in `render_page()` using the `maud` templating DSL:
- Change sidebar content
- Modify header/footer
- Adjust main content area structure

### 3. **Asset Handling**
Current image extensions: `["png", "jpg", "jpeg"]` (see `export/content.rs`)
- Modify to support more formats
- Add video/audio embed handling

### 4. **Link Resolution**
The fuzzy matching in `link/resolve.rs`:
- Could be made stricter (exact path matching)
- Could support regular expressions
- Currently uses subsequence matching

### 5. **Frontmatter Fields**
Special handling in `export/frontmatter.rs`:
- Currently: `tags`, `date`, `title` have special rendering
- Can add more custom fields

---

## Dependencies

**Key Libraries:**

- **`pulldown-cmark`**: Markdown parsing and HTML generation
- **`maud`**: Type-safe HTML templating (DSL-based)
- **`katex`**: LaTeX math rendering (via WebAssembly)
- **`tree-sitter`**: Tree structure utilities (partial use)
- **`serde` + `serde_yaml`**: YAML frontmatter parsing
- **`clap`**: Command-line argument parsing
- **`url`**: URL parsing and validation
- **`itertools`, `similar`**: Utility algorithms
- **`ego-tree`**: Tree data structure (potential future use)

---

## Testing

**Test Framework:** `insta` (snapshot testing)

**Test Coverage:**
- `tests/test_parse.rs`: AST parsing
- `tests/test_latex.rs`: LaTeX rendering
- `tests/test_extract_reference.rs`: Reference extraction
- `tests/test_embed_files.rs`: File embedding
- `tests/test_utils.rs`: Utility functions

---

## Performance Characteristics

**Complexity Analysis:**
- **Scanning**: O(N) files + O(M) references
- **Link Resolution**: O(M × K) where K = # of referenceable candidates per reference
- **Rendering**: O(N × A) where A = average AST nodes per note
- **Slug Generation**: O(N log N) due to collision handling

**Memory:**
- Entire vault ASTs in memory during processing
- Large vaults (1000+ notes) should still be manageable
- Assets are copied, not buffered

---

## Conclusion

oyster uses a modular, multi-phase architecture to transform Obsidian vaults into linked HTML sites. The separation of scanning, link resolution, and rendering enables global optimization and accurate cross-note linking. The use of type-safe templating (maud) and proper string handling (CowStr) ensures correctness and efficiency.
