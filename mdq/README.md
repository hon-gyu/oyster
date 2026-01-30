# mdq - Markdown Query Tool

A command-line tool for querying and extracting data from Markdown files. Syntax inspired by jq.

## Installation

TBA

## Usage

```bash
mdq <file> [OPTIONS]
```

Show table-of-content summary:
```bash
mdq document.md --summary
```

TIP: use `--summary` to get a quick overview of the document, then iteratively refine your query.

Extract frontmatter:
```bash
mdq document.md --frontmatter
```

Select a section by title:
```bash
mdq document.md --field "Introduction"
```

Get the number of child sections:
```bash
mdq document.md --nchildren
```

Chain multiple operations:
```bash
mdq document.md --field "Introduction" --nchildren
```

Extract code blocks:
```bash
mdq document.md --code 0  # Get first code block
```

## Output Formats

- `--format json` - JSON output
- `--format markdown` - Markdown output
- `--format summary` - Tree summary

## Available Operations

- `--field <title>` - Select section by title
- `--index <n>` - Select child section by index
- `--slice <start:end>` - Select slice of child sections
- `--title <n>` - Get title of child section by index
- `--summary` - Output summary tree
- `--nchildren` - Count child sections
- `--frontmatter` - Extract frontmatter
- `--body` - Strip frontmatter
- `--preface` - Extract content before first section
- `--has <title>` - Check if section exists
- `--delete <title>` - Delete section by title
- `--inc <n>` - Increment heading levels
- `--dec <n>` - Decrement heading levels
- `--code <n>` - Extract Nth code block
- `--codemeta <n>` - Extract Nth code block metadata
