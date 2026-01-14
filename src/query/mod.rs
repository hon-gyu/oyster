//! Query module for extracting structured data from Markdown documents.
//!
//! # Overview
//!
//! This module parses Markdown files and extracts:
//! - **Frontmatter**: YAML metadata at the start of the document
//! - **Sections**: Hierarchical tree of headings with their content
//!
//! # Output Format
//!
//! The output is a [`Markdown`] struct that can be serialized to JSON:
//! - Frontmatter is extracted as structured YAML with source location
//! - Sections form a tree rooted at level 0 (document root)
//! - Each section contains its heading, content, byte range, and children
//!
//! # Example
//!
//! ```ignore
//! let result = query_file(Path::new("doc.md"))?;
//! let json = serde_json::to_string_pretty(&result)?;
//! ```

use crate::ast::{Node, NodeKind, Tree};
use crate::hierarchy::{
    Hierarchical, HierarchicalWithDefaults, build_padded_tree,
};
use crate::link::extract_frontmatter;
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Serialize};
use serde_yaml::Value as YamlValue;

mod heading;
pub use heading::Heading;

/// Structured representation of a Markdown document.
///
/// Contains:
/// - `frontmatter`: Optional YAML metadata block with source location
/// - `sections`: Hierarchical tree of document sections
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Markdown {
    /// YAML frontmatter (if present at the start of the document)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub frontmatter: Option<Frontmatter>,
    /// Hierarchical section tree rooted at level 0
    pub sections: Section,
}

/// YAML frontmatter with source location information.
///
/// Fields:
/// - `content`: Parsed YAML value (can be any valid YAML structure)
/// - `start_byte`, `end_byte`: Byte range in the source file
/// - `start_point`, `end_point`: (row, column) positions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Frontmatter {
    /// Parsed YAML content
    pub content: YamlValue,
    /// Starting byte offset in source
    pub start_byte: usize,
    /// Ending byte offset in source
    pub end_byte: usize,
    /// Starting position as (row, column)
    pub start_point: (usize, usize),
    /// Ending position as (row, column)
    pub end_point: (usize, usize),
}

/// Internal wrapper for tree building that supports level 0 (document root).
///
/// This allows us to use `build_padded_tree` with a virtual root at level 0,
/// which captures content before the first heading.
#[derive(Debug, Clone)]
enum SectionHeading {
    /// Document root (level 0) - captures content before the first heading
    Root,
    /// A real Markdown heading (levels 1-6, i.e., # to ######)
    Heading(Heading),
}

impl Hierarchical for SectionHeading {
    fn level(&self) -> usize {
        match self {
            SectionHeading::Root => 0,
            SectionHeading::Heading(h) => h.level(),
        }
    }
}

impl HierarchicalWithDefaults for SectionHeading {
    fn default_at_level(level: usize, _index: Option<Vec<usize>>) -> Self {
        if level == 0 {
            SectionHeading::Root
        } else {
            SectionHeading::Heading(Heading {
                level: HeadingLevel::try_from(level)
                    .expect("Invalid arg: level should be in range 1..6"),
                text: String::new(),
                start_byte: 0,
                end_byte: 0,
                start_point: (0, 0),
                end_point: (0, 0),
                id: None,
                classes: Vec::new(),
                attrs: Vec::new(),
            })
        }
    }
}

/// A section of a Markdown document, representing a heading and its content.
///
/// # Structure
///
/// Sections form a tree structure:
/// - Root section (level 0): `heading` is `None`, contains preamble content
/// - Heading sections (levels 1-6): `heading` contains the [`Heading`] data
/// - Implicit sections: Created for level gaps (e.g., H1 → H3 creates implicit H2)
///
/// # Fields
///
/// - `heading`: The heading that starts this section (`None` for root)
/// - `index`: Hierarchical position (e.g., "0.1.2" means first H1's second H2's third H3)
/// - `content`: Text content between this heading and the next heading/child
/// - `start_byte`, `end_byte`: Byte range spanning the entire section
/// - `children`: Nested sections at deeper heading levels
///
/// # Index Format
///
/// The index uses dot notation where:
/// - "0" = document root
/// - "0.1" = first H1 under root
/// - "0.1.2" = second H2 under that H1
/// - "0.1.0" = implicit heading (index component is 0)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Section {
    /// The heading that starts this section.
    /// - `None` for the document root (level 0)
    /// - `Some(Heading)` for actual or implicit headings
    pub heading: Option<Heading>,
    /// Hierarchical index in dot notation (e.g., "0", "0.1", "0.1.2")
    pub index: String,
    /// Text content of this section, excluding child sections.
    /// Trimmed of leading/trailing whitespace.
    pub content: String,
    /// Starting byte offset of this section in the source
    pub start_byte: usize,
    /// Ending byte offset of this section in the source
    pub end_byte: usize,
    /// Child sections (headings at deeper levels)
    pub children: Vec<Section>,
}

impl std::fmt::Display for Section {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl Section {
    fn fmt_with_indent(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let prefix = "  ".repeat(indent);

        // Format heading title
        let title = match &self.heading {
            None => "(root)".to_string(),
            Some(h) if h.text.is_empty() => "(implicit)".to_string(),
            Some(h) => h.text.lines().next().unwrap_or("").to_string(),
        };

        // Print section header with index and byte range
        writeln!(
            f,
            "{}[{}] {} [{}..{}]",
            prefix, self.index, title, self.start_byte, self.end_byte
        )?;

        // Print content preview if non-empty
        if !self.content.is_empty() {
            let preview: String = self
                .content
                .chars()
                .take(50)
                .map(|c| if c == '\n' { ' ' } else { c })
                .collect();
            let ellipsis = if self.content.len() > 50 { "..." } else { "" };
            writeln!(f, "{}  content: \"{}{ellipsis}\"", prefix, preview)?;
        }

        // Print children
        for child in &self.children {
            child.fmt_with_indent(f, indent + 1)?;
        }

        Ok(())
    }
}

/// Build a hierarchical section tree from a document AST.
///
/// # Arguments
///
/// - `root`: The root node of the parsed Markdown AST
/// - `source`: The original source text (used for content extraction)
/// - `doc_start`: Byte offset where document content starts
///   - Pass `0` if there's no frontmatter
///   - Pass the frontmatter's `end_byte` to exclude it from root content
///
/// # Returns
///
/// A [`Section`] tree rooted at level 0, containing:
/// - Preamble content (text before the first heading)
/// - All headings organized hierarchically
/// - Implicit headings for level gaps (e.g., H1 → H3 creates implicit H2)
///
/// # Errors
///
/// Returns an error if heading extraction fails (e.g., invalid heading levels).
pub fn build_sections(
    root: &Node,
    source: &str,
    doc_start: usize,
) -> Result<Section, String> {
    // 1. Extract headings from AST
    let headings = extract_headings(root, source);

    // 2. Build tree with min_level = 0 for document root
    let tree = build_padded_tree(headings, Some(0))?;

    // 3. Convert to Section with content extraction
    let doc_end = source.len();
    Ok(hierarchy_to_section(&tree, source, doc_start, doc_end))
}

/// Query a Markdown file and extract its structured content.
///
/// This is the main entry point for parsing a Markdown file into
/// a [`Markdown`] struct containing frontmatter and sections.
///
/// # Arguments
///
/// - `path`: Path to the Markdown file to parse
///
/// # Returns
///
/// A [`Markdown`] struct containing:
/// - `frontmatter`: Parsed YAML metadata (if present) with source location
/// - `sections`: Hierarchical tree of document sections
///
/// # Errors
///
/// Returns an error if:
/// - The file cannot be read
/// - The file is empty
/// - Section building fails
///
/// # Example
///
/// ```ignore
/// let result = query_file(Path::new("README.md"))?;
/// println!("Title: {:?}", result.frontmatter);
/// println!("Sections: {}", result.sections);
/// ```
pub fn query_file(path: &std::path::Path) -> Result<Markdown, String> {
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    if source.is_empty() {
        return Err("File is empty".to_string());
    }

    let tree = Tree::new_with_default_opts(&source);

    // Extract frontmatter from the first child if it's a metadata block
    let first_child = tree.root_node.children.first();
    let (frontmatter, doc_start) = match first_child {
        Some(node) => {
            let fm_content = extract_frontmatter(node);
            match fm_content {
                Some(content) => {
                    let fm = Frontmatter {
                        content,
                        start_byte: node.start_byte,
                        end_byte: node.end_byte,
                        start_point: (
                            node.start_point.row,
                            node.start_point.column,
                        ),
                        end_point: (node.end_point.row, node.end_point.column),
                    };
                    (Some(fm), node.end_byte)
                }
                None => (None, 0),
            }
        }
        None => (None, 0),
    };

    // Build section tree, starting content after frontmatter
    let sections = build_sections(&tree.root_node, &source, doc_start)?;

    Ok(Markdown {
        frontmatter,
        sections,
    })
}

/// Extract all headings from the AST in document order
fn extract_headings(node: &Node, source: &str) -> Vec<SectionHeading> {
    let mut headings = Vec::new();
    extract_headings_recursive(node, source, &mut headings);
    headings
}

fn extract_headings_recursive(
    node: &Node,
    source: &str,
    headings: &mut Vec<SectionHeading>,
) {
    if let NodeKind::Heading {
        level,
        id,
        classes,
        attrs,
    } = &node.kind
    {
        // Extract heading text from source
        let text = source[node.start_byte..node.end_byte].to_string();
        headings.push(SectionHeading::Heading(Heading {
            level: *level,
            text,
            start_byte: node.start_byte,
            end_byte: node.end_byte,
            start_point: (node.start_point.row, node.start_point.column),
            end_point: (node.end_point.row, node.end_point.column),
            id: id.as_ref().map(|s| s.to_string()),
            classes: classes.iter().map(|s| s.to_string()).collect(),
            attrs: attrs
                .iter()
                .map(|(k, v)| {
                    (k.to_string(), v.as_ref().map(|s| s.to_string()))
                })
                .collect(),
        }));
    }

    for child in &node.children {
        extract_headings_recursive(child, source, headings);
    }
}

/// Convert HierarchyItem<SectionHeading> to Section
///
/// `doc_start` is passed through for the root section's content start.
fn hierarchy_to_section(
    item: &crate::hierarchy::HierarchyItem<SectionHeading>,
    source: &str,
    doc_start: usize,
    next_boundary: usize,
) -> Section {
    // Convert index to string (e.g., [0, 1, 2] -> "0.1.2")
    let index = item
        .index
        .as_ref()
        .map(|idx| {
            idx.iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(".")
        })
        .unwrap_or_default();

    // Get heading and section start byte
    // For root, content starts at doc_start (after frontmatter)
    let (heading, section_start, content_start) = match &item.value {
        SectionHeading::Root => (None, doc_start, doc_start),
        SectionHeading::Heading(h) => {
            (Some(h.clone()), h.start_byte, h.end_byte)
        }
    };

    // Calculate content end: first child's start, or next_boundary
    let content_end = if item.children.is_empty() {
        next_boundary
    } else {
        match &item.children[0].value {
            SectionHeading::Root => 0,
            SectionHeading::Heading(h) => h.start_byte,
        }
    };

    let content = if content_start < content_end {
        source[content_start..content_end].trim().to_string()
    } else {
        String::new()
    };

    // Convert children, calculating their next boundaries
    // doc_start is not used for children (only root uses it)
    let children: Vec<Section> = item
        .children
        .iter()
        .enumerate()
        .map(|(i, child)| {
            let child_next = if i + 1 < item.children.len() {
                match &item.children[i + 1].value {
                    SectionHeading::Root => 0,
                    SectionHeading::Heading(h) => h.start_byte,
                }
            } else {
                next_boundary
            };
            hierarchy_to_section(child, source, 0, child_next)
        })
        .collect();

    // Section end is the next_boundary (start of next sibling or end of parent)
    let section_end = next_boundary;

    Section {
        heading,
        index,
        content,
        start_byte: section_start,
        end_byte: section_end,
        children,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Tree;
    use insta::assert_snapshot;

    #[test]
    fn test_build_sections_basic() {
        let source = r#"# Title

Some intro content.

## Section A

Content of section A.

### Subsection A.1

Details here.

## Section B

Final thoughts.
"#;
        let tree = Tree::new(source, false);
        let sections = build_sections(&tree.root_node, source, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r#"
        [0] (root) [0..132]
          [0.1] # Title [0..132]
            content: "Some intro content."
            [0.1.1] ## Section A [30..102]
              content: "Content of section A."
              [0.1.1.1] ### Subsection A.1 [67..102]
                content: "Details here."
            [0.1.2] ## Section B [102..132]
              content: "Final thoughts."
        "#);
    }

    #[test]
    fn test_build_sections_with_preamble() {
        let source = r#"This is content before any heading.

# First Heading

Some content.
"#;
        let tree = Tree::new(source, false);
        let sections = build_sections(&tree.root_node, source, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r#"
        [0] (root) [0..68]
          content: "This is content before any heading."
          [0.1] # First Heading [37..68]
            content: "Some content."
        "#);
    }

    #[test]
    fn test_build_sections_with_frontmatter() {
        let source = r#"---
title: Test Document
---

Some preamble.

## Introduction

Intro content.
"#;
        let tree = Tree::new(source, false);
        let sections = build_sections(&tree.root_node, source, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r#"
        [0] (root) [0..78]
          [0.0] (implicit) [0..78]
            content: "--- title: Test Document ---  Some preamble."
            [0.0.1] ## Introduction [46..78]
              content: "Intro content."
        "#);
    }

    #[test]
    fn test_build_sections_gap_filling() {
        let source = r#"# Title

### Deep Section

Content.
"#;
        let tree = Tree::new(source, false);
        let sections = build_sections(&tree.root_node, source, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r##"
        [0] (root) [0..36]
          [0.1] # Title [0..36]
            [0.1.0] (implicit) [0..36]
              content: "# Title"
              [0.1.0.1] ### Deep Section [9..36]
                content: "Content."
        "##);
    }

    #[test]
    fn test_query_file() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let source = r#"---
title: Test Document
tags:
  - rust
  - markdown
---

Some preamble.

# Introduction

Intro content here.

## Details

More details.
"#;
        let mut file = NamedTempFile::new().unwrap();
        file.write_all(source.as_bytes()).unwrap();

        let result = query_file(file.path()).unwrap();
        let json = serde_json::to_string_pretty(&result).unwrap();
        assert_snapshot!(json, @r###"
        {
          "frontmatter": {
            "content": {
              "title": "Test Document",
              "tags": [
                "rust",
                "markdown"
              ]
            },
            "start_byte": 0,
            "end_byte": 56,
            "start_point": [
              0,
              0
            ],
            "end_point": [
              5,
              3
            ]
          },
          "sections": {
            "heading": null,
            "index": "0",
            "content": "Some preamble.",
            "start_byte": 56,
            "end_byte": 137,
            "children": [
              {
                "heading": {
                  "level": "H1",
                  "text": "# Introduction\n",
                  "start_byte": 74,
                  "end_byte": 89,
                  "start_point": [
                    9,
                    0
                  ],
                  "end_point": [
                    10,
                    0
                  ]
                },
                "index": "0.1",
                "content": "Intro content here.",
                "start_byte": 74,
                "end_byte": 137,
                "children": [
                  {
                    "heading": {
                      "level": "H2",
                      "text": "## Details\n",
                      "start_byte": 111,
                      "end_byte": 122,
                      "start_point": [
                        13,
                        0
                      ],
                      "end_point": [
                        14,
                        0
                      ]
                    },
                    "index": "0.1.1",
                    "content": "More details.",
                    "start_byte": 111,
                    "end_byte": 137,
                    "children": []
                  }
                ]
              }
            ]
          }
        }
        "###);
    }
}
