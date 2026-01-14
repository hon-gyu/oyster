//! Query module for extracting structured data from Markdown documents.
//!
//! We build AST first, then trim the AST to the things of our interest.
//! - Frontmatter
//! - Headings -> Sections
//!
//! All the query logic are handled by `jq`
use crate::ast::{Node, NodeKind};
use crate::hierarchy::{
    Hierarchical, HierarchicalWithDefaults, build_padded_tree,
};
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Serialize};
use serde_yaml::Value as YamlValue;

use crate::ast::Tree;
use crate::link::extract_frontmatter;
mod heading;
pub use heading::Heading;

/// Result of querying a markdown document
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Markdown {
    /// YAML frontmatter (if present)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub frontmatter: Option<Frontmatter>,
    /// Hierarchical section tree
    pub sections: Section,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Frontmatter {
    pub content: YamlValue,
    pub start_byte: usize,
    pub end_byte: usize,
    /// column and row
    pub start_point: (usize, usize),
    /// column and row
    pub end_point: (usize, usize),
}

// Wrapper type for tree building that supports level 0 (document root)
#[derive(Debug, Clone)]
enum SectionHeading {
    /// Document root (level 0) - content before the first heading
    Root,
    /// A real heading (levels 1-6)
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

/// A section of content under a heading
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Section {
    /// The heading that starts this section (None for document root)
    pub heading: Option<Heading>,
    /// Hierarchy index (e.g., "0", "0.1", "0.1.1")
    pub index: String,
    /// Content of this section excluding child sections
    pub content: String,
    /// Byte range of the entire section (heading + content + children)
    pub start_byte: usize,
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

/// Build hierarchical sections from document AST
///
/// `doc_start` is the byte offset where content starts (after frontmatter).
/// Pass 0 if there's no frontmatter.
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

/// Query a markdown file and return structured data
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
