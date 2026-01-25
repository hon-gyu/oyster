//! Parsing logic for extracting structured data from Markdown documents.

use super::heading::Heading;
use super::types::{Frontmatter, Markdown, Range, Section, SectionHeading};
use crate::ast::{Node, NodeKind, Tree};
use crate::hierarchy::build_padded_tree;
use crate::link::extract_frontmatter;

/// Boundary info for section ranges (byte offset and position)
struct Boundary {
    pub byte: usize,
    pub row: usize,
    pub col: usize,
}

impl Boundary {
    pub fn zero() -> Self {
        Self {
            byte: 0,
            row: 0,
            col: 0,
        }
    }
}

impl Markdown {
    pub fn new(source: &str) -> Self {
        let tree = Tree::new_with_default_opts(&source);

        // Extract frontmatter from the first child if it's a metadata block
        let first_child = tree.root_node.children.first();
        let (frontmatter, doc_start) = match first_child {
            Some(node) => {
                let fm_content = extract_frontmatter(node);
                match fm_content {
                    Some(value) => {
                        let fm = Frontmatter {
                            value,
                            range: Range::new(
                                node.start_byte,
                                node.end_byte,
                                node.start_point.row,
                                node.start_point.column,
                                node.end_point.row,
                                node.end_point.column,
                            ),
                        };
                        let start = Boundary {
                            byte: node.end_byte,
                            row: node.end_point.row,
                            col: node.end_point.column,
                        };
                        (Some(fm), start)
                    }
                    None => (None, Boundary::zero()),
                }
            }
            None => (None, Boundary::zero()),
        };

        // Build section tree, starting content after frontmatter
        let sections = build_sections(&tree.root_node, &source, doc_start).expect("Infallible: sections should be able to be built from valid AST");

        Self {
            frontmatter,
            sections,
        }
    }

    pub fn from_path(path: &std::path::Path) -> Result<Self, String> {
        let source = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read file: {}", e))?;

        if source.is_empty() {
            return Err("File is empty".to_string());
        }

        Ok(Markdown::new(&source))
    }
}

/// Build a hierarchical section tree from a document AST.
///
/// # Arguments
///
/// - `root`: The root node of the parsed Markdown AST
/// - `source`: The original source text (used for content extraction)
/// - `doc_start_byte`: Byte offset where document content starts
/// - `doc_start_line`: Line number where document content starts
///   - Pass `(0, 0)` if there's no frontmatter
///   - Pass the frontmatter's end position to exclude it from root content
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
fn build_sections(
    root: &Node,
    source: &str,
    doc_start: Boundary,
) -> Result<Section, String> {
    // 1. Extract headings from AST
    let headings = extract_headings(root, source);

    let doc_end = Boundary {
        byte: root.end_byte,
        row: root.end_point.row,
        col: root.end_point.column,
    };

    // 2. Handle case with no headings - create root section with all content
    if headings.is_empty() {
        let content = source[doc_start.byte..doc_end.byte].trim().to_string();
        return Ok(Section {
            heading: SectionHeading::Root,
            path: "root".to_string(),
            content,
            range: Range::new(
                doc_start.byte,
                doc_end.byte,
                doc_start.row,
                doc_start.col,
                doc_end.row,
                doc_end.col,
            ),
            children: vec![],
        });
    }

    // 3. Build tree with min_level = 0 for document root
    let tree = build_padded_tree(headings, Some(0))?;

    // 4. Convert to Section with content extraction
    Ok(hierarchy_to_section(&tree, source, doc_start, doc_end))
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
            range: Range::new(
                node.start_byte,
                node.end_byte,
                node.start_point.row,
                node.start_point.column,
                node.end_point.row,
                node.end_point.column,
            ),
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
/// `next_boundary` contains both byte offset and line number for the section end.
fn hierarchy_to_section(
    item: &crate::hierarchy::HierarchyItem<SectionHeading>,
    source: &str,
    doc_start: Boundary,
    next_boundary: Boundary,
) -> Section {
    // Convert index to path string (e.g., [0, 1, 2] -> "1.2")
    let path = item
        .index
        .as_ref()
        .map(|idx| {
            if idx.len() == 1 {
                "root".to_string()
            } else {
                idx[1..]
                    .iter()
                    .map(|n| n.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            }
        })
        .unwrap_or_default();

    // Check if this is an implicit section (last path component is 0, but not root)
    let is_implicit = if let Some(idx) = &item.index {
        idx.len() > 1 && idx.last() == Some(&0)
    } else {
        false
    };

    // Get heading and section start info
    // For root, content starts at doc_start (after frontmatter)
    // For implicit sections with children, inherit from first child
    let heading = item.value.clone();
    let (section_start_byte, content_start, start_pos) =
        if is_implicit && !item.children.is_empty() {
            // Implicit section: use first child's location
            match &item.children[0].value {
                SectionHeading::Root => (
                    doc_start.byte,
                    doc_start.byte,
                    (doc_start.row, doc_start.col),
                ),
                SectionHeading::Heading(h) => {
                    (h.range.bytes[0], h.range.bytes[0], h.range.start)
                }
            }
        } else {
            // Normal section
            match &heading {
                SectionHeading::Root => (
                    doc_start.byte,
                    doc_start.byte,
                    (doc_start.row, doc_start.col),
                ),
                SectionHeading::Heading(h) => {
                    (h.range.bytes[0], h.range.bytes[1], h.range.start)
                }
            }
        };

    // Calculate content end: first child's start, or next_boundary
    let content_end = if item.children.is_empty() {
        next_boundary.byte
    } else {
        match &item.children[0].value {
            SectionHeading::Root => 0,
            SectionHeading::Heading(h) => h.range.bytes[0],
        }
    };

    let content = if content_start < content_end {
        source[content_start..content_end].trim().to_string()
    } else {
        String::new()
    };

    // Convert children, calculating their next boundaries from AST info
    let children: Vec<Section> = item
        .children
        .iter()
        .enumerate()
        .map(|(i, child)| {
            let child_next = if i + 1 < item.children.len() {
                match &item.children[i + 1].value {
                    SectionHeading::Root => Boundary::zero(),
                    SectionHeading::Heading(h) => Boundary {
                        byte: h.range.bytes[0],
                        row: h.range.start.0,
                        col: h.range.start.1,
                    },
                }
            } else {
                Boundary {
                    byte: next_boundary.byte,
                    row: next_boundary.row,
                    col: next_boundary.col,
                }
            };
            hierarchy_to_section(child, source, Boundary::zero(), child_next)
        })
        .collect();

    Section {
        heading,
        path,
        content,
        range: Range::new(
            section_start_byte,
            next_boundary.byte,
            start_pos.0,
            start_pos.1,
            next_boundary.row,
            next_boundary.col,
        ),
        children,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Tree;
    use crate::query::Markdown;
    use insta::assert_snapshot;

    mod test_sections {
        use super::*;

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
            let sections =
                build_sections(&tree.root_node, source, Boundary::zero())
                    .unwrap();
            assert_snapshot!(sections.to_string(), @r"
    [root]
    └─[1] # Title
        Some intro content.
        ├─[1.1] ## Section A
        │   Content of section A.
        │   └─[1.1.1] ### Subsection A.1
        │       Details here.
        └─[1.2] ## Section B
            Final thoughts.
    ");
        }

        #[test]
        fn test_build_sections_with_preamble() {
            let source = r#"This is content before any heading.

# First Heading

Some content.
"#;
            let tree = Tree::new(source, false);
            let sections =
                build_sections(&tree.root_node, source, Boundary::zero())
                    .unwrap();
            assert_snapshot!(sections.to_string(), @r"
    [root]
    This is content before any heading.
    └─[1] # First Heading
        Some content.
    ");
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
            let sections =
                build_sections(&tree.root_node, source, Boundary::zero())
                    .unwrap();
            assert_snapshot!(sections.to_string(), @r"
    [root]
    └─[0]
        └─[0.1] ## Introduction
            Intro content.
    ");
        }

        #[test]
        fn test_build_sections_gap_filling() {
            let source = r#"# Title

### Deep Section

Content.
"#;
            let tree = Tree::new(source, false);
            let sections =
                build_sections(&tree.root_node, source, Boundary::zero())
                    .unwrap();
            assert_snapshot!(sections.to_string(), @r"
    [root]
    └─[1] # Title
        └─[1.0]
            └─[1.0.1] ### Deep Section
                Content.
    ");
        }
    }

    mod test_build_md {
        use super::*;

        #[test]
        fn test_build_md() {
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

            let result = Markdown::from_path(file.path()).unwrap();
            let json = serde_json::to_string_pretty(&result).unwrap();
            assert_snapshot!(json, @r###"
    {
      "frontmatter": {
        "value": {
          "title": "Test Document",
          "tags": [
            "rust",
            "markdown"
          ]
        },
        "range": {
          "loc": "1:1-6:4",
          "bytes": [
            0,
            56
          ]
        }
      },
      "sections": {
        "heading": null,
        "path": "root",
        "content": "Some preamble.",
        "range": {
          "loc": "6:4-17:1",
          "bytes": [
            56,
            137
          ]
        },
        "children": [
          {
            "heading": {
              "level": 1,
              "text": "# Introduction\n",
              "range": {
                "loc": "10:1-11:1",
                "bytes": [
                  74,
                  89
                ]
              }
            },
            "path": "1",
            "content": "Intro content here.",
            "range": {
              "loc": "10:1-17:1",
              "bytes": [
                74,
                137
              ]
            },
            "children": [
              {
                "heading": {
                  "level": 2,
                  "text": "## Details\n",
                  "range": {
                    "loc": "14:1-15:1",
                    "bytes": [
                      111,
                      122
                    ]
                  }
                },
                "path": "1.1",
                "content": "More details.",
                "range": {
                  "loc": "14:1-17:1",
                  "bytes": [
                    111,
                    137
                  ]
                },
                "children": []
              }
            ]
          }
        ]
      }
    }
    "###);
        }

        fn text_sparse_sections() -> String {
            r#"---
title: Test Document
tags:
  - rust
  - markdown
---

Some preamble.

# Introduction

Intro content here.

### Details

More details.

# Another L1 Heading

vdiqoj

#### Another Level 4

More content.

"#
            .to_string()
        }

        #[test]
        fn test_build_md_with_sparse_headings() {
            use std::io::Write;
            use tempfile::NamedTempFile;

            let source = text_sparse_sections();
            let mut file = NamedTempFile::new().unwrap();
            file.write_all(source.as_bytes()).unwrap();

            let result = Markdown::from_path(file.path()).unwrap();
            let json = serde_json::to_string_pretty(&result).unwrap();
            assert_snapshot!(json, @r#####"
    {
      "frontmatter": {
        "value": {
          "title": "Test Document",
          "tags": [
            "rust",
            "markdown"
          ]
        },
        "range": {
          "loc": "1:1-6:4",
          "bytes": [
            0,
            56
          ]
        }
      },
      "sections": {
        "heading": null,
        "path": "root",
        "content": "Some preamble.",
        "range": {
          "loc": "6:4-25:1",
          "bytes": [
            56,
            205
          ]
        },
        "children": [
          {
            "heading": {
              "level": 1,
              "text": "# Introduction\n",
              "range": {
                "loc": "10:1-11:1",
                "bytes": [
                  74,
                  89
                ]
              }
            },
            "path": "1",
            "content": "",
            "range": {
              "loc": "10:1-18:1",
              "bytes": [
                74,
                139
              ]
            },
            "children": [
              {
                "heading": {
                  "level": 2,
                  "text": "",
                  "range": {
                    "loc": "1:1-1:1",
                    "bytes": [
                      0,
                      0
                    ]
                  }
                },
                "path": "1.0",
                "content": "",
                "range": {
                  "loc": "14:1-18:1",
                  "bytes": [
                    111,
                    139
                  ]
                },
                "children": [
                  {
                    "heading": {
                      "level": 3,
                      "text": "### Details\n",
                      "range": {
                        "loc": "14:1-15:1",
                        "bytes": [
                          111,
                          123
                        ]
                      }
                    },
                    "path": "1.0.1",
                    "content": "More details.",
                    "range": {
                      "loc": "14:1-18:1",
                      "bytes": [
                        111,
                        139
                      ]
                    },
                    "children": []
                  }
                ]
              }
            ]
          },
          {
            "heading": {
              "level": 1,
              "text": "# Another L1 Heading\n",
              "range": {
                "loc": "18:1-19:1",
                "bytes": [
                  139,
                  160
                ]
              }
            },
            "path": "2",
            "content": "",
            "range": {
              "loc": "18:1-25:1",
              "bytes": [
                139,
                205
              ]
            },
            "children": [
              {
                "heading": {
                  "level": 2,
                  "text": "",
                  "range": {
                    "loc": "1:1-1:1",
                    "bytes": [
                      0,
                      0
                    ]
                  }
                },
                "path": "2.0",
                "content": "",
                "range": {
                  "loc": "1:1-25:1",
                  "bytes": [
                    0,
                    205
                  ]
                },
                "children": [
                  {
                    "heading": {
                      "level": 3,
                      "text": "",
                      "range": {
                        "loc": "1:1-1:1",
                        "bytes": [
                          0,
                          0
                        ]
                      }
                    },
                    "path": "2.0.0",
                    "content": "",
                    "range": {
                      "loc": "22:1-25:1",
                      "bytes": [
                        169,
                        205
                      ]
                    },
                    "children": [
                      {
                        "heading": {
                          "level": 4,
                          "text": "#### Another Level 4\n",
                          "range": {
                            "loc": "22:1-23:1",
                            "bytes": [
                              169,
                              190
                            ]
                          }
                        },
                        "path": "2.0.0.1",
                        "content": "More content.",
                        "range": {
                          "loc": "22:1-25:1",
                          "bytes": [
                            169,
                            205
                          ]
                        },
                        "children": []
                      }
                    ]
                  }
                ]
              }
            ]
          }
        ]
      }
    }
    "#####);
        }

        #[test]
        fn test_json_roundtrip() {
            use std::io::Write;
            use tempfile::NamedTempFile;

            let source = r#"---
title: Roundtrip Test
---

Preamble content.

# Introduction

Some intro.

## Details

More details.
"#;
            let mut file = NamedTempFile::new().unwrap();
            file.write_all(source.as_bytes()).unwrap();

            // Parse markdown to struct
            let original = Markdown::from_path(file.path()).unwrap();

            // Serialize to JSON
            let json = serde_json::to_string_pretty(&original).unwrap();

            // Deserialize back
            let roundtripped: Markdown = serde_json::from_str(&json).unwrap();

            // Re-serialize and compare
            let json2 = serde_json::to_string_pretty(&roundtripped).unwrap();

            assert_eq!(
                json, json2,
                "JSON roundtrip should produce identical output"
            );
        }

        #[test]
        fn test_struct_roundtrip() {
            use std::io::Write;
            use tempfile::NamedTempFile;

            let source = r#"---
title: Struct Roundtrip
---

Preamble.

# Heading One

Content one.

## Subheading

Subcontent.
"#;
            let mut file = NamedTempFile::new().unwrap();
            file.write_all(source.as_bytes()).unwrap();

            // Parse markdown to struct
            let original = Markdown::from_path(file.path()).unwrap();

            // Serialize to JSON and back
            let json = serde_json::to_string(&original).unwrap();
            let roundtripped: Markdown = serde_json::from_str(&json).unwrap();

            // Compare structs directly
            assert_eq!(
                original, roundtripped,
                "Struct roundtrip should produce identical struct"
            );
        }

        #[test]
        fn test_markdown_display_with_sparse_headings() {
            use std::io::Write;
            use tempfile::NamedTempFile;

            let source = text_sparse_sections();

            let mut file = NamedTempFile::new().unwrap();
            file.write_all(source.as_bytes()).unwrap();

            let result = Markdown::from_path(file.path()).unwrap();
            assert_snapshot!(result.to_string(), @r"
    ---
    title: Test Document
    tags:
    - rust
    - markdown
    ---

    [root]
    Some preamble.
    ├─[1] # Introduction
    │   └─[1.0]
    │       └─[1.0.1] ### Details
    │           More details.
    └─[2] # Another L1 Heading
        └─[2.0]
            └─[2.0.0]
                └─[2.0.0.1] #### Another Level 4
                    More content.
    ");
        }

        #[test]
        fn test_markdown_reconstruction() {
            let source = r#"---
title: Reconstruction Test
tags:
  - markdown
---

Preamble content here.

# Introduction

Intro content here.

## Details

More details.

# Second Section

Final content.
"#;
            let result = Markdown::new(source);
            let reconstructed = result.to_src();

            assert_snapshot!(reconstructed, @r"
    ---
    title: Reconstruction Test
    tags:
    - markdown
    ---

    Preamble content here.

    # Introduction

    Intro content here.

    ## Details

    More details.

    # Second Section

    Final content.
    ");
        }

        #[test]
        fn test_roundtrip_markdown_to_struct_to_markdown() {
            let original_source = r#"---
title: Roundtrip Test
---

Preamble.

# Heading 1

Content 1.

## Heading 2

Content 2.
"#;

            let parsed = Markdown::new(original_source);

            // struct -> markdown
            let reconstructed = parsed.to_src();

            // markdown -> struct (again)
            let reparsed = Markdown::new(reconstructed.as_str());

            // Compare the two structs
            assert_eq!(
                parsed, reparsed,
                "markdown -> struct -> markdown -> struct should be idempotent"
            );
        }

        #[test]
        fn test_roundtrip_struct_to_markdown_to_struct() {
            let source = r#"---
title: Struct Roundtrip
tags:
  - test
---

Root content.

# Section 1

Section 1 content.

### Deep Section

Deep content.

# Section 2

Section 2 content.
"#;
            // markdown -> struct
            let original_struct = Markdown::new(source);

            // struct -> markdown
            let markdown = original_struct.to_src();

            // markdown -> struct
            let roundtripped_struct = Markdown::new(markdown.as_str());

            // struct -> markdown (again)
            let markdown2 = roundtripped_struct.to_src();

            // The markdown should be identical after roundtrip
            assert_eq!(
                markdown, markdown2,
                "struct -> markdown -> struct -> markdown should be idempotent"
            );
        }
    }
}
