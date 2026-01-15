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
//!
//! # Non-goal
//! - Lossless conversion from Markdown to JSON
//!   - headinng contains extra information (e.g., id, classes, attrs), which might
//!     be dropped during serialization

use crate::ast::{Node, NodeKind, Tree};
use crate::hierarchy::{
    Hierarchical, HierarchicalWithDefaults, build_padded_tree,
};
use crate::link::extract_frontmatter;
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_yaml::Value as YamlValue;

mod heading;
pub use heading::Heading;

/// Source location range with byte offsets and line numbers.
///
/// Provides a compact representation of source location:
/// - `bytes`: `[start_byte, end_byte]` - byte range in source
/// - `lines`: `[start_line, end_line]` - line numbers (0-indexed)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Range {
    /// Byte range: [start_byte, end_byte]
    pub bytes: [usize; 2],
    /// Line range: [start_line, end_line] (0-indexed)
    pub lines: [usize; 2],
}

impl Range {
    pub fn new(
        start_byte: usize,
        end_byte: usize,
        start_line: usize,
        end_line: usize,
    ) -> Self {
        Self {
            bytes: [start_byte, end_byte],
            lines: [start_line, end_line],
        }
    }

    pub fn zero() -> Self {
        Self {
            bytes: [0, 0],
            lines: [0, 0],
        }
    }
}

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
/// - `value`: Parsed YAML value (can be any valid YAML structure)
/// - `range`: Source location (byte range and line numbers)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Frontmatter {
    /// Parsed YAML content
    pub value: YamlValue,
    /// Source location range
    pub range: Range,
}

/// Internal wrapper for tree building that supports level 0 (document root).
///
/// This allows us to use `build_padded_tree` with a virtual root at level 0,
/// which captures content before the first heading.
///
/// # Serialization
///
/// Serializes as `null` for Root, or the Heading object directly for Heading variant.
#[derive(Debug, Clone)]
pub enum SectionHeading {
    /// Document root (level 0) - captures content before the first heading
    Root,
    /// A real Markdown heading (levels 1-6, i.e., # to ######)
    Heading(Heading),
}

impl Serialize for SectionHeading {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            SectionHeading::Root => serializer.serialize_none(),
            SectionHeading::Heading(h) => h.serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for SectionHeading {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: Option<Heading> = Option::deserialize(deserializer)?;
        Ok(match value {
            None => SectionHeading::Root,
            Some(h) => SectionHeading::Heading(h),
        })
    }
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
                range: Range::zero(),
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
/// - Root section (level 0): `heading` is `null`, contains preamble content
/// - Heading sections (levels 1-6): `heading` contains the [`Heading`] data
/// - Implicit sections: Created for level gaps (e.g., H1 → H3 creates implicit H2)
///
/// # Fields
///
/// - `heading`: The heading that starts this section (`null` for root)
/// - `index`: Hierarchical position (e.g., "0.1.2" means first H1's second H2's third H3)
/// - `content`: Text content between this heading and the next heading/child
/// - `range`: Source location (byte range and line numbers)
/// - `children`: Nested sections at deeper heading levels
///
/// # Index Format
///
/// The index uses dot notation where:
/// - "root" = document root
/// - "1" = first H1 under root
/// - "1.2" = second H2 under that H1
/// - "1.0" = implicit heading (index component is 0)
///
/// # Contract
/// - implicit section's information will be the same as its first child except Root
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Section {
    /// The heading that starts this section (null for root)
    pub heading: SectionHeading,
    /// Hierarchical index in dot notation (e.g., "root", "1", "1.2")
    pub index: String,
    /// Text content of this section, excluding child sections.
    /// Trimmed of leading/trailing whitespace.
    pub content: String,
    /// Source location range
    pub range: Range,
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
            SectionHeading::Root => "(root)".to_string(),
            // TODO: this assumes that empty headings are implicit
            // which is not true
            SectionHeading::Heading(_) if self.is_implicit() => {
                "(implicit)".to_string()
            }
            SectionHeading::Heading(h) => {
                h.text.lines().next().unwrap_or("").to_string()
            }
        };

        // Print section header with index and byte range
        writeln!(
            f,
            "{}[{}] {} [{}..{}]",
            prefix, self.index, title, self.range.bytes[0], self.range.bytes[1]
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

    fn is_implicit(&self) -> bool {
        let indices = self.index.split('.').collect::<Vec<_>>();
        if indices.len() == 1 && indices[0] == "root" {
            true
        } else {
            let last_index_number = indices.last().expect("Never: cannot be 0");
            last_index_number
                .parse::<usize>()
                .expect("Never: index is constructed from ints")
                == 0
        }
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
pub fn build_sections(
    root: &Node,
    source: &str,
    doc_start_byte: usize,
    doc_start_line: usize,
) -> Result<Section, String> {
    // 1. Extract headings from AST
    let headings = extract_headings(root, source);

    // 2. Build tree with min_level = 0 for document root
    let tree = build_padded_tree(headings, Some(0))?;

    // 3. Convert to Section with content extraction
    let doc_start = Boundary {
        byte: doc_start_byte,
        line: doc_start_line,
    };
    let doc_end = Boundary {
        byte: root.end_byte,
        line: root.end_point.row,
    };
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
    let (frontmatter, doc_start_byte, doc_start_line) = match first_child {
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
                            node.end_point.row,
                        ),
                    };
                    (Some(fm), node.end_byte, node.end_point.row)
                }
                None => (None, 0, 0),
            }
        }
        None => (None, 0, 0),
    };

    // Build section tree, starting content after frontmatter
    let sections = build_sections(
        &tree.root_node,
        &source,
        doc_start_byte,
        doc_start_line,
    )?;

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
            range: Range::new(
                node.start_byte,
                node.end_byte,
                node.start_point.row,
                node.end_point.row,
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

/// Boundary info for section ranges (byte offset and line number)
struct Boundary {
    byte: usize,
    line: usize,
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
    // Convert index to string (e.g., [0, 1, 2] -> "0.1.2")
    let index = item
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

    // Check if this is an implicit section (last index component is 0, but not root)
    let is_implicit = if let Some(idx) = &item.index {
        idx.len() > 1 && idx.last() == Some(&0)
    } else {
        false
    };

    // Get heading and section start info
    // For root, content starts at doc_start (after frontmatter)
    // For implicit sections with children, inherit from first child
    let heading = item.value.clone();
    let (section_start, content_start, start_line) =
        if is_implicit && !item.children.is_empty() {
            // Implicit section: use first child's location
            match &item.children[0].value {
                SectionHeading::Root => {
                    (doc_start.byte, doc_start.byte, doc_start.line)
                }
                SectionHeading::Heading(h) => {
                    (h.range.bytes[0], h.range.bytes[0], h.range.lines[0])
                }
            }
        } else {
            // Normal section
            match &heading {
                SectionHeading::Root => {
                    (doc_start.byte, doc_start.byte, doc_start.line)
                }
                SectionHeading::Heading(h) => {
                    (h.range.bytes[0], h.range.bytes[1], h.range.lines[0])
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
                    SectionHeading::Root => Boundary { byte: 0, line: 0 },
                    SectionHeading::Heading(h) => Boundary {
                        byte: h.range.bytes[0],
                        line: h.range.lines[0],
                    },
                }
            } else {
                Boundary {
                    byte: next_boundary.byte,
                    line: next_boundary.line,
                }
            };
            hierarchy_to_section(
                child,
                source,
                Boundary { byte: 0, line: 0 },
                child_next,
            )
        })
        .collect();

    Section {
        heading,
        index,
        content,
        range: Range::new(
            section_start,
            next_boundary.byte,
            start_line,
            next_boundary.line,
        ),
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
        let sections = build_sections(&tree.root_node, source, 0, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r#"
        [root] (root) [0..132]
          [1] # Title [0..132]
            content: "Some intro content."
            [1.1] ## Section A [30..102]
              content: "Content of section A."
              [1.1.1] ### Subsection A.1 [67..102]
                content: "Details here."
            [1.2] ## Section B [102..132]
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
        let sections = build_sections(&tree.root_node, source, 0, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r#"
        [root] (root) [0..68]
          content: "This is content before any heading."
          [1] # First Heading [37..68]
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
        let sections = build_sections(&tree.root_node, source, 0, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r#"
        [root] (root) [0..78]
          [0] (implicit) [46..78]
            [0.1] ## Introduction [46..78]
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
        let sections = build_sections(&tree.root_node, source, 0, 0).unwrap();
        assert_snapshot!(sections.to_string(), @r#"
        [root] (root) [0..36]
          [1] # Title [0..36]
            [1.0] (implicit) [9..36]
              [1.0.1] ### Deep Section [9..36]
                content: "Content."
        "#);
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
            "value": {
              "title": "Test Document",
              "tags": [
                "rust",
                "markdown"
              ]
            },
            "range": {
              "bytes": [
                0,
                56
              ],
              "lines": [
                0,
                5
              ]
            }
          },
          "sections": {
            "heading": null,
            "index": "root",
            "content": "Some preamble.",
            "range": {
              "bytes": [
                56,
                137
              ],
              "lines": [
                5,
                16
              ]
            },
            "children": [
              {
                "heading": {
                  "level": "H1",
                  "text": "# Introduction\n",
                  "range": {
                    "bytes": [
                      74,
                      89
                    ],
                    "lines": [
                      9,
                      10
                    ]
                  }
                },
                "index": "1",
                "content": "Intro content here.",
                "range": {
                  "bytes": [
                    74,
                    137
                  ],
                  "lines": [
                    9,
                    16
                  ]
                },
                "children": [
                  {
                    "heading": {
                      "level": "H2",
                      "text": "## Details\n",
                      "range": {
                        "bytes": [
                          111,
                          122
                        ],
                        "lines": [
                          13,
                          14
                        ]
                      }
                    },
                    "index": "1.1",
                    "content": "More details.",
                    "range": {
                      "bytes": [
                        111,
                        137
                      ],
                      "lines": [
                        13,
                        16
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

    #[test]
    fn test_query_file_padded() {
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

### Details

More details.

# Another L1 Heading

vdiqoj

#### Another Level 4

More content.

"#;
        let mut file = NamedTempFile::new().unwrap();
        file.write_all(source.as_bytes()).unwrap();

        let result = query_file(file.path()).unwrap();
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
              "bytes": [
                0,
                56
              ],
              "lines": [
                0,
                5
              ]
            }
          },
          "sections": {
            "heading": null,
            "index": "root",
            "content": "Some preamble.",
            "range": {
              "bytes": [
                56,
                205
              ],
              "lines": [
                5,
                24
              ]
            },
            "children": [
              {
                "heading": {
                  "level": "H1",
                  "text": "# Introduction\n",
                  "range": {
                    "bytes": [
                      74,
                      89
                    ],
                    "lines": [
                      9,
                      10
                    ]
                  }
                },
                "index": "1",
                "content": "",
                "range": {
                  "bytes": [
                    74,
                    139
                  ],
                  "lines": [
                    9,
                    17
                  ]
                },
                "children": [
                  {
                    "heading": {
                      "level": "H2",
                      "text": "",
                      "range": {
                        "bytes": [
                          0,
                          0
                        ],
                        "lines": [
                          0,
                          0
                        ]
                      }
                    },
                    "index": "1.0",
                    "content": "",
                    "range": {
                      "bytes": [
                        111,
                        139
                      ],
                      "lines": [
                        13,
                        17
                      ]
                    },
                    "children": [
                      {
                        "heading": {
                          "level": "H3",
                          "text": "### Details\n",
                          "range": {
                            "bytes": [
                              111,
                              123
                            ],
                            "lines": [
                              13,
                              14
                            ]
                          }
                        },
                        "index": "1.0.1",
                        "content": "More details.",
                        "range": {
                          "bytes": [
                            111,
                            139
                          ],
                          "lines": [
                            13,
                            17
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
                  "level": "H1",
                  "text": "# Another L1 Heading\n",
                  "range": {
                    "bytes": [
                      139,
                      160
                    ],
                    "lines": [
                      17,
                      18
                    ]
                  }
                },
                "index": "2",
                "content": "",
                "range": {
                  "bytes": [
                    139,
                    205
                  ],
                  "lines": [
                    17,
                    24
                  ]
                },
                "children": [
                  {
                    "heading": {
                      "level": "H2",
                      "text": "",
                      "range": {
                        "bytes": [
                          0,
                          0
                        ],
                        "lines": [
                          0,
                          0
                        ]
                      }
                    },
                    "index": "2.0",
                    "content": "",
                    "range": {
                      "bytes": [
                        0,
                        205
                      ],
                      "lines": [
                        0,
                        24
                      ]
                    },
                    "children": [
                      {
                        "heading": {
                          "level": "H3",
                          "text": "",
                          "range": {
                            "bytes": [
                              0,
                              0
                            ],
                            "lines": [
                              0,
                              0
                            ]
                          }
                        },
                        "index": "2.0.0",
                        "content": "",
                        "range": {
                          "bytes": [
                            169,
                            205
                          ],
                          "lines": [
                            21,
                            24
                          ]
                        },
                        "children": [
                          {
                            "heading": {
                              "level": "H4",
                              "text": "#### Another Level 4\n",
                              "range": {
                                "bytes": [
                                  169,
                                  190
                                ],
                                "lines": [
                                  21,
                                  22
                                ]
                              }
                            },
                            "index": "2.0.0.1",
                            "content": "More content.",
                            "range": {
                              "bytes": [
                                169,
                                205
                              ],
                              "lines": [
                                21,
                                24
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
}
