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

/// Source location range with byte offsets and line:column positions.
///
/// # Serialization
///
/// Serializes to:
/// ```json
/// { "loc": "12:5-13:6", "bytes": [100, 200] }
/// ```
/// where `loc` is 1-indexed (row:col-row:col) for editor compatibility.
#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    /// Byte range: [start_byte, end_byte]
    pub bytes: [usize; 2],
    /// Start position: (row, column), 0-indexed internally
    pub start: (usize, usize),
    /// End position: (row, column), 0-indexed internally
    pub end: (usize, usize),
}

impl Serialize for Range {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut s = serializer.serialize_struct("Range", 2)?;
        // 1-indexed for display
        let loc = format!(
            "{}:{}-{}:{}",
            self.start.0 + 1,
            self.start.1 + 1,
            self.end.0 + 1,
            self.end.1 + 1
        );
        s.serialize_field("loc", &loc)?;
        s.serialize_field("bytes", &self.bytes)?;
        s.end()
    }
}

impl<'de> Deserialize<'de> for Range {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct RangeHelper {
            loc: String,
            bytes: [usize; 2],
        }

        let helper = RangeHelper::deserialize(deserializer)?;

        // Parse "row:col-row:col" (1-indexed) back to 0-indexed
        let parts: Vec<&str> = helper.loc.split('-').collect();
        if parts.len() != 2 {
            return Err(serde::de::Error::custom("invalid loc format"));
        }

        let start_parts: Vec<&str> = parts[0].split(':').collect();
        let end_parts: Vec<&str> = parts[1].split(':').collect();

        if start_parts.len() != 2 || end_parts.len() != 2 {
            return Err(serde::de::Error::custom("invalid loc format"));
        }

        let start_row: usize =
            start_parts[0].parse().map_err(serde::de::Error::custom)?;
        let start_col: usize =
            start_parts[1].parse().map_err(serde::de::Error::custom)?;
        let end_row: usize =
            end_parts[0].parse().map_err(serde::de::Error::custom)?;
        let end_col: usize =
            end_parts[1].parse().map_err(serde::de::Error::custom)?;

        Ok(Range {
            bytes: helper.bytes,
            // Convert from 1-indexed to 0-indexed
            start: (start_row.saturating_sub(1), start_col.saturating_sub(1)),
            end: (end_row.saturating_sub(1), end_col.saturating_sub(1)),
        })
    }
}

impl Range {
    pub fn new(
        start_byte: usize,
        end_byte: usize,
        start_row: usize,
        start_col: usize,
        end_row: usize,
        end_col: usize,
    ) -> Self {
        Self {
            bytes: [start_byte, end_byte],
            start: (start_row, start_col),
            end: (end_row, end_col),
        }
    }

    pub fn zero() -> Self {
        Self {
            bytes: [0, 0],
            start: (0, 0),
            end: (0, 0),
        }
    }
}

/// Structured representation of a Markdown document.
///
/// Contains:
/// - `frontmatter`: Optional YAML metadata block with source location
/// - `sections`: Hierarchical tree of document sections
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Markdown {
    /// YAML frontmatter (if present at the start of the document)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub frontmatter: Option<Frontmatter>,
    /// Hierarchical section tree rooted at level 0
    pub sections: Section,
}

impl std::fmt::Display for Markdown {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display frontmatter if present (complete, as YAML)
        if let Some(fm) = &self.frontmatter {
            writeln!(f, "---")?;
            // Use serde_yaml to format the value
            let yaml_str = serde_yaml::to_string(&fm.value)
                .unwrap_or_else(|_| "<invalid yaml>".to_string());
            // serde_yaml adds a trailing newline, so we trim and add our own
            write!(f, "{}", yaml_str.trim_end())?;
            writeln!(f)?;
            writeln!(f, "---")?;
            writeln!(f)?;
        }

        // Display section tree
        write!(f, "{}", self.sections)
    }
}

impl Markdown {
    pub fn new(source: &str) -> Result<Self, String> {
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
        let sections = build_sections(&tree.root_node, &source, doc_start)?;

        Ok(Markdown {
            frontmatter,
            sections,
        })
    }
    /// Convert the Markdown struct back to source markdown text.
    ///
    /// Reconstructs the original markdown format including:
    /// - Frontmatter (if present) in YAML format between `---` delimiters
    /// - All sections with their headings and content
    ///
    /// Note: Implicit sections (created for level gaps) are skipped as they
    /// have no content in the original source.
    pub fn to_src(&self) -> String {
        let mut result = String::new();

        // Add frontmatter if present
        if let Some(fm) = &self.frontmatter {
            result.push_str("---\n");
            let yaml_str = serde_yaml::to_string(&fm.value)
                .unwrap_or_else(|_| String::new());
            result.push_str(yaml_str.trim_end());
            result.push_str("\n---\n\n");
        }

        // Add sections as markdown
        result.push_str(&self.sections.to_src());

        result
    }
}

/// YAML frontmatter with source location information.
///
/// Fields:
/// - `value`: Parsed YAML value (can be any valid YAML structure)
/// - `range`: Source location (byte range and line numbers)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq)]
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
/// - `path`: Hierarchical path (e.g., "1.2.3" means first H1's second H2's third H3)
/// - `content`: Text content between this heading and the next heading/child
/// - `range`: Source location (byte range and line numbers)
/// - `children`: Nested sections at deeper heading levels
///
/// # Path Format
///
/// The path uses dot notation where:
/// - "root" = document root
/// - "1" = first H1 under root
/// - "1.2" = second H2 under that H1
/// - "1.0" = implicit heading (path component is 0)
///
/// # Contract
/// - implicit section's information will be the same as its first child except Root
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Section {
    /// The heading that starts this section (null for root)
    pub heading: SectionHeading,
    /// Hierarchical path in dot notation (e.g., "root", "1", "1.2")
    pub path: String,
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
        self.fmt_with_prefix(f, "")
    }
}

impl Section {
    /// Maximum characters to show for content preview
    const CONTENT_PREVIEW_LEN: usize = 50;

    // TODO: make implicit sections more prominent
    // TODO: color?
    fn fmt_with_prefix(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        prefix: &str,
    ) -> std::fmt::Result {
        // Format heading info: level, title
        let title = match &self.heading {
            SectionHeading::Root => "".to_string(),
            SectionHeading::Heading(_) if self.is_implicit() => "".to_string(),
            SectionHeading::Heading(h) => {
                h.text.lines().next().unwrap_or("").to_string()
            }
        };

        // Print section header: level, path, title, byte range
        writeln!(
            f,
            // "[{}] {} [{}:{}-{}:{}]",
            "[{}] {}",
            self.path,
            title,
            // self.range.start.0,
            // self.range.start.1,
            // self.range.end.0,
            // self.range.end.1,
        )?;

        // Print content preview if non-empty
        if !self.content.is_empty() {
            let preview: String = self
                .content
                .chars()
                .take(Self::CONTENT_PREVIEW_LEN)
                .map(|c| if c == '\n' { ' ' } else { c })
                .collect();
            let ellipsis =
                if self.content.chars().count() > Self::CONTENT_PREVIEW_LEN {
                    "..."
                } else {
                    ""
                };
            // writeln!(f, "{}content: \"{preview}{ellipsis}\"", prefix)?;
            writeln!(f, "{}{preview}{ellipsis}", prefix)?;
        }

        // Print children with tree branches
        let child_count = self.children.len();
        for (i, child) in self.children.iter().enumerate() {
            let is_last = i == child_count - 1;

            // Print branch character
            if is_last {
                write!(f, "{}└─", prefix)?;
            } else {
                write!(f, "{}├─", prefix)?;
            }

            // Determine prefix for child's children
            let child_prefix = if is_last {
                format!("{}    ", prefix)
            } else {
                format!("{}│   ", prefix)
            };

            child.fmt_with_prefix(f, &child_prefix)?;
        }

        Ok(())
    }

    fn is_implicit(&self) -> bool {
        let parts = self.path.split('.').collect::<Vec<_>>();
        if parts.len() == 1 && parts[0] == "root" {
            true
        } else {
            let last = parts.last().expect("Never: path cannot be empty");
            last.parse::<usize>()
                .expect("Never: path is constructed from ints")
                == 0
        }
    }

    /// Convert section tree back to markdown format.
    ///
    /// # Returns
    ///
    /// A string containing the markdown representation of this section and its children.
    fn to_src(&self) -> String {
        let mut result = String::new();

        match &self.heading {
            SectionHeading::Root => {
                // Root section: output content if present
                if !self.content.is_empty() {
                    result.push_str(&self.content);
                    result.push_str("\n\n");
                }
            }
            SectionHeading::Heading(h) => {
                // Skip implicit sections (they have empty heading text)
                if !h.text.is_empty() {
                    // Output heading text (already includes trailing newline)
                    result.push_str(&h.text);

                    // Add blank line after heading
                    if !self.content.is_empty() {
                        result.push('\n');
                    }

                    // Output content if present
                    if !self.content.is_empty() {
                        result.push_str(&self.content);
                        result.push_str("\n\n");
                    }
                }
            }
        }

        // Recursively output children
        for child in &self.children {
            result.push_str(&child.to_src());
        }

        result
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

    // 2. Build tree with min_level = 0 for document root
    let tree = build_padded_tree(headings, Some(0))?;

    // 3. Convert to Section with content extraction
    let doc_end = Boundary {
        byte: root.end_byte,
        row: root.end_point.row,
        col: root.end_point.column,
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

    Markdown::new(&source)
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

/// Boundary info for section ranges (byte offset and position)
struct Boundary {
    byte: usize,
    row: usize,
    col: usize,
}

impl Boundary {
    fn zero() -> Self {
        Self {
            byte: 0,
            row: 0,
            col: 0,
        }
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
        let sections =
            build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
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
            build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
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
            build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
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
            build_sections(&tree.root_node, source, Boundary::zero()).unwrap();
        assert_snapshot!(sections.to_string(), @r"
        [root] 
        └─[1] # Title
            └─[1.0] 
                └─[1.0.1] ### Deep Section
                    Content.
        ");
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
    fn test_query_file_padded() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let source = text_sparse_sections();
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
        let original = query_file(file.path()).unwrap();

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
        let original = query_file(file.path()).unwrap();

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
    fn test_markdown_display() {
        use std::io::Write;
        use tempfile::NamedTempFile;

        let source = text_sparse_sections();

        let mut file = NamedTempFile::new().unwrap();
        file.write_all(source.as_bytes()).unwrap();

        let result = query_file(file.path()).unwrap();
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
        let result = Markdown::new(source).unwrap();
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

        let parsed = Markdown::new(original_source).unwrap();

        // struct -> markdown
        let reconstructed = parsed.to_src();

        // markdown -> struct (again)
        let reparsed = Markdown::new(reconstructed.as_str()).unwrap();

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
        let original_struct = Markdown::new(source).unwrap();

        // struct -> markdown
        let markdown = original_struct.to_src();

        // markdown -> struct
        let roundtripped_struct = Markdown::new(markdown.as_str()).unwrap();

        // struct -> markdown (again)
        let markdown2 = roundtripped_struct.to_src();

        // The markdown should be identical after roundtrip
        assert_eq!(
            markdown, markdown2,
            "struct -> markdown -> struct -> markdown should be idempotent"
        );
    }
}
