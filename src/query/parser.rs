//! Parsing logic for extracting structured data from Markdown documents.

use super::heading::Heading;
use super::types::{Frontmatter, Markdown, Range, Section, SectionHeading};
use crate::ast::{Node, NodeKind, Tree};
use crate::hierarchy::build_padded_tree;
use crate::link::extract_frontmatter;

/// Boundary info for section ranges (byte offset and position)
pub(crate) struct Boundary {
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
pub(crate) fn build_sections(
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
