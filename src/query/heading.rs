//! Heading representation for Markdown documents.

use super::Range;
use crate::hierarchy::Hierarchical;
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Serialize};

/// A Markdown heading with source location and optional attributes.
///
/// # Fields
///
/// Core fields:
/// - `level`: Heading level (H1-H6, corresponding to # through ######)
/// - `text`: Raw text content including the heading markers (e.g., "# Title\n")
/// - `range`: Source location (byte range and line numbers)
///
/// Optional attributes (from extended Markdown syntax like `{#id .class attr=value}`):
/// - `id`: Heading ID for linking (e.g., `{#my-heading}`)
/// - `classes`: CSS classes (e.g., `{.warning .highlight}`)
/// - `attrs`: Key-value attributes (e.g., `{data-foo=bar}`)
///
/// # Serialization
///
/// Optional fields (`id`, `classes`, `attrs`) are omitted from JSON when empty.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Heading {
    /// Heading level: H1 (1) through H6 (6)
    pub level: HeadingLevel,
    /// Raw heading text including markers (e.g., "## Section\n")
    pub text: String,
    /// Source location range
    pub range: Range,
    /// Optional heading ID for anchor links (e.g., `{#introduction}`)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    /// CSS classes from extended syntax (e.g., `{.warning}`)
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub classes: Vec<String>,
    /// Custom attributes as key-value pairs (e.g., `{data-section=intro}`)
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub attrs: Vec<(String, Option<String>)>,
}

impl Hierarchical for Heading {
    fn level(&self) -> usize {
        self.level as usize
    }
}
