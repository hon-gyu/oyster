//! Heading representation for Markdown documents.

use super::Range;
use crate::hierarchy::Hierarchical;
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// Serialize HeadingLevel as integer (1-6)
mod heading_level_serde {
    use super::*;

    pub fn serialize<S>(level: &HeadingLevel, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u8(*level as u8)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<HeadingLevel, D::Error>
    where
        D: Deserializer<'de>,
    {
        let level: u8 = Deserialize::deserialize(deserializer)?;
        HeadingLevel::try_from(level as usize)
            .map_err(|_| serde::de::Error::custom(format!("invalid heading level: {}", level)))
    }
}

/// A Markdown heading with source location and optional attributes.
///
/// # Fields
///
/// Core fields:
/// - `level`: Heading level (1-6, corresponding to # through ######)
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
/// - `level` serializes as integer (1-6)
/// - Optional fields (`id`, `classes`, `attrs`) are omitted when empty
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Heading {
    /// Heading level: 1 through 6
    #[serde(with = "heading_level_serde")]
    pub level: HeadingLevel,
    /// Raw heading text including markers (e.g., "## Section\n")
    pub text: String,
    /// Source location range
    pub range: Range,
    /// Optional heading ID for anchor links (e.g., `{#introduction}`)
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub id: Option<String>,
    /// CSS classes from extended syntax (e.g., `{.warning}`)
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub classes: Vec<String>,
    /// Custom attributes as key-value pairs (e.g., `{data-section=intro}`)
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub attrs: Vec<(String, Option<String>)>,
}

impl Hierarchical for Heading {
    fn level(&self) -> usize {
        self.level as usize
    }
}
