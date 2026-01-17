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

mod heading;
mod parser;
mod types;

#[cfg(test)]
mod tests;

// Public types
pub use heading::Heading;
pub use types::{Frontmatter, Markdown, Range, Section, SectionHeading};

// Public functions
pub use parser::query_file;
