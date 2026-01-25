//! Core types for structured Markdown representation.

use super::heading::Heading;
use crate::hierarchy::{Hierarchical, HierarchicalWithDefaults};
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_yaml::Value as YamlValue;

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
            result.push_str(&fm.to_src());
        }

        // Add sections as markdown
        result.push_str(&self.sections.to_src());

        result
    }

    pub fn index_sections(&self, idx: usize) -> Self {
        let new_md_src = {
            let mut buf = String::new();
            if let Some(frontmatter) = &self.frontmatter {
                buf.push_str(&frontmatter.to_src());
            }

            let orgi_src = self.to_src();
            let new_secs_byte_start =
                self.sections.children[idx].range.bytes[0];
            let new_secs_byte_end = self.sections.children[idx].range.bytes[1];
            buf.push_str(&orgi_src[new_secs_byte_start..new_secs_byte_end]);
            buf
        };
        Self::new(&new_md_src)
    }

    // TODO(perf): This is very inefficient
    /// Left and right inclusive
    pub fn slice_sections_inclusive(
        &self,
        start_idx: usize,
        end_idx: usize,
    ) -> Self {
        let new_md_src = {
            let mut buf = String::new();
            if let Some(frontmatter) = &self.frontmatter {
                buf.push_str(&frontmatter.to_src());
            }

            let orgi_src = self.to_src();
            let new_secs_byte_start =
                self.sections.children[start_idx].range.bytes[0];
            let new_secs_byte_end =
                self.sections.children[end_idx].range.bytes[1];
            buf.push_str(&orgi_src[new_secs_byte_start..new_secs_byte_end]);
            buf
        };
        Self::new(&new_md_src)
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

impl Frontmatter {
    pub fn to_src(&self) -> String {
        let mut buf = String::new();
        buf.push_str("---\n");
        let yaml_str = serde_yaml::to_string(&self.value)
            .unwrap_or_else(|_| String::new());
        buf.push_str(yaml_str.trim_end());
        buf.push_str("\n---\n\n");
        buf
    }
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
    /// The heading that starts this section (H1-6 or root)
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
        let title: Option<String> = match &self.heading {
            SectionHeading::Root => None,
            SectionHeading::Heading(_) if self.is_implicit() => None,
            SectionHeading::Heading(h) => {
                let t = h.text.lines().next().unwrap_or("").to_string();
                Some(t)
            }
        };

        // Print section header: path and title
        if let Some(title) = title {
            writeln!(f, "[{}] {}", self.path, title)?;
        } else {
            writeln!(f, "[{}]", self.path)?;
        }

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

    /// Returns true if the node is implicit, i.e., synthetically created for
    /// level gaps (e.g., H1 → H3 creates implicit H2, or document root node).
    ///
    /// Determined by checking if the path is "root" or ending with 0 as in "1.0".
    pub(crate) fn is_implicit(&self) -> bool {
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
    pub(crate) fn to_src(&self) -> String {
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

// Range type
// ====================

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
