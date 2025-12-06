//! AST transformation utilities
//!
//! This module provides functions to transform the AST after parsing,
//! such as detecting custom blockquote types that pulldown-cmark doesn't recognize.

use super::node::{Node, NodeKind};
use pulldown_cmark::BlockQuoteKind;

/// Obsidian-specific callout types (beyond the 5 standard GFM types)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObsidianCalloutKind {
    Abstract, // Aliases: summary, tldr
    Info,
    Todo,
    Success,  // Aliases: check, done
    Question, // Aliases: help, faq
    Failure,  // Aliases: fail, missing
    Danger,   // Alias: error
    Bug,
    Example,
    Quote, // Alias: cite
}

impl ObsidianCalloutKind {
    /// Parse an Obsidian callout type from a string (case-insensitive)
    /// Supports aliases
    fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "ABSTRACT" | "SUMMARY" | "TLDR" => Some(Self::Abstract),
            "INFO" => Some(Self::Info),
            "TODO" => Some(Self::Todo),
            "SUCCESS" | "CHECK" | "DONE" => Some(Self::Success),
            "QUESTION" | "HELP" | "FAQ" => Some(Self::Question),
            "FAILURE" | "FAIL" | "MISSING" => Some(Self::Failure),
            "DANGER" | "ERROR" => Some(Self::Danger),
            "BUG" => Some(Self::Bug),
            "EXAMPLE" => Some(Self::Example),
            "QUOTE" | "CITE" => Some(Self::Quote),
            _ => None,
        }
    }

    /// Get CSS class name for this Obsidian callout type
    pub fn class_name(&self) -> &'static str {
        match self {
            Self::Abstract => "callout-abstract",
            Self::Info => "callout-info",
            Self::Todo => "callout-todo",
            Self::Success => "callout-success",
            Self::Question => "callout-question",
            Self::Failure => "callout-failure",
            Self::Danger => "callout-danger",
            Self::Bug => "callout-bug",
            Self::Example => "callout-example",
            Self::Quote => "callout-quote",
        }
    }
}

/// User-defined custom callout types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CustomCalloutKind {
    Llm,
    // Add more custom types as needed
}

impl CustomCalloutKind {
    /// Parse a custom callout type from a string
    fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "LLM" => Some(Self::Llm),
            _ => None,
        }
    }

    /// Get CSS class name for this custom type
    pub fn class_name(&self) -> &'static str {
        match self {
            Self::Llm => "custom-callout-llm",
        }
    }
}

/// Extended blockquote kind that includes standard, Obsidian, and custom types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtendedBlockQuoteKind {
    /// Standard GFM alert types (recognized by pulldown-cmark)
    Standard(BlockQuoteKind),
    /// Obsidian-specific callout types
    Obsidian(ObsidianCalloutKind),
    /// User-defined custom callout types
    Custom(CustomCalloutKind),
}

impl ExtendedBlockQuoteKind {
    pub fn class_name(&self) -> &'static str {
        match self {
            Self::Standard(BlockQuoteKind::Note) => "markdown-alert-note",
            Self::Standard(BlockQuoteKind::Tip) => "markdown-alert-tip",
            Self::Standard(BlockQuoteKind::Important) => {
                "markdown-alert-important"
            }
            Self::Standard(BlockQuoteKind::Warning) => "markdown-alert-warning",
            Self::Standard(BlockQuoteKind::Caution) => "markdown-alert-caution",
            Self::Obsidian(obsidian) => obsidian.class_name(),
            Self::Custom(custom) => custom.class_name(),
        }
    }

    /// Get default title for this callout type (title case)
    pub fn default_title(&self) -> &'static str {
        match self {
            Self::Standard(BlockQuoteKind::Note) => "Note",
            Self::Standard(BlockQuoteKind::Tip) => "Tip",
            Self::Standard(BlockQuoteKind::Important) => "Important",
            Self::Standard(BlockQuoteKind::Warning) => "Warning",
            Self::Standard(BlockQuoteKind::Caution) => "Caution",
            Self::Obsidian(ObsidianCalloutKind::Abstract) => "Abstract",
            Self::Obsidian(ObsidianCalloutKind::Info) => "Info",
            Self::Obsidian(ObsidianCalloutKind::Todo) => "Todo",
            Self::Obsidian(ObsidianCalloutKind::Success) => "Success",
            Self::Obsidian(ObsidianCalloutKind::Question) => "Question",
            Self::Obsidian(ObsidianCalloutKind::Failure) => "Failure",
            Self::Obsidian(ObsidianCalloutKind::Danger) => "Danger",
            Self::Obsidian(ObsidianCalloutKind::Bug) => "Bug",
            Self::Obsidian(ObsidianCalloutKind::Example) => "Example",
            Self::Obsidian(ObsidianCalloutKind::Quote) => "Quote",
            Self::Custom(CustomCalloutKind::Llm) => "LLM",
        }
    }
}

/// Foldable state for callouts
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FoldableState {
    /// Expanded by default (using +)
    Expanded,
    /// Collapsed by default (using -)
    Collapsed,
}

/// Complete metadata for a callout
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CalloutMetadata {
    /// The type of callout
    pub kind: ExtendedBlockQuoteKind,
    /// Custom title (None means use default)
    pub title: Option<String>,
    /// Whether the callout is foldable and its default state
    pub foldable: Option<FoldableState>,
}

impl CalloutMetadata {
    /// Get the title to display (custom or default)
    pub fn display_title(&self) -> String {
        self.title
            .clone()
            .unwrap_or_else(|| self.kind.default_title().to_string())
    }
}

/// Detect callout metadata by examining the first paragraph
///
/// Pattern: `[!TYPE(+|-)] optional title`
/// Note: pulldown-cmark splits `[!TYPE]` into separate text nodes: "[", "!TYPE", "]"
///
/// Spacing rules:
/// - Only 0-1 spaces between `>` and `[` are valid for callouts
/// - 2+ spaces make it a regular blockquote
///
/// Foldable markers:
/// - `[!TYPE]+` - foldable, expanded by default
/// - `[!TYPE]-` - foldable, collapsed by default
///
/// Returns the complete callout metadata, or None if:
/// - Already recognized by pulldown-cmark (Standard types)
/// - Invalid spacing (2+ spaces)
/// - Not a valid callout pattern
fn detect_callout_metadata(
    node: &Node,
    source_text: &str,
) -> Option<CalloutMetadata> {
    // Only process blockquotes without a recognized type
    if !matches!(&node.kind, NodeKind::BlockQuote(None)) {
        return None;
    }

    // Find first paragraph child
    let first_para = node.children.first()?;
    if !matches!(&first_para.kind, NodeKind::Paragraph) {
        return None;
    }

    // Check spacing: extract the blockquote prefix from source
    // The blockquote node includes the `>` markers
    let blockquote_src = source_text.get(node.start_byte..node.end_byte)?;
    let first_line = blockquote_src.lines().next()?;

    // Check if first line has the pattern `> [` or `>[`
    // If it's `>  [` (2+ spaces), it's not a valid callout
    if let Some(after_gt) = first_line.strip_prefix('>') {
        // Count leading spaces
        let spaces = after_gt.chars().take_while(|c| *c == ' ').count();
        if spaces >= 2 {
            // Invalid spacing for callout
            return None;
        }
        // Valid spacing (0 or 1 space), continue
    } else {
        // Doesn't start with '>', shouldn't happen
        return None;
    }

    // Check if we have at least 3 children: "[", "!TYPE", "]"
    if first_para.children.len() < 3 {
        return None;
    }

    // Get the first three nodes
    let first = match &first_para.children[0].kind {
        NodeKind::Text(s) => s.as_ref(),
        _ => return None,
    };

    let second = match &first_para.children[1].kind {
        NodeKind::Text(s) => s.as_ref(),
        _ => return None,
    };

    let third = match &first_para.children[2].kind {
        NodeKind::Text(s) => s.as_ref(),
        _ => return None,
    };

    // Check pattern: first="[", second="!TYPE", third="]"
    if first.trim() != "[" || third.trim() != "]" {
        return None;
    }

    // Extract type from second node (should be "!TYPE")
    if !second.starts_with('!') {
        return None;
    }

    let type_str = &second[1..]; // Remove the "!"

    // Try to resolve the type
    let kind = if let Some(obsidian_kind) =
        ObsidianCalloutKind::from_str(type_str)
    {
        ExtendedBlockQuoteKind::Obsidian(obsidian_kind)
    } else if let Some(custom_kind) = CustomCalloutKind::from_str(type_str) {
        ExtendedBlockQuoteKind::Custom(custom_kind)
    } else {
        // Unknown type - default to Note
        ExtendedBlockQuoteKind::Standard(BlockQuoteKind::Note)
    };

    // Extract foldable state and custom title if present
    // Both come in the 4th child (after "]")
    let (foldable, title) = if first_para.children.len() > 3 {
        // Get the first text node after "]"
        let first_after_bracket = &first_para.children[3];

        if let NodeKind::Text(s) = &first_after_bracket.kind {
            let text = s.as_ref();

            // Check for foldable markers at the start
            let (foldable_state, remaining_text) =
                if let Some(rest) = text.strip_prefix('+') {
                    (Some(FoldableState::Expanded), rest.trim())
                } else if let Some(rest) = text.strip_prefix('-') {
                    (Some(FoldableState::Collapsed), rest.trim())
                } else {
                    (None, text.trim())
                };

            // Collect title text (remaining text in this node + any following text nodes)
            let mut title_parts = Vec::new();
            if !remaining_text.is_empty() {
                title_parts.push(remaining_text);
            }

            // Add any subsequent text nodes before line break
            for child in &first_para.children[4..] {
                match &child.kind {
                    NodeKind::Text(s) => title_parts.push(s.as_ref()),
                    NodeKind::SoftBreak | NodeKind::HardBreak => break,
                    _ => {}
                }
            }

            let title_str = title_parts.join("").trim().to_string();
            let title_opt = if title_str.is_empty() {
                None
            } else {
                Some(title_str)
            };

            (foldable_state, title_opt)
        } else {
            (None, None)
        }
    } else {
        (None, None)
    };

    Some(CalloutMetadata {
        kind,
        title,
        foldable,
    })
}

/// Transform blockquote nodes to detect and annotate extended types
///
/// This walks the AST and detects Obsidian and custom blockquote types.
/// Note: Currently this is just for detection. To actually store the metadata,
/// you would need to extend NodeKind or use a separate data structure.
pub fn transform_custom_blockquotes(node: &mut Node, source_text: &str) {
    // Detect callout metadata
    if let Some(metadata) = detect_callout_metadata(node, source_text) {
        // Store the metadata somehow - we need to extend NodeKind for this
        // For now, we'll demonstrate the detection logic
        eprintln!("Detected callout: {:?}", metadata);
    }

    // Recursively transform children
    for child in &mut node.children {
        transform_custom_blockquotes(child, source_text);
    }
}

/// Get complete callout metadata (type, title, foldable state)
///
/// This is the main function to use when rendering blockquotes.
/// Returns complete metadata for all three categories: Standard, Obsidian, and Custom.
pub fn get_callout_metadata(
    node: &Node,
    source_text: &str,
) -> Option<CalloutMetadata> {
    match &node.kind {
        NodeKind::BlockQuote(Some(standard_kind)) => {
            // Standard types don't have custom titles or foldable state in pulldown-cmark
            Some(CalloutMetadata {
                kind: ExtendedBlockQuoteKind::Standard(*standard_kind),
                title: None,
                foldable: None,
            })
        }
        NodeKind::BlockQuote(None) => {
            detect_callout_metadata(node, source_text)
        }
        _ => None,
    }
}

/// Helper to get just the blockquote type (without title/foldable)
///
/// This is a convenience function for when you only need the type.
pub fn get_blockquote_kind(
    node: &Node,
    source_text: &str,
) -> Option<ExtendedBlockQuoteKind> {
    get_callout_metadata(node, source_text).map(|m| m.kind)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Tree;

    #[test]
    fn test_detect_obsidian_danger_blockquote() {
        let md = r#"> [!ERROR]
> This is an error message"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Danger)
        );
        assert_eq!(metadata.title, None);
        assert_eq!(metadata.foldable, None);
    }

    #[test]
    fn test_detect_custom_llm_blockquote() {
        let md = r#"> [!LLM]
> This content was generated by an LLM"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Custom(CustomCalloutKind::Llm)
        );
        assert_eq!(metadata.title, None);
    }

    #[test]
    fn test_custom_title() {
        // Use an extended type so we can extract custom titles
        let md = r#"> [!INFO] Callouts can have custom titles
> Like this one."#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Info)
        );
        assert_eq!(
            metadata.title,
            Some("Callouts can have custom titles".to_string())
        );
        assert_eq!(metadata.display_title(), "Callouts can have custom titles");
    }

    #[test]
    fn test_extended_custom_title() {
        let md = r#"> [!QUESTION] Custom Question Title
> Content here"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Question)
        );
        assert_eq!(metadata.title, Some("Custom Question Title".to_string()));
        assert_eq!(metadata.display_title(), "Custom Question Title");
    }

    #[test]
    fn test_title_only_callout() {
        let md = r#"> [!INFO] Title-only callout"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Info)
        );
        assert_eq!(metadata.title, Some("Title-only callout".to_string()));
    }

    #[test]
    fn test_foldable_expanded() {
        let md = r#"> [!FAQ]+ Are callouts foldable?
> Yes! They are."#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Question)
        );
        assert_eq!(metadata.title, Some("Are callouts foldable?".to_string()));
        assert_eq!(metadata.foldable, Some(FoldableState::Expanded));
    }

    #[test]
    fn test_foldable_collapsed() {
        let md = r#"> [!FAQ]- Are callouts foldable?
> Yes! In a foldable callout, the contents are hidden when the callout is collapsed."#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Question)
        );
        assert_eq!(metadata.title, Some("Are callouts foldable?".to_string()));
        assert_eq!(metadata.foldable, Some(FoldableState::Collapsed));
    }

    #[test]
    fn test_standard_blockquote_not_detected() {
        let md = r#"> [!NOTE]
> This is a standard note"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        // Should be recognized as standard
        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Standard(BlockQuoteKind::Note)
        );
    }

    #[test]
    fn test_regular_blockquote_not_detected() {
        let md = r#"> This is a regular blockquote
> without any callout marker"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md);
        assert_eq!(metadata, None);
    }

    #[test]
    fn test_two_space_blockquote_not_detected() {
        let md = r#">  [!TIP]
> tip with 2 space -> not a callout, just blockquote"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        // Should not detect due to 2 spaces
        let metadata = get_callout_metadata(blockquote, md);
        assert_eq!(metadata, None);
    }

    #[test]
    fn test_obsidian_aliases() {
        // Test Summary alias for Abstract
        let md = r#"> [!SUMMARY]
> This is a summary"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = get_callout_metadata(blockquote, md).unwrap();
        assert_eq!(
            metadata.kind,
            ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Abstract)
        );
    }

    #[test]
    fn test_get_blockquote_kind_standard() {
        let md = r#"> [!NOTE]
> This is a standard note"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let kind = get_blockquote_kind(blockquote, md);
        assert_eq!(
            kind,
            Some(ExtendedBlockQuoteKind::Standard(BlockQuoteKind::Note))
        );
    }

    #[test]
    fn test_get_blockquote_kind_obsidian() {
        let md = r#"> [!INFO]
> This is an info callout"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let kind = get_blockquote_kind(blockquote, md);
        assert_eq!(
            kind,
            Some(ExtendedBlockQuoteKind::Obsidian(ObsidianCalloutKind::Info))
        );
    }

    #[test]
    fn test_get_blockquote_kind_custom() {
        let md = r#"> [!LLM]
> AI generated"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let kind = get_blockquote_kind(blockquote, md);
        assert_eq!(
            kind,
            Some(ExtendedBlockQuoteKind::Custom(CustomCalloutKind::Llm))
        );
    }
}
