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

    pub fn name(&self) -> &'static str {
        match self {
            Self::Abstract => "abstract",
            Self::Info => "info",
            Self::Todo => "todo",
            Self::Success => "success",
            Self::Question => "question",
            Self::Failure => "failure",
            Self::Danger => "danger",
            Self::Bug => "bug",
            Self::Example => "example",
            Self::Quote => "quote",
        }
    }
}

/// User-defined custom callout types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CustomCalloutKind {
    Llm,
    // more
}

impl CustomCalloutKind {
    /// Parse a custom callout type from a string
    fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "LLM" => Some(Self::Llm),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Self::Llm => "llm",
        }
    }
}

/// Extended blockquote kind that includes standard, Obsidian, and custom types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CalloutKind {
    /// Standard GFM alert types (recognized by pulldown-cmark)
    GFM(BlockQuoteKind),
    /// Obsidian-specific callout types
    Obsidian(ObsidianCalloutKind),
    /// User-defined custom callout types
    Custom(CustomCalloutKind),
}

impl TryFrom<&str> for CalloutKind {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        if let Some(gfm_kind) = match s.to_uppercase().as_str() {
            "NOTE" => Some(BlockQuoteKind::Note),
            "TIP" => Some(BlockQuoteKind::Tip),
            "IMPORTANT" => Some(BlockQuoteKind::Important),
            "WARNING" => Some(BlockQuoteKind::Warning),
            "CAUTION" => Some(BlockQuoteKind::Caution),
            _ => None,
        } {
            Ok(Self::GFM(gfm_kind))
        } else if let Some(obsidian_kind) = ObsidianCalloutKind::from_str(s) {
            Ok(Self::Obsidian(obsidian_kind))
        } else if let Some(custom_kind) = CustomCalloutKind::from_str(s) {
            Ok(Self::Custom(custom_kind))
        } else {
            Err(format!("Unknown callout type: {}", s))
        }
    }
}

impl CalloutKind {
    pub fn name(&self) -> &'static str {
        match self {
            Self::GFM(BlockQuoteKind::Note) => "note",
            Self::GFM(BlockQuoteKind::Tip) => "tip",
            Self::GFM(BlockQuoteKind::Important) => "important",
            Self::GFM(BlockQuoteKind::Warning) => "warning",
            Self::GFM(BlockQuoteKind::Caution) => "caution",
            Self::Obsidian(obsidian) => obsidian.name(),
            Self::Custom(custom) => custom.name(),
        }
    }

    /// Get default title for this callout type (title case)
    pub fn default_title(&self) -> &'static str {
        match self {
            Self::GFM(BlockQuoteKind::Note) => "Note",
            Self::GFM(BlockQuoteKind::Tip) => "Tip",
            Self::GFM(BlockQuoteKind::Important) => "Important",
            Self::GFM(BlockQuoteKind::Warning) => "Warning",
            Self::GFM(BlockQuoteKind::Caution) => "Caution",
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
pub struct CalloutData {
    /// The type of callout
    pub kind: CalloutKind,
    /// Custom title (None means use default)
    pub title: Option<String>,
    /// Whether the callout is foldable and its default state
    pub foldable: Option<FoldableState>,
    pub content_start_byte: usize,
}

impl CalloutData {
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
/// Title is text after `[!TYPE(+|-)]` and until the softbreak, hardbreak, or end of
/// paragraph node
pub fn callout_data_of_gfm_blockquote(
    node: &Node,
    source_text: &str,
) -> Option<CalloutData> {
    // Only process blockquote nodes
    if !matches!(&node.kind, NodeKind::BlockQuote) {
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
    let kind = if let Ok(kind) = CalloutKind::try_from(type_str) {
        kind
    } else {
        // Unknown type - default to Note
        CalloutKind::GFM(BlockQuoteKind::Note)
    };

    // Extract foldable state and custom title if present
    // Both come in the 4th child (after "]")
    let (foldable, title, content_start_byte) = if first_para.children.len() > 3
    {
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

            // Track the last node index used for title, to find content start
            let mut last_title_node_idx = 3;

            // Add any subsequent text nodes before line break
            for (idx, child) in first_para.children[4..].iter().enumerate() {
                match &child.kind {
                    NodeKind::Text(s) => {
                        title_parts.push(s.as_ref());
                        last_title_node_idx = 4 + idx;
                    }
                    NodeKind::SoftBreak | NodeKind::HardBreak => {
                        // Content starts after this line break
                        last_title_node_idx = 4 + idx;
                        break;
                    }
                    _ => {}
                }
            }

            let title_str = title_parts.join("").trim().to_string();
            let title_opt = if title_str.is_empty() {
                None
            } else {
                Some(title_str)
            };

            // Calculate content start byte
            // Content starts after the last node used for title/break
            let content_byte =
                if last_title_node_idx + 1 < first_para.children.len() {
                    // There are more children after the title line, content starts at next node
                    first_para.children[last_title_node_idx + 1].start_byte
                } else {
                    // No more children in first paragraph, content starts after it
                    first_para.end_byte
                };

            (foldable_state, title_opt, content_byte)
        } else {
            // No text after bracket, content starts after this node
            let content_byte = first_after_bracket.end_byte;
            (None, None, content_byte)
        }
    } else {
        // Only the [!TYPE] marker, content starts after the "]" node (index 2)
        let content_byte = first_para.children[2].end_byte;
        (None, None, content_byte)
    };

    Some(CalloutData {
        kind,
        title,
        foldable,
        content_start_byte,
    })
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
/// Title is text after `[!TYPE(+|-)]` and until the softbreak, hardbreak, or end of
/// paragraph node
pub fn callout_node_of_gfm_blockquote<'a>(
    node: &Node<'a>,
    source_text: &str,
) -> Option<Node<'a>> {
    // Only process blockquote nodes
    if !matches!(&node.kind, NodeKind::BlockQuote) {
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
    let kind = if let Ok(kind) = CalloutKind::try_from(type_str) {
        kind
    } else {
        // Unknown type - default to Note
        CalloutKind::GFM(BlockQuoteKind::Note)
    };

    // Extract foldable state and custom title if present
    // Both come in the 4th child (after "]")
    let (foldable, title, content_start_byte) = if first_para.children.len() > 3
    {
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

            // Track the last node index used for title, to find content start
            let mut last_title_node_idx = 3;

            // Add any subsequent text nodes before line break
            for (idx, child) in first_para.children[4..].iter().enumerate() {
                match &child.kind {
                    NodeKind::Text(s) => {
                        title_parts.push(s.as_ref());
                        last_title_node_idx = 4 + idx;
                    }
                    NodeKind::SoftBreak | NodeKind::HardBreak => {
                        // Content starts after this line break
                        last_title_node_idx = 4 + idx;
                        break;
                    }
                    _ => {}
                }
            }

            let title_str = title_parts.join("").trim().to_string();
            let title_opt = if title_str.is_empty() {
                None
            } else {
                Some(title_str)
            };

            // Calculate content start byte
            // Content starts after the last node used for title/break
            let content_byte =
                if last_title_node_idx + 1 < first_para.children.len() {
                    // There are more children after the title line, content starts at next node
                    first_para.children[last_title_node_idx + 1].start_byte
                } else {
                    // No more children in first paragraph, content starts after it
                    first_para.end_byte
                };

            (foldable_state, title_opt, content_byte)
        } else {
            // No text after bracket, content starts after this node
            let content_byte = first_after_bracket.end_byte;
            (None, None, content_byte)
        }
    } else {
        // Only the [!TYPE] marker, content starts after the "]" node (index 2)
        let content_byte = first_para.children[2].end_byte;
        (None, None, content_byte)
    };

    let _ = Some(CalloutData {
        kind,
        title,
        foldable,
        content_start_byte,
    });
    todo!()
}

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    use super::*;
    use crate::ast::Tree;

    #[test]
    fn test_detect_obsidian_danger_blockquote() {
        let md = r#"> [!ERROR]
> This is an error message"#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];

        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Danger), title: None, foldable: None, content_start_byte: 11 } [0..37]
          Paragraph [2..37]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!ERROR")) [3..9]
            Text(Borrowed("]")) [9..10]
            SoftBreak [10..11]
            Text(Borrowed("This is an error message")) [13..37]
        "#);
    }

    #[test]
    fn test_detect_custom_llm_blockquote() {
        let md = r#"> [!LLM]
> This content was generated by an LLM"#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];

        assert_snapshot!(callout_node, @r#"
        Callout { kind: Custom(Llm), title: None, foldable: None, content_start_byte: 9 } [0..47]
          Paragraph [2..47]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!LLM")) [3..7]
            Text(Borrowed("]")) [7..8]
            SoftBreak [8..9]
            Text(Borrowed("This content was generated by an LLM")) [11..47]
        "#);
    }

    #[test]
    fn test_custom_title() {
        // Use an extended type so we can extract custom titles
        let md = r#"> [!INFO] Callouts can have custom titles
> Like this one."#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Info), title: Some("Callouts can have custom titles"), foldable: None, content_start_byte: 44 } [0..58]
          Paragraph [2..58]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!INFO")) [3..8]
            Text(Borrowed("]")) [8..9]
            Text(Borrowed(" Callouts can have custom titles")) [9..41]
            SoftBreak [41..42]
            Text(Borrowed("Like this one.")) [44..58]
        "#);
    }

    #[test]
    fn test_extended_custom_title() {
        let md = r#"> [!QUESTION] Custom Question Title
> Content here"#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Question), title: Some("Custom Question Title"), foldable: None, content_start_byte: 38 } [0..50]
          Paragraph [2..50]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!QUESTION")) [3..12]
            Text(Borrowed("]")) [12..13]
            Text(Borrowed(" Custom Question Title")) [13..35]
            SoftBreak [35..36]
            Text(Borrowed("Content here")) [38..50]
        "#);
    }

    #[test]
    fn test_title_only_callout() {
        let md = r#"> [!INFO] Title-only callout"#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Info), title: Some("Title-only callout"), foldable: None, content_start_byte: 28 } [0..28]
          Paragraph [2..28]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!INFO")) [3..8]
            Text(Borrowed("]")) [8..9]
            Text(Borrowed(" Title-only callout")) [9..28]
        "#);
    }

    #[test]
    fn test_foldable_expanded() {
        let md = r#"> [!FAQ]+ Are callouts foldable?
> Yes! They are."#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Question), title: Some("Are callouts foldable?"), foldable: Some(Expanded), content_start_byte: 35 } [0..49]
          Paragraph [2..49]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!FAQ")) [3..7]
            Text(Borrowed("]")) [7..8]
            Text(Borrowed("+ Are callouts foldable?")) [8..32]
            SoftBreak [32..33]
            Text(Borrowed("Yes! They are.")) [35..49]
        "#);
    }

    #[test]
    fn test_foldable_collapsed() {
        let md = r#"> [!FAQ]- Are callouts foldable?
> Yes! In a foldable callout, the contents are hidden when the callout is collapsed."#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Question), title: Some("Are callouts foldable?"), foldable: Some(Collapsed), content_start_byte: 35 } [0..117]
          Paragraph [2..117]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!FAQ")) [3..7]
            Text(Borrowed("]")) [7..8]
            Text(Borrowed("- Are callouts foldable?")) [8..32]
            SoftBreak [32..33]
            Text(Borrowed("Yes! In a foldable callout, the contents are hidden when the callout is collapsed.")) [35..117]
        "#);
    }

    #[test]
    fn test_standard_blockquote_not_detected() {
        let md = r#"> [!NOTE]
> This is a standard note"#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];
        assert_snapshot!(callout_node, @r#"
        Callout { kind: GFM(Note), title: None, foldable: None, content_start_byte: 10 } [0..35]
          Paragraph [2..35]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!NOTE")) [3..8]
            Text(Borrowed("]")) [8..9]
            SoftBreak [9..10]
            Text(Borrowed("This is a standard note")) [12..35]
        "#);
    }

    #[test]
    fn test_regular_blockquote_not_detected() {
        let md = r#"> This is a regular blockquote
> without any callout marker"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        let metadata = callout_data_of_gfm_blockquote(blockquote, md);
        assert_eq!(metadata, None);
    }

    #[test]
    fn test_two_space_blockquote_not_detected() {
        let md = r#">  [!TIP]
> tip with 2 space -> not a callout, just blockquote"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        // Should not detect due to 2 spaces
        let metadata = callout_data_of_gfm_blockquote(blockquote, md);
        assert_eq!(metadata, None);
    }

    #[test]
    fn test_obsidian_aliases() {
        // Test Summary alias for Abstract
        let md = r#"> [!SUMMARY]
> This is a summary"#;

        let tree = Tree::new(md);
        let callout_node = &tree.root_node.children[0];

        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Abstract), title: None, foldable: None, content_start_byte: 13 } [0..32]
          Paragraph [2..32]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!SUMMARY")) [3..11]
            Text(Borrowed("]")) [11..12]
            SoftBreak [12..13]
            Text(Borrowed("This is a summary")) [15..32]
        "#);
    }
}
