use crate::ast::tree::setup_parent_pointers;

use super::node::{Node, NodeKind};
use pulldown_cmark::BlockQuoteKind;
use regex::Regex;

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

    fn from_str_or_default(s: &str) -> Self {
        if let Ok(kind) = CalloutKind::try_from(s) {
            kind
        } else {
            CalloutKind::GFM(BlockQuoteKind::Note)
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

impl From<bool> for FoldableState {
    fn from(b: bool) -> Self {
        if b {
            FoldableState::Expanded
        } else {
            FoldableState::Collapsed
        }
    }
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

fn parse_callout_decl(
    line: &str,
) -> Option<(String, Option<bool>, Option<String>)> {
    let re =
        Regex::new(r"(?i)^>\s?\[!([A-Za-z]+)\]([+-])?(?:\s+(.+))?$").unwrap();

    re.captures(line).map(|caps| {
        let type_name = caps.get(1).unwrap().as_str().to_string();
        let foldable = caps.get(2).map(|m| m.as_str() == "+");
        let title = caps
            .get(3)
            .map(|m| m.as_str().to_string())
            .filter(|s| !s.trim().is_empty());

        (type_name, foldable, title)
    })
}

/// Detect callout metadata by examining the first paragraph
///
/// Pattern: `[!TYPE(+|-)] optional title`
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
    md_src: &str,
) -> Option<Node<'a>> {
    // Only process blockquote nodes
    if !matches!(&node.kind, NodeKind::BlockQuote) {
        return None;
    }

    // Find first paragraph child
    let fst_para = node.children.first()?;
    if !matches!(&fst_para.kind, NodeKind::Paragraph) {
        return None;
    }

    // The end byte of first line is
    // - the end byte of the last child before softbreak / hardbreak
    // - or the end of the last node of the children of paragraph node
    //
    // - fst_para_content_children_start_idx
    //   - the index of the paragraph's children, which is the first child belongs
    //     to callout content
    //   - None if first paragraph has no children belonging to callout content
    //   - Used in later splits of callout declaration and content
    //
    // Post: the end-byte is always the end byte of one of the first paragraph's
    // children
    let (fst_para_content_children_start_idx, fst_line_end_byte) = {
        // Find first softbreak or hardbreak
        let fst_break_idx_and_node = fst_para
            .children
            .iter()
            .enumerate()
            .filter(|(_, c)| {
                matches!(&c.kind, NodeKind::SoftBreak | NodeKind::HardBreak)
            })
            .next();

        let fst_line_end_type = match fst_break_idx_and_node {
            Some((break_node_idx, _)) => {
                debug_assert!(
                    break_node_idx != 0,
                    "Softbreak or hardbreak cannot be first child, idx: {}",
                    break_node_idx
                );
                // Get the end byte of the last child before the break
                fst_para.child(break_node_idx - 1).unwrap().end_byte
            }
            None => {
                // No softbreak or hardbreak, get the end byte of the last child
                fst_para.children.last().unwrap().end_byte
            }
        };
        let fst_para_content_children_start_idx = {
            if let Some((break_node_idx, _)) = fst_break_idx_and_node {
                if break_node_idx == fst_para.child_count() {
                    None
                } else {
                    Some(break_node_idx + 1)
                }
            } else {
                None
            }
        };
        (fst_para_content_children_start_idx, fst_line_end_type)
    };

    let decl_text = md_src
        .get(node.start_byte..fst_line_end_byte)
        .expect("Never");

    if let Some((type_name, foldable, title)) = parse_callout_decl(decl_text) {
        let mut callout_decl_children = Vec::<Node>::new();
        let mut callout_content_children = Vec::<Node>::new();

        match fst_para_content_children_start_idx {
            Some(fst_para_content_children_start_idx) => {
                // Decl
                let decl_para_children = fst_para.children
                    [0..fst_para_content_children_start_idx]
                    .to_vec();
                let decl_para_last_child = decl_para_children.last().unwrap();
                let decl_para = Node {
                    kind: NodeKind::Paragraph,
                    start_byte: fst_para.start_byte,
                    end_byte: decl_para_last_child.end_byte,
                    start_point: fst_para.start_point,
                    end_point: decl_para_last_child.end_point,
                    children: decl_para_children,
                    parent: None, // Set later
                };
                callout_decl_children.push(decl_para);

                // Content
                let content_para_children = fst_para.children
                    [fst_para_content_children_start_idx
                        ..fst_para.children.len()]
                    .to_vec();
                let content_para_first_child =
                    content_para_children.first().unwrap();
                let content_para = Node {
                    kind: NodeKind::Paragraph,
                    start_byte: content_para_first_child.start_byte,
                    end_byte: fst_para.end_byte,
                    start_point: content_para_first_child.start_point,
                    end_point: fst_para.end_point,
                    children: content_para_children,
                    parent: None, // Set later
                };
                callout_content_children.push(content_para);
                // The rest of blockquote's children
                if node.child_count() > 1 {
                    callout_content_children
                        .extend(node.children[1..node.child_count()].to_vec())
                }
            }
            None => {
                // The first paragraph is the decl
                callout_decl_children.push(fst_para.clone());
                if node.child_count() > 1 {
                    // The rest of blockquote's children are the content
                    callout_content_children
                        .extend(node.children[1..node.child_count()].to_vec())
                }
            }
        }

        let callout_decl = Node {
            kind: NodeKind::CalloutDeclaraion,
            start_byte: fst_para.start_byte,
            start_point: fst_para.start_point,
            end_byte: callout_decl_children.last().unwrap().end_byte,
            end_point: callout_decl_children.last().unwrap().end_point,
            children: callout_decl_children,
            parent: None, // Set later
        };
        let callout_content = if callout_content_children.is_empty() {
            None
        } else {
            Some(Node {
                kind: NodeKind::CalloutContent,
                start_byte: callout_content_children
                    .first()
                    .unwrap()
                    .start_byte,
                start_point: callout_content_children
                    .first()
                    .unwrap()
                    .start_point,
                end_byte: node.children.last().unwrap().end_byte,
                end_point: node.children.last().unwrap().end_point,
                children: callout_content_children,
                parent: None, // Set later
            })
        };

        let mut callout = Node {
            kind: NodeKind::Callout {
                kind: CalloutKind::from_str_or_default(&type_name),
                title,
                foldable: foldable.map(|b| b.into()),
            },
            start_byte: node.start_byte,
            start_point: node.start_point,
            end_byte: node.end_byte,
            end_point: node.end_point,
            children: {
                if let Some(callout_content) = callout_content {
                    vec![callout_decl, callout_content]
                } else {
                    vec![callout_decl]
                }
            },
            parent: None, // Set later
        };

        setup_parent_pointers(&mut callout);
        Some(callout)
    } else {
        return None;
    }
}

#[cfg(test)]
mod decl_parse_tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("> [!NOTE]", "NOTE", None, None)]
    #[case("> [!WARNING]", "WARNING", None, None)]
    #[case(">[!TIP]", "TIP", None, None)]
    #[case("> [!note]", "note", None, None)]
    #[case("> [!Note]", "Note", None, None)]
    #[case("> [!WaRnInG]", "WaRnInG", None, None)]
    fn test_valid_callouts_no_title(
        #[case] input: &str,
        #[case] expected_type: &str,
        #[case] expected_foldable: Option<bool>,
        #[case] expected_title: Option<&str>,
    ) {
        let result = parse_callout_decl(input);
        assert_eq!(
            result,
            Some((
                expected_type.to_string(),
                expected_foldable,
                expected_title.map(|s| s.to_string())
            ))
        );
    }

    #[rstest]
    #[case("> [!NOTE] This is a title", "NOTE", None, Some("This is a title"))]
    #[case("> [!WARNING] Be careful!", "WARNING", None, Some("Be careful!"))]
    #[case(">[!TIP] Quick tip", "TIP", None, Some("Quick tip"))]
    #[case(
        "> [!note] lowercase with title",
        "note",
        None,
        Some("lowercase with title")
    )]
    #[case(
        "> [!INFO]   Multiple   spaces",
        "INFO",
        None,
        Some("Multiple   spaces")
    )]
    #[case(
        "> [!TIP] Title: with-special_chars!",
        "TIP",
        None,
        Some("Title: with-special_chars!")
    )]
    fn test_valid_callouts_with_title(
        #[case] input: &str,
        #[case] expected_type: &str,
        #[case] expected_foldable: Option<bool>,
        #[case] expected_title: Option<&str>,
    ) {
        let result = parse_callout_decl(input);
        assert_eq!(
            result,
            Some((
                expected_type.to_string(),
                expected_foldable,
                expected_title.map(|s| s.to_string())
            ))
        );
    }

    #[rstest]
    #[case(">[!WARNING]+", "WARNING", Some(true), None)]
    #[case("> [!ERROR]-", "ERROR", Some(false), None)]
    #[case("> [!info]+", "info", Some(true), None)]
    #[case(">[!DANGER]-", "DANGER", Some(false), None)]
    fn test_valid_callouts_foldable_no_title(
        #[case] input: &str,
        #[case] expected_type: &str,
        #[case] expected_foldable: Option<bool>,
        #[case] expected_title: Option<&str>,
    ) {
        let result = parse_callout_decl(input);
        assert_eq!(
            result,
            Some((
                expected_type.to_string(),
                expected_foldable,
                expected_title.map(|s| s.to_string())
            ))
        );
    }

    #[rstest]
    #[case("> [!INFO]+ Expanded", "INFO", Some(true), Some("Expanded"))]
    #[case("> [!DANGER]- Collapsed", "DANGER", Some(false), Some("Collapsed"))]
    #[case(
        ">[!warning]+ Be careful",
        "warning",
        Some(true),
        Some("Be careful")
    )]
    #[case(
        "> [!TiP]- Collapsed tip",
        "TiP",
        Some(false),
        Some("Collapsed tip")
    )]
    fn test_valid_callouts_foldable_with_title(
        #[case] input: &str,
        #[case] expected_type: &str,
        #[case] expected_foldable: Option<bool>,
        #[case] expected_title: Option<&str>,
    ) {
        let result = parse_callout_decl(input);
        assert_eq!(
            result,
            Some((
                expected_type.to_string(),
                expected_foldable,
                expected_title.map(|s| s.to_string())
            ))
        );
    }

    #[rstest]
    #[case("> [!NOTE]   ", "NOTE", None, None, "whitespace only title")]
    fn test_whitespace_title_filtered(
        #[case] input: &str,
        #[case] expected_type: &str,
        #[case] expected_foldable: Option<bool>,
        #[case] expected_title: Option<&str>,
        #[case] _description: &str,
    ) {
        let result = parse_callout_decl(input);
        assert_eq!(
            result,
            Some((
                expected_type.to_string(),
                expected_foldable,
                expected_title.map(|s| s.to_string())
            ))
        );
    }

    #[rstest]
    #[case(">  [!NOTE]", "two spaces after >")]
    #[case(">   [!WARNING] Title", "multiple spaces after >")]
    #[case("> [NOTE]", "missing exclamation")]
    #[case("> !NOTE", "missing brackets")]
    #[case("[!NOTE] Title", "no greater than")]
    #[case("> [!NOTE]* Title", "invalid foldable marker")]
    #[case("> [!]", "empty type")]
    #[case("> [!NOTE123]", "numbers in type")]
    #[case(" > [!NOTE]", "space before greater than")]
    #[case("> [!NOTE] Title\nMore", "content after newline")]
    #[case("> [!123]", "type is all numbers")]
    #[case("> [!NO-TE]", "hyphen in type")]
    #[case("> [!NO_TE]", "underscore in type")]
    fn test_invalid_callouts(#[case] input: &str, #[case] _description: &str) {
        let result = parse_callout_decl(input);
        assert_eq!(result, None);
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    use super::*;
    use crate::ast::Tree;

    fn assert_range_match(blockquote: &Node, callout_node: &Node) -> () {
        if blockquote.start_byte != callout_node.start_byte {
            panic!(
                "Expected start_byte of blockquote to match start_byte of callout node, but they differ: {} != {}",
                blockquote.start_byte, callout_node.start_byte
            );
        }
        if blockquote.end_byte != callout_node.end_byte {
            panic!(
                "Expected end_byte of blockquote to match end_byte of callout node, but they differ: {} != {}",
                blockquote.end_byte, callout_node.end_byte
            );
        }
        if blockquote.start_point != callout_node.start_point {
            panic!(
                "Expected start_point of blockquote to match start_point of callout node, but they differ: {:?} != {:?}",
                blockquote.start_point, callout_node.start_point
            );
        }
        if blockquote.end_point != callout_node.end_point {
            panic!(
                "Expected end_point of blockquote to match end_point of callout node, but they differ: {:?} != {:?}",
                blockquote.end_point, callout_node.end_point
            );
        }
    }

    #[test]
    fn test_detect_obsidian_danger_blockquote() {
        let md = r#"> [!ERROR]
> This is an error message"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..37]
          Paragraph [2..37]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!ERROR")) [3..9]
            Text(Borrowed("]")) [9..10]
            SoftBreak [10..11]
            Text(Borrowed("This is an error message")) [13..37]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();

        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Danger), title: None, foldable: None } [0..37]
          CalloutDeclaraion [2..11]
            Paragraph [2..11]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!ERROR")) [3..9]
              Text(Borrowed("]")) [9..10]
              SoftBreak [10..11]
          CalloutContent [13..37]
            Paragraph [13..37]
              Text(Borrowed("This is an error message")) [13..37]
        "#);
    }

    #[test]
    fn test_detect_custom_llm_blockquote() {
        let md = r#"> [!LLM]
> This content was generated by an LLM"#;
        let tree = Tree::new(md);

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..47]
          Paragraph [2..47]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!LLM")) [3..7]
            Text(Borrowed("]")) [7..8]
            SoftBreak [8..9]
            Text(Borrowed("This content was generated by an LLM")) [11..47]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Custom(Llm), title: None, foldable: None } [0..47]
          CalloutDeclaraion [2..9]
            Paragraph [2..9]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!LLM")) [3..7]
              Text(Borrowed("]")) [7..8]
              SoftBreak [8..9]
          CalloutContent [11..47]
            Paragraph [11..47]
              Text(Borrowed("This content was generated by an LLM")) [11..47]
        "#);
    }

    #[test]
    fn test_custom_title() {
        let md = r#"> [!INFO] Callouts can have custom titles
> Like this one."#;
        let tree = Tree::new(md);

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..58]
          Paragraph [2..58]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!INFO")) [3..8]
            Text(Borrowed("]")) [8..9]
            Text(Borrowed(" Callouts can have custom titles")) [9..41]
            SoftBreak [41..42]
            Text(Borrowed("Like this one.")) [44..58]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Info), title: Some("Callouts can have custom titles"), foldable: None } [0..58]
          CalloutDeclaraion [2..42]
            Paragraph [2..42]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!INFO")) [3..8]
              Text(Borrowed("]")) [8..9]
              Text(Borrowed(" Callouts can have custom titles")) [9..41]
              SoftBreak [41..42]
          CalloutContent [44..58]
            Paragraph [44..58]
              Text(Borrowed("Like this one.")) [44..58]
        "#);
    }

    #[test]
    fn test_extended_custom_title() {
        let md = r#"> [!QUESTION] Custom Question Title
> Content here"#;
        let tree = Tree::new(md);

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..50]
          Paragraph [2..50]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!QUESTION")) [3..12]
            Text(Borrowed("]")) [12..13]
            Text(Borrowed(" Custom Question Title")) [13..35]
            SoftBreak [35..36]
            Text(Borrowed("Content here")) [38..50]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Question), title: Some("Custom Question Title"), foldable: None } [0..50]
          CalloutDeclaraion [2..36]
            Paragraph [2..36]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!QUESTION")) [3..12]
              Text(Borrowed("]")) [12..13]
              Text(Borrowed(" Custom Question Title")) [13..35]
              SoftBreak [35..36]
          CalloutContent [38..50]
            Paragraph [38..50]
              Text(Borrowed("Content here")) [38..50]
        "#);
    }

    #[test]
    fn test_title_only_callout() {
        let md = r#"> [!INFO] Title-only callout"#;
        let tree = Tree::new(md);

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..28]
          Paragraph [2..28]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!INFO")) [3..8]
            Text(Borrowed("]")) [8..9]
            Text(Borrowed(" Title-only callout")) [9..28]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Info), title: Some("Title-only callout"), foldable: None } [0..28]
          CalloutDeclaraion [2..28]
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

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..49]
          Paragraph [2..49]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!FAQ")) [3..7]
            Text(Borrowed("]")) [7..8]
            Text(Borrowed("+ Are callouts foldable?")) [8..32]
            SoftBreak [32..33]
            Text(Borrowed("Yes! They are.")) [35..49]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Question), title: Some("Are callouts foldable?"), foldable: Some(Expanded) } [0..49]
          CalloutDeclaraion [2..33]
            Paragraph [2..33]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!FAQ")) [3..7]
              Text(Borrowed("]")) [7..8]
              Text(Borrowed("+ Are callouts foldable?")) [8..32]
              SoftBreak [32..33]
          CalloutContent [35..49]
            Paragraph [35..49]
              Text(Borrowed("Yes! They are.")) [35..49]
        "#);
    }

    #[test]
    fn test_foldable_collapsed() {
        let md = r#"> [!FAQ]- Are callouts foldable?
> Yes! In a foldable callout, the contents are hidden when the callout is collapsed."#;
        let tree = Tree::new(md);

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..117]
          Paragraph [2..117]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!FAQ")) [3..7]
            Text(Borrowed("]")) [7..8]
            Text(Borrowed("- Are callouts foldable?")) [8..32]
            SoftBreak [32..33]
            Text(Borrowed("Yes! In a foldable callout, the contents are hidden when the callout is collapsed.")) [35..117]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Question), title: Some("Are callouts foldable?"), foldable: Some(Collapsed) } [0..117]
          CalloutDeclaraion [2..33]
            Paragraph [2..33]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!FAQ")) [3..7]
              Text(Borrowed("]")) [7..8]
              Text(Borrowed("- Are callouts foldable?")) [8..32]
              SoftBreak [32..33]
          CalloutContent [35..117]
            Paragraph [35..117]
              Text(Borrowed("Yes! In a foldable callout, the contents are hidden when the callout is collapsed.")) [35..117]
        "#);
    }

    #[test]
    fn test_standard_blockquote() {
        let md = r#"> [!NOTE]
> This is a standard note"#;
        let tree = Tree::new(md);

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..35]
          Paragraph [2..35]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!NOTE")) [3..8]
            Text(Borrowed("]")) [8..9]
            SoftBreak [9..10]
            Text(Borrowed("This is a standard note")) [12..35]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: GFM(Note), title: None, foldable: None } [0..35]
          CalloutDeclaraion [2..10]
            Paragraph [2..10]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!NOTE")) [3..8]
              Text(Borrowed("]")) [8..9]
              SoftBreak [9..10]
          CalloutContent [12..35]
            Paragraph [12..35]
              Text(Borrowed("This is a standard note")) [12..35]
        "#);
    }

    #[test]
    fn test_regular_blockquote_not_detected() {
        let md = r#"> This is a regular blockquote
> without any callout marker"#;
        let tree = Tree::new(md);

        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..59]
          Paragraph [2..59]
            Text(Borrowed("This is a regular blockquote")) [2..30]
            SoftBreak [30..31]
            Text(Borrowed("without any callout marker")) [33..59]
        "#);

        let callout_node = callout_node_of_gfm_blockquote(blockquote, md);
        assert_eq!(callout_node, None);
    }

    #[test]
    fn test_two_space_blockquote_not_detected() {
        let md = r#">  [!TIP]
> tip with 2 space -> not a callout, just blockquote"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];

        // Should not detect due to 2 spaces
        let callout_node = callout_node_of_gfm_blockquote(blockquote, md);
        assert_eq!(callout_node, None);
    }

    #[test]
    fn test_obsidian_aliases() {
        // Test Summary alias for Abstract
        let md = r#"> [!SUMMARY]
> This is a summary"#;

        let tree = Tree::new(md);
        let blockquote = &tree.root_node.children[0];
        assert_snapshot!(&blockquote, @r#"
        BlockQuote [0..32]
          Paragraph [2..32]
            Text(Borrowed("[")) [2..3]
            Text(Borrowed("!SUMMARY")) [3..11]
            Text(Borrowed("]")) [11..12]
            SoftBreak [12..13]
            Text(Borrowed("This is a summary")) [15..32]
        "#);

        let callout_node =
            callout_node_of_gfm_blockquote(blockquote, md).unwrap();
        assert_snapshot!(callout_node, @r#"
        Callout { kind: Obsidian(Abstract), title: None, foldable: None } [0..32]
          CalloutDeclaraion [2..13]
            Paragraph [2..13]
              Text(Borrowed("[")) [2..3]
              Text(Borrowed("!SUMMARY")) [3..11]
              Text(Borrowed("]")) [11..12]
              SoftBreak [12..13]
          CalloutContent [15..32]
            Paragraph [15..32]
              Text(Borrowed("This is a summary")) [15..32]
        "#);
    }
}
