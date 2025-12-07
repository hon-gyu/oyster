//! Node definitions for the Markdown AST

use crate::validate;
use nonempty;
use std::fmt::Display;
use std::ops::Range;
use tree_sitter::Point;

use super::callout::{CalloutKind, FoldableState};
use pulldown_cmark::{
    Alignment, CodeBlockKind, CowStr, Event, HeadingLevel, LinkType,
    MetadataBlockKind, Tag,
};

// Node
// ====================

#[derive(Clone, Debug, PartialEq)]
pub struct Node<'a> {
    pub children: Vec<Node<'a>>,
    // byte range of the inline element or the entire element span
    // for nested elements
    pub start_byte: usize,
    pub end_byte: usize,
    // position in rows and columns
    pub start_point: Point,
    pub end_point: Point,
    pub kind: NodeKind<'a>,
    pub(crate) parent: Option<*const Node<'a>>,
}

// Node kind
// --------------------

/// Node kind
///
/// pulldown-cmark's `Event` consists of element that can be nested,
/// representing as Event::Start(Tag) and Event::End(Tag), and event that
/// can't be nested.
///
/// including Tag + Event that is not start or end
#[derive(Clone, Debug, PartialEq)]
pub enum NodeKind<'a> {
    /// Root node
    Document,
    /// Tag
    Paragraph,
    Heading {
        level: HeadingLevel,
        id: Option<CowStr<'a>>,
        classes: Vec<CowStr<'a>>,
        attrs: Vec<(CowStr<'a>, Option<CowStr<'a>>)>,
    },
    // We don't enable GFM during pulldown-cmark parsing
    BlockQuote,
    Callout {
        /// The type of callout
        kind: CalloutKind,
        /// Custom title (None means use default)
        title: Option<String>,
        /// Whether the callout is foldable and its default state
        foldable: Option<FoldableState>,
        /// Byte offset where the callout content starts (after type declaration and title)
        content_start_byte: usize,
    },
    CodeBlock(CodeBlockKind<'a>),
    HtmlBlock,
    List(Option<u64>),
    Item,
    FootnoteDefinition(CowStr<'a>),
    DefinitionList,
    DefinitionListTitle,
    DefinitionListDefinition,
    Table(Vec<Alignment>),
    TableHead,
    TableRow,
    TableCell,
    Emphasis,
    Strong,
    Strikethrough,
    Superscript,
    Subscript,
    Link {
        link_type: LinkType,
        dest_url: CowStr<'a>,
        title: CowStr<'a>,
        id: CowStr<'a>,
    },
    Image {
        link_type: LinkType,
        dest_url: CowStr<'a>,
        title: CowStr<'a>,
        id: CowStr<'a>,
    },
    MetadataBlock(MetadataBlockKind),
    /// inline Events
    Text(CowStr<'a>),
    Code(CowStr<'a>),
    InlineMath(CowStr<'a>),
    DisplayMath(CowStr<'a>),
    Html(CowStr<'a>),
    InlineHtml(CowStr<'a>),
    FootnoteReference(CowStr<'a>),
    SoftBreak,
    HardBreak,
    Rule,
    TaskListMarker(bool),
}

impl<'a> NodeKind<'a> {
    pub(crate) fn from_tag(tag: Tag<'a>) -> Self {
        match tag {
            Tag::Paragraph => Self::Paragraph,
            Tag::Heading {
                level,
                id,
                classes,
                attrs,
            } => Self::Heading {
                level,
                id,
                classes,
                attrs,
            },
            Tag::BlockQuote(_) => Self::BlockQuote,
            Tag::CodeBlock(x) => Self::CodeBlock(x),
            Tag::HtmlBlock => Self::HtmlBlock,
            Tag::List(x) => Self::List(x),
            Tag::Item => Self::Item,
            Tag::FootnoteDefinition(x) => Self::FootnoteDefinition(x),
            Tag::DefinitionList => Self::DefinitionList,
            Tag::DefinitionListTitle => Self::DefinitionListTitle,
            Tag::DefinitionListDefinition => Self::DefinitionListDefinition,
            Tag::Table(x) => Self::Table(x),
            Tag::TableHead => Self::TableHead,
            Tag::TableRow => Self::TableRow,
            Tag::TableCell => Self::TableCell,
            Tag::Emphasis => Self::Emphasis,
            Tag::Strong => Self::Strong,
            Tag::Strikethrough => Self::Strikethrough,
            Tag::Superscript => Self::Superscript,
            Tag::Subscript => Self::Subscript,
            Tag::Link {
                link_type,
                dest_url,
                title,
                id,
            } => Self::Link {
                link_type,
                dest_url,
                title,
                id,
            },
            Tag::Image {
                link_type,
                dest_url,
                title,
                id,
            } => Self::Image {
                link_type,
                dest_url,
                title,
                id,
            },
            Tag::MetadataBlock(x) => Self::MetadataBlock(x),
        }
    }

    pub(crate) fn from_event(event: Event<'a>) -> Self {
        match event {
            Event::Text(x) => Self::Text(x),
            Event::Code(x) => Self::Code(x),
            Event::InlineMath(x) => Self::InlineMath(x),
            Event::DisplayMath(x) => Self::DisplayMath(x),
            Event::Html(x) => Self::Html(x),
            Event::InlineHtml(x) => Self::InlineHtml(x),
            Event::FootnoteReference(x) => Self::FootnoteReference(x),
            Event::SoftBreak => Self::SoftBreak,
            Event::HardBreak => Self::HardBreak,
            Event::Rule => Self::Rule,
            Event::TaskListMarker(x) => Self::TaskListMarker(x),
            Event::Start(_) => {
                panic!("Unexpected start event, only inline events are allowed")
            }
            Event::End(_) => {
                panic!("Unexpected end event, only inline events are allowed")
            }
        }
    }

    pub fn into_static(self) -> NodeKind<'static> {
        match self {
            NodeKind::Document => NodeKind::Document,
            NodeKind::Paragraph => NodeKind::Paragraph,
            NodeKind::Heading {
                level,
                id,
                classes,
                attrs,
            } => NodeKind::Heading {
                level,
                id: id.map(|s| s.into_static()),
                classes: classes.into_iter().map(|s| s.into_static()).collect(),
                attrs: attrs
                    .into_iter()
                    .map(|(k, v)| (k.into_static(), v.map(|s| s.into_static())))
                    .collect(),
            },
            NodeKind::BlockQuote => NodeKind::BlockQuote,
            NodeKind::Callout {
                kind,
                title,
                foldable,
                content_start_byte,
            } => NodeKind::Callout {
                kind,
                title,
                foldable,
                content_start_byte,
            },
            NodeKind::CodeBlock(kb) => NodeKind::CodeBlock(kb.into_static()),
            NodeKind::HtmlBlock => NodeKind::HtmlBlock,
            NodeKind::List(v) => NodeKind::List(v),
            NodeKind::Item => NodeKind::Item,
            NodeKind::FootnoteDefinition(a) => {
                NodeKind::FootnoteDefinition(a.into_static())
            }
            NodeKind::Table(v) => NodeKind::Table(v),
            NodeKind::TableHead => NodeKind::TableHead,
            NodeKind::TableRow => NodeKind::TableRow,
            NodeKind::TableCell => NodeKind::TableCell,
            NodeKind::Emphasis => NodeKind::Emphasis,
            NodeKind::Strong => NodeKind::Strong,
            NodeKind::Strikethrough => NodeKind::Strikethrough,
            NodeKind::Superscript => NodeKind::Superscript,
            NodeKind::Subscript => NodeKind::Subscript,
            NodeKind::Link {
                link_type,
                dest_url,
                title,
                id,
            } => NodeKind::Link {
                link_type,
                dest_url: dest_url.into_static(),
                title: title.into_static(),
                id: id.into_static(),
            },
            NodeKind::Image {
                link_type,
                dest_url,
                title,
                id,
            } => NodeKind::Image {
                link_type,
                dest_url: dest_url.into_static(),
                title: title.into_static(),
                id: id.into_static(),
            },
            NodeKind::MetadataBlock(v) => NodeKind::MetadataBlock(v),
            NodeKind::DefinitionList => NodeKind::DefinitionList,
            NodeKind::DefinitionListTitle => NodeKind::DefinitionListTitle,
            NodeKind::DefinitionListDefinition => {
                NodeKind::DefinitionListDefinition
            }
            NodeKind::Text(s) => NodeKind::Text(s.into_static()),
            NodeKind::Code(s) => NodeKind::Code(s.into_static()),
            NodeKind::InlineMath(s) => NodeKind::InlineMath(s.into_static()),
            NodeKind::DisplayMath(s) => NodeKind::DisplayMath(s.into_static()),
            NodeKind::Html(s) => NodeKind::Html(s.into_static()),
            NodeKind::InlineHtml(s) => NodeKind::InlineHtml(s.into_static()),
            NodeKind::FootnoteReference(s) => {
                NodeKind::FootnoteReference(s.into_static())
            }
            NodeKind::SoftBreak => NodeKind::SoftBreak,
            NodeKind::HardBreak => NodeKind::HardBreak,
            NodeKind::Rule => NodeKind::Rule,
            NodeKind::TaskListMarker(b) => NodeKind::TaskListMarker(b),
        }
    }
}

impl<'a> Display for Node<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl<'a> Node<'a> {
    fn fmt_with_indent(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        // Print indentation
        f.write_str(&" ".repeat(indent))?;

        // Print node kind and range
        writeln!(
            f,
            "{:?} [{}..{}]",
            self.kind, self.start_byte, self.end_byte
        )?;

        for child in self.children.iter() {
            child.fmt_with_indent(f, indent + 2)?;
        }

        Ok(())
    }

    pub fn byte_range(&self) -> Range<usize> {
        self.start_byte..self.end_byte
    }

    pub fn parent(&self) -> Option<&Node<'a>> {
        // Deref pointer in unsafe block
        self.parent.map(|ptr| unsafe { &*ptr })
    }

    pub fn next_sibling(&self) -> Option<&Node<'a>> {
        let parent = self.parent()?;
        let my_index = parent
            .children
            .iter()
            .position(
                // convert reference to raw pointer and compare their addresses
                // equivalent to
                // child as *const Node == self as *const Node
                |child| std::ptr::eq(child, self),
            )
            .expect("A node's parent should contains itself");
        parent.children.get(my_index + 1)
    }

    pub fn prev_sibling(&self) -> Option<&Node<'a>> {
        let parent = self.parent()?;
        let my_index = parent
            .children
            .iter()
            .position(|child| std::ptr::eq(child, self))
            .expect("A node's parent should contains itself");
        parent.children.get(my_index - 1)
    }

    /// Get the node that contains descendant.
    ///
    /// Note that this can return descendant itself.
    ///
    /// Use Cases
    /// This method is useful for:
    /// 1. Finding editing boundaries: "Which top-level section contains
    /// this text I'm editing?"
    /// 2. Navigation: "Which main child should I expand to show this
    /// nested element?"
    /// 3. Structural queries: "Which paragraph contains this specific
    /// word?"
    ///
    /// Why "can return descendant itself"
    /// The note mentions it can return the descendant itself because if
    /// you call:
    /// node.child_with_descendant(&node)  // descendant is the node itself
    pub fn child_with_descendant(
        &self,
        descendant: &Node<'a>,
    ) -> Option<&Node<'a>> {
        if std::ptr::eq(self, descendant) {
            return Some(self);
        }
        if !range_contain(
            &(self.start_byte..self.end_byte),
            &(descendant.start_byte..descendant.end_byte),
        ) {
            return None;
        }
        self.children.iter().find(|child| {
            range_contain(
                &(child.start_byte..child.end_byte),
                &(descendant.start_byte..descendant.end_byte),
            )
        })
    }

    pub fn child(&self, i: usize) -> Option<&Node<'a>> {
        self.children.get(i)
    }

    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    // Get this node's first child that contains or starts after
    // the given byte offset.
    pub fn first_child_for_byte(&self, byte: usize) -> Option<&Node<'_>> {
        use std::cmp::Ordering;

        // Find containment
        match self.children.binary_search_by(|child| {
            if child.start_byte > byte {
                Ordering::Greater
            } else if child.end_byte <= byte {
                Ordering::Less
            } else {
                Ordering::Equal
            }
        }) {
            Ok(containing_child_idx) => {
                Some(&self.children[containing_child_idx])
            }
            Err(insertion_idx) => self.children.get(insertion_idx),
        }
    }

    #[allow(dead_code)]
    fn to_sexp(&self) -> String {
        todo!()
    }

    /// Convert this node with borrowed data to an owned version with 'static lifetime
    pub fn into_static(self) -> Node<'static> {
        Node {
            children: self
                .children
                .into_iter()
                .map(|c| c.into_static())
                .collect(),
            start_byte: self.start_byte,
            end_byte: self.end_byte,
            start_point: self.start_point,
            end_point: self.end_point,
            kind: self.kind.into_static(),
            parent: None, // Parent pointers need to be rebuilt after conversion
        }
    }
}

fn range_contain(range: &Range<usize>, other: &Range<usize>) -> bool {
    range.start <= other.start && range.end >= other.end
}

pub enum InvalidNode {
    InvalidNode,
}

impl validate::Validate for Node<'_> {
    type ValidationError = InvalidNode;

    // TODO: validate that children range is a partition of the parent's range
    fn validate(
        &self,
    ) -> Result<(), nonempty::NonEmpty<Self::ValidationError>> {
        Ok(())
    }
}
