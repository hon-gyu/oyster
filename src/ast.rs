use crate::validate;
use nonempty;
/// AST is tree-sitter like
use pulldown_cmark::{
    Alignment, BlockQuoteKind, CodeBlockKind, CowStr, Event, HeadingLevel,
    LinkType, MetadataBlockKind, Options, Tag, TagEnd,
};
use std::ops::Range;

/// A tree that represents the syntactic structure of a source code file.
struct Tree<'a> {
    root_node: Node<'a>,
    /// the options that were used to parse the syntax tree.
    opts: Options,
}

impl Tree<'_> {
    /// Copied from tree-sitter's Tree struct
    /// We may want something in other format
    fn print_dot_graph(&self) {
        todo!()
    }

    fn into_string(&self) -> String {
        todo!()
    }
}

/// including Tag + Event that is not start or end
enum NodeKind<'a> {
    /// Tag
    Paragraph,
    Heading {
        level: HeadingLevel,
        id: Option<CowStr<'a>>,
        classes: Vec<CowStr<'a>>,
        attrs: Vec<(CowStr<'a>, Option<CowStr<'a>>)>,
    },
    BlockQuote(Option<BlockQuoteKind>),
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
    fn from_tag(tag: Tag<'a>) -> Self {
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
            Tag::BlockQuote(x) => Self::BlockQuote(x),
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

    fn from_event(event: Event<'a>) -> Self {
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

    fn into_static(self) -> NodeKind<'static> {
        match self {
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
            NodeKind::BlockQuote(k) => NodeKind::BlockQuote(k),
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

struct Node<'a> {
    children: Vec<Node<'a>>,
    range: Range<usize>,
    kind: NodeKind<'a>,
}

enum InvalidNode {
    InvalidNode,
}

impl validate::Validate for Node<'_> {
    type ValidationError = InvalidNode;

    fn validate(
        &self,
    ) -> Result<(), nonempty::NonEmpty<Self::ValidationError>> {
        todo!()
    }
}

impl Node<'_> {
    fn child(&self, i: usize) -> Option<&Node<'_>> {
        todo!()
    }

    fn child_count(&self) -> usize {
        todo!()
    }
}

fn build_ast(events_with_offset: Vec<Event>) -> Tree {
    todo!()
}
