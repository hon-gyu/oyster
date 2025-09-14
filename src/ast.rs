/// AST is tree-sitter like
use pulldown_cmark::{
    Alignment, BlockQuoteKind, CodeBlockKind, CowStr, Event, HeadingLevel,
    LinkType, MetadataBlockKind, Options, Tag, TagEnd,
};
use std::ops::Range;

/// A tree that represents the syntactic structure of a source code file.
struct Tree {
    root_node: Node,
    /// the options that were used to parse the syntax tree.
    opts: Options,
}

impl Tree {
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

impl NodeKind {
    fn from_tag(tag: Tag) -> Self {
        todo!()
    }

    fn from_event(event: Event) -> Option<Self> {
        match event {
            Event::Start(_) => None,
            Event::End(_) => None,
            _ => todo!(),
        }
    }
}

struct Node {
    children: Vec<Node>,
    range: Range<usize>,
    kind: NodeKind,
}

impl Node {
    fn child(&self, i: usize) -> Option<&Node> {
        todo!()
    }

    fn child_count(&self) -> usize {
        todo!()
    }
}

fn build_ast(events_with_offset: Vec<Event>) -> Tree {
    todo!()
}
