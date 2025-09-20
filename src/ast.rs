use crate::validate;
use nonempty;
/// AST is tree-sitter like
use pulldown_cmark::{
    Alignment, BlockQuoteKind, CodeBlockKind, CowStr, Event, HeadingLevel,
    LinkType, MetadataBlockKind, Options, Tag,
};
use std::fmt::Display;
use std::ops::Range;
use tree_sitter::{InputEdit, Point, Range as TSRange};

/// A tree that represents the syntactic structure of a source code file.
#[derive(Clone, Debug, PartialEq)]
pub struct Tree<'a> {
    root_node: Node<'a>,
    /// the options that were used to parse the syntax tree.
    opts: Options,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node<'a> {
    children: Vec<Node<'a>>,
    byte_range: Range<usize>,
    kind: NodeKind<'a>,
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
            self.kind, self.byte_range.start, self.byte_range.end
        )?;

        for child in self.children.iter() {
            child.fmt_with_indent(f, indent + 2)?;
        }

        Ok(())
    }
}

/// including Tag + Event that is not start or end
#[derive(Clone, Debug, PartialEq)]
enum NodeKind<'a> {
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

pub enum InvalidNode {
    InvalidNode,
}

impl validate::Validate for Node<'_> {
    type ValidationError = InvalidNode;

    fn validate(
        &self,
    ) -> Result<(), nonempty::NonEmpty<Self::ValidationError>> {
        Ok(())
    }
}

impl Node<'_> {
    // TODO: validate that children range is a partition of the parent's range

    pub fn start_byte(&self) -> usize {
        self.byte_range.start
    }

    pub fn end_byte(&self) -> usize {
        self.byte_range.end
    }

    pub fn byte_range(&self) -> Range<usize> {
        self.byte_range.clone()
    }

    pub fn range(&self) -> TSRange {
        todo!()
    }

    // Get this node's start position in terms of rows and columns
    fn start_position(&self) -> Point {
        todo!()
    }

    // Get this node's end position in terms of rows and columns
    fn end_position(&self) -> Point {
        todo!()
    }

    pub fn child(&self, i: usize) -> Option<&Node<'_>> {
        self.children.get(i)
    }

    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    fn first_child_for_byte(&self, byte: usize) -> Option<&Node<'_>> {
        todo!()
    }

    fn to_sexp(&self) -> String {
        todo!()
    }
}

impl Tree<'_> {
    fn edit(&mut self, edit: &InputEdit) {
        todo!()
    }
}

pub fn build_ast<'a>(
    events_with_offset: Vec<(Event<'a>, Range<usize>)>,
    opts: Options,
) -> Tree<'a> {
    // Stack to keep track of the things we are working on (excluding the root)
    // Each item in the stack is a tuple containing the current node and its previous
    // siblings.
    // When we detect a deeper nesting level, we push a new node and its existing siblings
    let mut stack: Vec<(Node<'a>, Vec<Node<'a>>)> = Vec::new();
    let mut curr_children: Vec<Node<'a>> = Vec::new();

    let doc_start = events_with_offset
        .first()
        .map(|(_, r)| r.start)
        .expect("No events found");
    let doc_end = events_with_offset
        .last()
        .map(|(_, r)| r.end)
        .expect("No events found");

    for (event, offset) in events_with_offset {
        match event {
            Event::Start(tag) => {
                let node = Node {
                    children: Vec::new(),
                    byte_range: offset.clone(),
                    kind: NodeKind::from_tag(tag),
                };
                stack.push((node, curr_children));
                curr_children = Vec::new();
            }
            Event::End(_tag) => {
                // Wrap up the current node
                let (mut completed_node, siblings) =
                    stack.pop().expect("Unbalanced tags");
                completed_node.children = curr_children;

                curr_children = siblings;
                curr_children.push(completed_node);
            }
            inline_event => {
                let leaf_node = Node {
                    children: Vec::new(),
                    byte_range: offset.clone(),
                    kind: NodeKind::from_event(inline_event),
                };
                curr_children.push(leaf_node);
            }
        }
    }

    let root_node = Node {
        children: curr_children,
        byte_range: doc_start..doc_end,
        kind: NodeKind::Document,
    };

    Tree {
        root_node: root_node,
        opts: opts,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;
    use insta::assert_snapshot;
    use pulldown_cmark::Parser;
    use std::fs;

    fn data() -> String {
        let md = fs::read_to_string("src/basic.md").unwrap();
        md
    }

    #[test]
    fn test_build_ast() {
        let md = data();
        let opts = parse::default_opts();
        let parser = Parser::new_ext(&md, opts);
        let events_with_offsets = parser.into_offset_iter().collect::<Vec<_>>();

        let ast = build_ast(events_with_offsets, opts);
        assert_snapshot!(ast.root_node, @r#"
        Document [0..522]
          MetadataBlock(YamlStyle) [0..52]
            Text(Borrowed("title: \"Test Document\"\nauthor: \"Test Author\"\n")) [4..49]
          Heading { level: H1, id: None, classes: [], attrs: [] } [54..66]
            Text(Borrowed("Heading 1")) [56..65]
          Paragraph [67..117]
            Text(Borrowed("Basic text with ")) [67..83]
            Strong [83..91]
              Text(Borrowed("bold")) [85..89]
            Text(Borrowed(" and ")) [91..96]
            Emphasis [96..104]
              Text(Borrowed("italic")) [97..103]
            Text(Borrowed(" formatting.")) [104..116]
          Heading { level: H2, id: None, classes: [], attrs: [] } [118..128]
            Text(Borrowed("A List")) [121..127]
          List(None) [129..187]
            Item [129..164]
              Text(Borrowed("item 1")) [131..137]
              List(None) [140..164]
                Item [140..151]
                  Text(Borrowed("item 1.1")) [142..150]
                Item [153..164]
                  Text(Borrowed("item 1.2")) [155..163]
            Item [164..187]
              Text(Borrowed("item 2")) [166..172]
              List(None) [175..187]
                Item [175..187]
                  Text(Borrowed("item 2.1")) [177..185]
          Heading { level: H2, id: None, classes: [], attrs: [] } [187..206]
            Text(Borrowed("Task Lists")) [190..200]
          List(None) [207..302]
            Item [207..228]
              TaskListMarker(true) [209..212]
              Text(Borrowed("Completed task")) [213..227]
            Item [228..302]
              TaskListMarker(false) [230..233]
              Text(Borrowed("Incomplete task")) [234..249]
              List(None) [252..302]
                Item [252..275]
                  TaskListMarker(true) [254..257]
                  Text(Borrowed("Nested completed")) [258..274]
                Item [277..302]
                  TaskListMarker(false) [279..282]
                  Text(Borrowed("Nested incomplete")) [283..300]
          Heading { level: H2, id: None, classes: [], attrs: [] } [302..319]
            Text(Borrowed("Strikethrough")) [305..318]
          Paragraph [320..349]
            Strikethrough [320..348]
              Text(Borrowed("This text is crossed out")) [322..346]
          Heading { level: H2, id: None, classes: [], attrs: [] } [350..364]
            Text(Borrowed("Code Block")) [353..363]
          CodeBlock(Fenced(Borrowed("python"))) [365..402]
            Text(Borrowed("def foo():\n    return 1\n")) [375..399]
          Heading { level: H2, id: None, classes: [], attrs: [] } [404..412]
            Text(Borrowed("Math")) [407..411]
          Paragraph [413..437]
            Text(Borrowed("Inline math: ")) [413..426]
            InlineMath(Borrowed("E = mc^2")) [426..436]
          Paragraph [438..505]
            Text(Borrowed("Block math:")) [438..449]
            SoftBreak [449..450]
            DisplayMath(Boxed("\n\\int_{-\\infty}^{\\infty} e^{-x^2} dx = \\sqrt{\\pi}\n")) [450..504]
          Heading { level: H3, id: None, classes: [], attrs: [] } [506..522]
            Text(Borrowed("Heading 3.1")) [510..521]
        "#);
    }
}
