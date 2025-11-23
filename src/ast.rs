//! Define the AST for the Markdown
use crate::parse::default_opts;
use crate::validate;
use nonempty;
use pulldown_cmark::{
    Alignment, BlockQuoteKind, CodeBlockKind, CowStr, Event, HeadingLevel,
    LinkType, MetadataBlockKind, Options, Parser, Tag,
};
use std::fmt::Display;
use std::ops::Range;
use tree_sitter::{InputEdit, Point};

/// A tree that represents the syntactic structure of a source code file.
#[derive(Clone, Debug, PartialEq)]
pub struct Tree<'a> {
    pub root_node: Node<'a>,
    /// the options that were used to parse the syntax tree.
    pub opts: Options,
}

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
    parent: Option<*const Node<'a>>,
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
}

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

    // TODO: validate that children range is a partition of the parent's range
    fn validate(
        &self,
    ) -> Result<(), nonempty::NonEmpty<Self::ValidationError>> {
        Ok(())
    }
}

/// Index for efficiently converting byte offsets to line/column positions
struct LineIndex {
    /// Byte offset where each line starts
    line_starts: Vec<usize>,
}

impl LineIndex {
    /// Build a line index from the source text
    fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        for (i, c) in text.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    /// Convert a byte offset to a Point (row, column)
    fn byte_to_point(&self, text: &str, byte: usize) -> Point {
        // Binary search to find which line contains this byte
        let row = match self.line_starts.binary_search(&byte) {
            Ok(line) => line,
            Err(line) => line.saturating_sub(1),
        };

        let line_start = self.line_starts[row];

        // Count characters from line start to byte position
        let column = text[line_start..byte.min(text.len())].chars().count();

        Point { row, column }
    }
}

impl<'a> Node<'a> {
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

    // // Get this node's start position in terms of rows and columns
    // pub fn start_position(&self, text: &str) -> Point {
    //     byte_to_point(text, self.range.start)
    // }

    // // Get this node's end position in terms of rows and columns
    // pub fn end_position(&self, text: &str) -> Point {
    //     byte_to_point(text, self.range.end)
    // }

    pub fn child(&self, i: usize) -> Option<&Node<'a>> {
        self.children.get(i)
    }

    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    // Get this node’s first child that contains or starts after
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
}

fn range_contain(range: &Range<usize>, other: &Range<usize>) -> bool {
    range.start <= other.start && range.end >= other.end
}

impl<'a> Tree<'a> {
    pub fn new(text: &'a str) -> Self {
        let opts = default_opts();
        let parser = Parser::new_ext(text, opts);
        let events_with_offsets = parser.into_offset_iter().collect::<Vec<_>>();
        let mut tree = build_ast(text, events_with_offsets, opts);
        setup_parent_pointers(&mut tree.root_node);
        tree
    }

    #[allow(warnings)]
    pub fn edit(&mut self, edit: &InputEdit) {
        todo!()
    }

    /// Compare this old edited syntax tree to a new syntax tree representing
    /// the same document, returning a sequence of ranges whose syntactic
    /// structure has changed.
    #[allow(warnings)]
    pub fn changed_ranges(&self, other: &Self) {
        // ) -> impl ExactSizeIterator<Item = TSRange> {
        todo!()
    }
}

fn setup_parent_pointers<'a>(node: &mut Node<'a>) {
    let parent_ptr = node as *const Node;
    for child in node.children.iter_mut() {
        child.parent = Some(parent_ptr);
        setup_parent_pointers(child);
    }
}

/// Builds an AST from the given text and events, with the parent pointers empty.
///
/// While iterating over the events, we keep track of
/// 1. the current parent node (initiated as the root node)
/// 2. the children of the current parent node
/// 3. the previous siblings of the current parent node
///
/// When we encounter a new tag (`Event::Start`), we go one level deeper
/// - We create a new node and set it the current working parent
/// - We create a new empty children vector
///
/// When we encounter an end tag (`Event::End`), we go one level up
/// - The current working parent has collected all its children
/// - It previous siblings and itself will be the children ready to be appended
fn build_ast<'a>(
    text: &str,
    events_with_offset: Vec<(Event<'a>, Range<usize>)>,
    opts: Options,
) -> Tree<'a> {
    // Build line index for efficient byte-to-point conversion
    let line_index = LineIndex::new(text);

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
        // Dispatch event to the handling of either tag or inline
        match event {
            // for `Event::Start`, pulldown-cmark provides the offset of the
            // span of the entire element, including the start and end tags.
            //
            // Invariant: the start and end offsets of the element are
            // the same for the same element.
            Event::Start(tag) => {
                let node = Node {
                    children: Vec::new(),
                    start_byte: offset.start,
                    end_byte: offset.end,
                    start_point: line_index.byte_to_point(text, offset.start),
                    end_point: line_index.byte_to_point(text, offset.end),
                    kind: NodeKind::from_tag(tag),
                    parent: None,
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
                    start_byte: offset.start,
                    end_byte: offset.end,
                    start_point: line_index.byte_to_point(text, offset.start),
                    end_point: line_index.byte_to_point(text, offset.end),
                    kind: NodeKind::from_event(inline_event),
                    parent: None,
                };
                curr_children.push(leaf_node);
            }
        }
    }

    let root_node = Node {
        children: curr_children,
        start_byte: doc_start,
        end_byte: doc_end,
        start_point: line_index.byte_to_point(text, doc_start),
        end_point: line_index.byte_to_point(text, doc_end),
        kind: NodeKind::Document,
        parent: None,
    };

    Tree {
        root_node: root_node,
        opts: opts,
    }
}

// ====================

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;
    use std::fs;

    fn data() -> String {
        let md = fs::read_to_string("tests/data/notes/basic.md").unwrap();
        md
    }

    #[test]
    fn test_positions_match_editor() {
        // Line 0: "First line"
        // Line 1: "Second line"
        // Line 2: "Third line"
        let text = r#"First line
Second line
Third line"#;
        let tree = Tree::new(text);

        // Root should span from (0,0) to (2,10)
        assert_eq!(tree.root_node.start_point.row, 0);
        assert_eq!(tree.root_node.start_point.column, 0);
        assert_eq!(tree.root_node.end_point.row, 2);
        assert_eq!(tree.root_node.end_point.column, 10);

        // Test with escaped newline - it should still count as a new line
        let text = r#"line 1\
line 2"#;
        let tree = Tree::new(text);

        // The \n creates a new line in the source, so positions should reflect that
        // Line 0: "line 1\"
        // Line 1: "line 2"
        assert_eq!(tree.root_node.start_point.row, 0);
        assert_eq!(tree.root_node.end_point.row, 1);
    }

    #[test]
    fn test_build_ast() {
        let md = data();
        let ast = Tree::new(&md);
        assert_snapshot!(ast.root_node, @r#"
        Document [0..535]
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
          Paragraph [523..535]
            Text(Borrowed("emoji: 😀")) [523..534]
        "#);
    }
}
