//! Tree structure and parsing logic for the Markdown AST

use crate::parse::default_opts;
use pulldown_cmark::{Event, Options, Parser};
use std::ops::Range;
use tree_sitter::{InputEdit, Point};

use super::node::{Node, NodeKind};

/// A tree that represents the syntactic structure of a source code file.
#[derive(Clone, Debug, PartialEq)]
pub struct Tree<'a> {
    pub root_node: Node<'a>,
    /// the options that were used to parse the syntax tree.
    pub opts: Options,
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

pub(crate) fn setup_parent_pointers<'a>(node: &mut Node<'a>) {
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

    // Handle empty files
    if events_with_offset.is_empty() {
        let root_node = Node {
            kind: NodeKind::Document,
            children: Vec::new(),
            start_byte: 0,
            end_byte: 0,
            start_point: Point { row: 0, column: 0 },
            end_point: Point { row: 0, column: 0 },
            parent: None,
        };
        return Tree { root_node, opts };
    }

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

    Tree { root_node, opts }
}
