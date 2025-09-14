/// AST is tree-sitter like
use pulldown_cmark::{
    CodeBlockKind, CowStr, Event, HeadingLevel, MetadataBlockKind, Options,
    Tag, TagEnd,
};
use std::ops::Range;

/// A tree that represents the syntactic structure of a source code file.
struct Tree {
    root_node: Node,
    /// the options that were used to parse the syntax tree.
    opts: Options,
}

impl Tree {
    fn print_dot_graph(&self) {
        todo!()
    }
}

struct Node {
    children: Vec<Node>,
    range: Range<usize>,
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
