//! Table of contents based on heading levels
#[allow(dead_code)] // TODO: remove
use crate::ast::{Node as ASTNode, NodeKind};
use crate::value::Value;
use ego_tree::{Tree, iter::Edge};
use pulldown_cmark::HeadingLevel;
use std::ops::Range;
use tree_sitter::Point;

impl<'a> ASTNode<'a> {
    pub fn is_heading(&self, level: Option<usize>) -> bool {
        match &self.kind {
            NodeKind::Heading { level: l, .. } => {
                if let Some(level) = level {
                    (*l as usize) == level
                } else {
                    true
                }
            }
            _ => false,
        }
    }

    pub fn get_heading_text(&self, text: &str) -> Option<String> {
        if !self.is_heading(None) {
            return None;
        } else if self.children.len() == 0 {
            return None;
        } else {
            let first_child_start = &self.children[0].start_byte;
            let last_child_end =
                &self.children[self.children.len() - 1].end_byte;
            Some(text[*first_child_start..*last_child_end].to_string())
        }
    }
}

pub trait HasLevel {
    fn get_level(&self) -> usize;

    /// Build a tree for a given root node and a list of ordered nodes
    /// based on their level information.
    fn build_tree(root_node: Self, nodes: Vec<Self>) -> Tree<Self>
    where
        Self: Sized,
    {
        let mut tree = Tree::new(root_node);

        // Use a stack of node IDs instead of mutable references
        let mut stack: Vec<ego_tree::NodeId> = vec![tree.root().id()];

        for node in nodes {
            let curr_level = node.get_level();

            // Pop stack until we find a parent with level < current level
            while stack.len() > 1 {
                let parent_id = *stack.last().unwrap();
                let parent_level =
                    tree.get(parent_id).unwrap().value().get_level();

                if parent_level < curr_level {
                    break;
                }
                stack.pop();
            }
            let parent_id = *stack.last().unwrap();

            // Append the node to the current parent
            let new_node_id =
                tree.get_mut(parent_id).unwrap().append(node).id();
            stack.push(new_node_id);
        }
        tree
    }
}

pub struct Section {
    level: usize,
    heading_text: String,
    range: Range<usize>,
    heading_range: Range<usize>,
    content: Value,
}

impl Into<Value> for Section {
    fn into(self) -> Value {
        Value::O(vec![(self.heading_text, self.content)])
    }
}

impl HasLevel for Section {
    fn get_level(&self) -> usize {
        self.level
    }
}

/// Heading information, including text and ending location
#[derive(Debug, PartialEq, Clone)]
pub struct Heading {
    pub level: HeadingLevel,
    pub text: String,
    pub start_byte: usize,
    pub end_byte: usize,
    pub start_point: Point,
    pub end_point: Point,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Root,
    Heading(Heading),
}

impl HasLevel for Node {
    fn get_level(&self) -> usize {
        match self {
            Node::Heading(h) => h.level as usize,
            Node::Root => 0,
        }
    }
}

fn pp_tree(
    tree: &Tree<Node>,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    for edge in tree.root().traverse() {
        match edge {
            Edge::Open(node) => {
                let ancestor_count = node.ancestors().count();
                let depth = ancestor_count.saturating_sub(1);
                let indent = "  ".repeat(depth);
                match node.value() {
                    Node::Root => writeln!(f, "Root")?,
                    Node::Heading(h) => writeln!(
                        f,
                        "{}├─ H{}: {}",
                        indent, h.level as usize, h.text
                    )?,
                }
            }
            Edge::Close(_) => {} // Ignore close edges
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {}
