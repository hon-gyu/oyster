//! Table of contents based on heading levels
use crate::ast::{Node as ASTNode, NodeKind, Tree as ASTTree};
use ego_tree::{Tree, iter::Edge};
use pulldown_cmark::HeadingLevel;
use std::fmt::Display;
use tree_sitter::Point;

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

trait HasLevel {
    fn get_level(&self) -> usize;

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

impl HasLevel for Node {
    fn get_level(&self) -> usize {
        match self {
            Node::Heading(h) => h.level as usize,
            Node::Root => 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Hierarchy(Tree<Node>);

impl Display for Hierarchy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tree = &self.0;
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
}

pub fn build_heading_hierarchy(tree: &ASTTree) -> Hierarchy {
    let root_node = &tree.root_node;
    let mut nodes: Vec<Node> = vec![];
    extract_heading_nodes_from_ast_node(root_node, &mut nodes);

    let heading_tree = HasLevel::build_tree(Node::Root, nodes);

    Hierarchy(heading_tree)
}

fn extract_heading_nodes_from_ast_node(
    node: &ASTNode,
    acc_heading_nodes: &mut Vec<Node>,
) {
    for child in node.children.iter() {
        if let NodeKind::Heading { level, .. } = child.kind {
            match &child.children[..] {
                [
                    ASTNode {
                        kind: NodeKind::Text(text),
                        ..
                    },
                ] => {
                    let entry = Heading {
                        level,
                        text: text.to_string(),
                        start_byte: child.start_byte,
                        end_byte: child.end_byte,
                        start_point: child.start_point,
                        end_point: child.end_point,
                    };
                    acc_heading_nodes.push(Node::Heading(entry));
                }
                [] => {
                    // Empty heading
                    let entry = Heading {
                        level,
                        text: "".to_string(),
                        start_byte: child.start_byte,
                        end_byte: child.end_byte,
                        start_point: child.start_point,
                        end_point: child.end_point,
                    };
                    acc_heading_nodes.push(Node::Heading(entry));
                }
                _ => unreachable!("Never: Heading without inner text"),
            }
        } else {
            child.children.iter().for_each(|c| {
                extract_heading_nodes_from_ast_node(c, acc_heading_nodes)
            });
        }
    }
    ()
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    fn data() -> String {
        let md = r#######"
# Heading

some text

## Heading 2

some text

### Heading 3

some text

#### Heading 4

some text

##### Heading 5

some text

###### Heading 6

some text

# Heading

some text

#

emtpy heading
"#######;
        md.to_string()
    }

    fn data_sparse_headings() -> String {
        r#######"

### Heading 3


#### Heading 4

some text

##### Heading 5

## Heading 2

###### Heading 6

some text
"#######
            .to_string()
    }

    #[test]
    fn test_toc_1() {
        let md = data();
        let tree = ASTTree::new(&md);
        let toc = build_heading_hierarchy(&tree);
        assert_snapshot!(toc, @r"
        Root
        ├─ H1: Heading
          ├─ H2: Heading 2
            ├─ H3: Heading 3
              ├─ H4: Heading 4
                ├─ H5: Heading 5
                  ├─ H6: Heading 6
        ├─ H1: Heading
        ├─ H1:
        ");
    }

    #[test]
    fn test_toc_2() {
        let md = data_sparse_headings();
        let tree = ASTTree::new(&md);

        let toc = build_heading_hierarchy(&tree);
        assert_snapshot!(toc, @r"
        Root
        ├─ H3: Heading 3
          ├─ H4: Heading 4
            ├─ H5: Heading 5
        ├─ H2: Heading 2
          ├─ H6: Heading 6
        ");
    }
}
