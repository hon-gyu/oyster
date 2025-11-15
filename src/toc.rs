//! Table of contents based on heading levels
use crate::ast::{Node, NodeKind, Tree};
use ptree::TreeBuilder;
use pulldown_cmark::HeadingLevel;
use std::collections::{HashMap, HashSet};
use tree_sitter::Point;

#[derive(Debug, PartialEq, Clone)]
pub struct TOCEntry {
    pub level: HeadingLevel,
    pub text: String,
    pub start_byte: usize,
    pub end_byte: usize,
    pub start_point: Point,
    pub end_point: Point,
}

type Depth = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct TOC {
    entries: Vec<(TOCEntry, Depth)>,
}

impl TOCEntry {
    fn to_tree_label(&self) -> String {
        format!("{:?} {} (L{})", self.level, self.text, self.start_point.row,)
    }
}

impl TOC {
    /// Convert the TOC to a `ptree::Tree`, useful for debugging
    ///
    /// Note: this is implmented by LLM
    pub fn to_tree(&self) -> TreeBuilder {
        let mut builder = TreeBuilder::new("TOC".to_string());

        if self.entries.is_empty() {
            return builder;
        }

        // Track depth and use the builder stack
        let mut depth_stack: Vec<Depth> = vec![];

        for (entry, depth) in self.entries.iter() {
            let label = entry.to_tree_label();

            // End children until we're at the right depth
            while let Some(&stack_depth) = depth_stack.last() {
                if stack_depth < *depth {
                    break;
                }
                builder.end_child();
                depth_stack.pop();
            }

            // Begin this child
            builder.begin_child(label);
            depth_stack.push(*depth);
        }

        // Close all remaining children
        while depth_stack.pop().is_some() {
            builder.end_child();
        }

        builder
    }

    /// Render the TOC as a tree string
    pub fn render_tree(&self) -> String {
        let tree = self.to_tree().build();
        let mut output = Vec::new();
        ptree::write_tree(&tree, &mut output).expect("Failed to write tree");
        String::from_utf8(output).expect("Invalid UTF-8")
    }
}

pub fn extract_toc(tree: &Tree) -> TOC {
    let root_node = &tree.root_node;
    let mut entries: Vec<TOCEntry> = vec![];
    extract_toc_from_node(root_node, &mut entries);

    // map heading levels to depths
    // TODO(confirm): Will this consume the data?
    let mut unique_levels: Vec<HeadingLevel> = entries
        .iter()
        .map(|e| e.level)
        .collect::<HashSet<HeadingLevel>>()
        .into_iter()
        .collect();
    unique_levels.sort();

    let level_to_depth: HashMap<HeadingLevel, Depth> = unique_levels
        .into_iter()
        .enumerate()
        .map(|(i, level)| (level, i))
        .collect();

    TOC {
        entries: entries
            .iter()
            .map(|e| (e.clone(), level_to_depth[&e.level]))
            .collect(),
    }
}

fn extract_toc_from_node(node: &Node, toc_entries: &mut Vec<TOCEntry>) {
    for child in node.children.iter() {
        if let NodeKind::Heading { level, .. } = child.kind {
            match &child.children[..] {
                [
                    Node {
                        kind: NodeKind::Text(text),
                        ..
                    },
                ] => {
                    let entry = TOCEntry {
                        level,
                        text: text.to_string(),
                        start_byte: child.start_byte,
                        end_byte: child.end_byte,
                        start_point: child.start_point,
                        end_point: child.end_point,
                    };
                    toc_entries.push(entry);
                }
                _ => panic!("Heading without inner text"),
            }
        } else {
            child
                .children
                .iter()
                .for_each(|c| extract_toc_from_node(c, toc_entries));
        }
    }
    ()
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::{assert_debug_snapshot, assert_snapshot};

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
        let tree = Tree::new(&md);
        // assert_snapshot!(tree.root_node, @"");

        // let mut entries: Vec<TOCEntry> = vec![];
        // extract_toc_from_node(&tree.root_node, &mut entries);
        // assert_debug_snapshot!(entries, @"");

        let toc = extract_toc(&tree);
        assert_debug_snapshot!(toc, @r#"
        TOC {
            entries: [
                (
                    TOCEntry {
                        level: H1,
                        text: "Heading",
                        start_byte: 1,
                        end_byte: 11,
                        start_point: Point {
                            row: 1,
                            column: 0,
                        },
                        end_point: Point {
                            row: 2,
                            column: 0,
                        },
                    },
                    0,
                ),
                (
                    TOCEntry {
                        level: H2,
                        text: "Heading 2",
                        start_byte: 23,
                        end_byte: 36,
                        start_point: Point {
                            row: 5,
                            column: 0,
                        },
                        end_point: Point {
                            row: 6,
                            column: 0,
                        },
                    },
                    1,
                ),
                (
                    TOCEntry {
                        level: H3,
                        text: "Heading 3",
                        start_byte: 48,
                        end_byte: 62,
                        start_point: Point {
                            row: 9,
                            column: 0,
                        },
                        end_point: Point {
                            row: 10,
                            column: 0,
                        },
                    },
                    2,
                ),
                (
                    TOCEntry {
                        level: H4,
                        text: "Heading 4",
                        start_byte: 74,
                        end_byte: 89,
                        start_point: Point {
                            row: 13,
                            column: 0,
                        },
                        end_point: Point {
                            row: 14,
                            column: 0,
                        },
                    },
                    3,
                ),
                (
                    TOCEntry {
                        level: H5,
                        text: "Heading 5",
                        start_byte: 101,
                        end_byte: 117,
                        start_point: Point {
                            row: 17,
                            column: 0,
                        },
                        end_point: Point {
                            row: 18,
                            column: 0,
                        },
                    },
                    4,
                ),
                (
                    TOCEntry {
                        level: H6,
                        text: "Heading 6",
                        start_byte: 129,
                        end_byte: 146,
                        start_point: Point {
                            row: 21,
                            column: 0,
                        },
                        end_point: Point {
                            row: 22,
                            column: 0,
                        },
                    },
                    5,
                ),
                (
                    TOCEntry {
                        level: H1,
                        text: "Heading",
                        start_byte: 158,
                        end_byte: 168,
                        start_point: Point {
                            row: 25,
                            column: 0,
                        },
                        end_point: Point {
                            row: 26,
                            column: 0,
                        },
                    },
                    0,
                ),
            ],
        }
        "#);
    }

    #[test]
    fn test_toc_2() {
        let md = data_sparse_headings();
        let tree = Tree::new(&md);
        // assert_snapshot!(tree.root_node, @"");

        // let mut entries: Vec<TOCEntry> = vec![];
        // extract_toc_from_node(&tree.root_node, &mut entries);
        // assert_debug_snapshot!(entries, @"");

        let toc = extract_toc(&tree);
        assert_debug_snapshot!(toc, @r#"
        TOC {
            entries: [
                (
                    TOCEntry {
                        level: H3,
                        text: "Heading 3",
                        start_byte: 2,
                        end_byte: 16,
                        start_point: Point {
                            row: 2,
                            column: 0,
                        },
                        end_point: Point {
                            row: 3,
                            column: 0,
                        },
                    },
                    1,
                ),
                (
                    TOCEntry {
                        level: H4,
                        text: "Heading 4",
                        start_byte: 18,
                        end_byte: 33,
                        start_point: Point {
                            row: 5,
                            column: 0,
                        },
                        end_point: Point {
                            row: 6,
                            column: 0,
                        },
                    },
                    2,
                ),
                (
                    TOCEntry {
                        level: H5,
                        text: "Heading 5",
                        start_byte: 45,
                        end_byte: 61,
                        start_point: Point {
                            row: 9,
                            column: 0,
                        },
                        end_point: Point {
                            row: 10,
                            column: 0,
                        },
                    },
                    3,
                ),
                (
                    TOCEntry {
                        level: H2,
                        text: "Heading 2",
                        start_byte: 62,
                        end_byte: 75,
                        start_point: Point {
                            row: 11,
                            column: 0,
                        },
                        end_point: Point {
                            row: 12,
                            column: 0,
                        },
                    },
                    0,
                ),
                (
                    TOCEntry {
                        level: H6,
                        text: "Heading 6",
                        start_byte: 76,
                        end_byte: 93,
                        start_point: Point {
                            row: 13,
                            column: 0,
                        },
                        end_point: Point {
                            row: 14,
                            column: 0,
                        },
                    },
                    4,
                ),
            ],
        }
        "#);
    }

    #[test]
    fn test_print_tree() {
        let md = data();
        let tree = Tree::new(&md);
        let toc = extract_toc(&tree);

        let output = toc.render_tree();
        assert_snapshot!(output, @r"
        TOC
        ├─ H1 Heading (L1)
        │  └─ H2 Heading 2 (L5)
        │     └─ H3 Heading 3 (L9)
        │        └─ H4 Heading 4 (L13)
        │           └─ H5 Heading 5 (L17)
        │              └─ H6 Heading 6 (L21)
        └─ H1 Heading (L25)
        ")
    }

    #[test]
    fn test_print_tree_sparse() {
        let md = data_sparse_headings();
        let tree = Tree::new(&md);
        let toc = extract_toc(&tree);

        let output = toc.render_tree();
        assert_snapshot!(output, @r"
        TOC
        ├─ H3 Heading 3 (L2)
        │  └─ H4 Heading 4 (L5)
        │     └─ H5 Heading 5 (L9)
        └─ H2 Heading 2 (L11)
           └─ H6 Heading 6 (L13)
        ")
    }
}
