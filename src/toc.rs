//! Table of contents based on heading levels
use crate::ast::{Node, NodeKind, Tree};
use ptree::TreeBuilder;
use pulldown_cmark::HeadingLevel;
use std::collections::{HashMap, HashSet};
use std::ops::Range;

#[derive(Debug, PartialEq, Clone)]
pub struct TOCEntry {
    pub level: HeadingLevel,
    pub text: String,
    pub range: Range<usize>,
}

type Depth = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct TOC {
    entries: Vec<(TOCEntry, Depth)>,
}

impl TOC {
    /// Convert the TOC to a `ptree::Tree`
    /// #implmented_by_AI
    pub fn to_tree(&self) -> TreeBuilder {
        let mut builder = TreeBuilder::new("TOC".to_string());

        if self.entries.is_empty() {
            return builder;
        }

        // Track depth and use the builder stack
        let mut depth_stack: Vec<Depth> = vec![];

        for (entry, depth) in self.entries.iter() {
            let label = format!("{:?} {}", entry.level, entry.text);

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

    dbg!(&unique_levels);
    let level_to_depth: HashMap<HeadingLevel, Depth> = unique_levels
        .into_iter()
        .enumerate()
        .map(|(i, level)| (level, i))
        .collect();

    dbg!(&level_to_depth);

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
                    let range = child.byte_range();
                    let entry = TOCEntry {
                        level,
                        text: text.to_string(),
                        range,
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
                        range: 1..11,
                    },
                    0,
                ),
                (
                    TOCEntry {
                        level: H2,
                        text: "Heading 2",
                        range: 23..36,
                    },
                    1,
                ),
                (
                    TOCEntry {
                        level: H3,
                        text: "Heading 3",
                        range: 48..62,
                    },
                    2,
                ),
                (
                    TOCEntry {
                        level: H4,
                        text: "Heading 4",
                        range: 74..89,
                    },
                    3,
                ),
                (
                    TOCEntry {
                        level: H5,
                        text: "Heading 5",
                        range: 101..117,
                    },
                    4,
                ),
                (
                    TOCEntry {
                        level: H6,
                        text: "Heading 6",
                        range: 129..146,
                    },
                    5,
                ),
                (
                    TOCEntry {
                        level: H1,
                        text: "Heading",
                        range: 158..168,
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
                        range: 2..16,
                    },
                    1,
                ),
                (
                    TOCEntry {
                        level: H4,
                        text: "Heading 4",
                        range: 18..33,
                    },
                    2,
                ),
                (
                    TOCEntry {
                        level: H5,
                        text: "Heading 5",
                        range: 45..61,
                    },
                    3,
                ),
                (
                    TOCEntry {
                        level: H2,
                        text: "Heading 2",
                        range: 62..75,
                    },
                    0,
                ),
                (
                    TOCEntry {
                        level: H6,
                        text: "Heading 6",
                        range: 76..93,
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
        ├─ H1 Heading
        │  └─ H2 Heading 2
        │     └─ H3 Heading 3
        │        └─ H4 Heading 4
        │           └─ H5 Heading 5
        │              └─ H6 Heading 6
        └─ H1 Heading
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
        ├─ H3 Heading 3
        │  └─ H4 Heading 4
        │     └─ H5 Heading 5
        └─ H2 Heading 2
           └─ H6 Heading 6
        ")
    }
}
