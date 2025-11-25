//! Generic trait and utilities for building hierarchical tree structures

/// Trait for types that have a hierarchical level
pub trait Hierarchical {
    fn level(&self) -> usize;
}

/// A tree node that can hold hierarchical children
#[derive(Debug)]
pub struct TreeNode<T> {
    pub value: T,
    pub children: Vec<TreeNode<T>>,
}

impl<T> TreeNode<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            children: Vec::new(),
        }
    }
}

/// Build a tree from a flat list of hierarchical items
///
/// This function takes an ordered list of items that implement `Hierarchical`
/// and builds a tree structure based on their levels. Items with higher levels
/// become children of items with lower levels.
///
/// # Example
/// Given headings: H1, H2, H3, H2
/// The tree will be: H1 -> [H2 -> [H3], H2]
pub fn build_tree<T: Hierarchical>(items: Vec<T>) -> Vec<TreeNode<T>> {
    let mut roots = Vec::new();
    let mut stack: Vec<TreeNode<T>> = Vec::new();

    for item in items {
        let curr_level = item.level();
        let node = TreeNode::new(item);

        // Pop nodes from stack that are at the same or deeper level
        while let Some(top) = stack.last() {
            if top.value.level() >= curr_level {
                let popped = stack.pop().unwrap();
                if let Some(parent) = stack.last_mut() {
                    parent.children.push(popped);
                } else {
                    roots.push(popped);
                }
            } else {
                break;
            }
        }

        stack.push(node);
    }

    // Pop remaining nodes from stack
    while let Some(node) = stack.pop() {
        if let Some(parent) = stack.last_mut() {
            parent.children.push(node);
        } else {
            roots.push(node);
        }
    }

    roots
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq)]
    struct TestItem {
        level: usize,
        name: String,
    }

    impl Hierarchical for TestItem {
        fn level(&self) -> usize {
            self.level
        }
    }

    #[test]
    fn test_build_tree_simple() {
        let items = vec![
            TestItem {
                level: 1,
                name: "H1".to_string(),
            },
            TestItem {
                level: 2,
                name: "H2".to_string(),
            },
            TestItem {
                level: 3,
                name: "H3".to_string(),
            },
        ];

        let tree = build_tree(items);
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].value.name, "H1");
        assert_eq!(tree[0].children.len(), 1);
        assert_eq!(tree[0].children[0].value.name, "H2");
        assert_eq!(tree[0].children[0].children.len(), 1);
        assert_eq!(tree[0].children[0].children[0].value.name, "H3");
    }

    #[test]
    fn test_build_tree_with_level_jump() {
        let items = vec![
            TestItem {
                level: 1,
                name: "H1".to_string(),
            },
            TestItem {
                level: 3,
                name: "H3".to_string(),
            },
            TestItem {
                level: 2,
                name: "H2".to_string(),
            },
        ];

        let tree = build_tree(items);
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].children.len(), 2);
        assert_eq!(tree[0].children[0].value.name, "H3");
        assert_eq!(tree[0].children[1].value.name, "H2");
    }

    #[test]
    fn test_build_tree_multiple_roots() {
        let items = vec![
            TestItem {
                level: 1,
                name: "H1a".to_string(),
            },
            TestItem {
                level: 2,
                name: "H2".to_string(),
            },
            TestItem {
                level: 1,
                name: "H1b".to_string(),
            },
        ];

        let tree = build_tree(items);
        assert_eq!(tree.len(), 2);
        assert_eq!(tree[0].value.name, "H1a");
        assert_eq!(tree[1].value.name, "H1b");
        assert_eq!(tree[0].children.len(), 1);
        assert_eq!(tree[0].children[0].value.name, "H2");
    }
}
