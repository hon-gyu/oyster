//! Generic trait and utilities for building hierarchical tree structures
use crate::export::utils::get_relative_dest;
use crate::link::Referenceable;
use maud::{Markup, html};
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

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

// ====================
// File tree
// ====================

/// File tree item for home page
#[derive(Debug)]
pub struct FileTreeItem {
    pub name: String,
    pub path: PathBuf,
    pub slug: Option<String>, // None for directories, Some for files
    pub depth: usize,
}

impl FileTreeItem {
    pub fn is_directory(&self) -> bool {
        self.slug.is_none()
    }
}

impl Hierarchical for FileTreeItem {
    fn level(&self) -> usize {
        self.depth
    }
}

/// Build a file tree (note paths only)
pub fn build_file_tree<F>(
    vault_slug: &Path,
    referenceables: &[Referenceable],
    vault_path_to_slug: &F,
) -> Vec<TreeNode<FileTreeItem>>
where
    F: Fn(&PathBuf) -> Option<String>,
{
    // Extract note paths
    let note_paths: Vec<&PathBuf> = referenceables
        .iter()
        .filter_map(|r| match r {
            Referenceable::Note { path, .. } => Some(path),
            _ => None,
        })
        .collect();

    // Build a set of all directories and files with their depths
    let mut items = Vec::new();
    let mut all_dirs: BTreeSet<(PathBuf, usize)> = BTreeSet::new();

    for note_path in note_paths {
        // Add all parent directories
        let mut current = note_path.as_path();
        let mut depth = note_path.components().count() - 1;

        while let Some(parent) = current.parent() {
            if parent != Path::new("") {
                all_dirs.insert((parent.to_path_buf(), depth - 1));
            }
            current = parent;
            depth = depth.saturating_sub(1);
        }

        // Add the file itself with relative link
        let file_depth = note_path.components().count() - 1;
        let name = note_path.file_stem().unwrap().to_string_lossy().to_string();
        let absolute_slug = vault_path_to_slug(note_path).unwrap();
        // Convert to relative path from current page
        let relative_slug =
            get_relative_dest(vault_slug, Path::new(absolute_slug));
        items.push(FileTreeItem {
            name,
            path: note_path.clone(),
            slug: Some(relative_slug),
            depth: file_depth,
        });
    }

    // Add directories
    for (dir_path, depth) in all_dirs {
        let name = dir_path
            .file_name()
            .unwrap_or(dir_path.as_os_str())
            .to_string_lossy()
            .to_string();
        items.push(FileTreeItem {
            name,
            path: dir_path,
            slug: None,
            depth,
        });
    }

    // Sort items by path for consistent tree building
    items.sort_by(|a, b| a.path.cmp(&b.path));

    // Build tree using Hierarchical trait
    let tree = build_tree(items);

    tree
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
