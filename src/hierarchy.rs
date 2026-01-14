//! Generic utilities for building hierarchical tree structures from flat lists.
//!
//! # Overview
//!
//! This module provides two tree-building strategies for hierarchical items:
//!
//! ## `build_compact_tree`
//!
//! - **Relative ordering**: Only the relative level order matters, not absolute values
//! - **No gap-filling**: H1 → H3 makes H3 a direct child of H1 (no implicit H2)
//! - **Multiple roots**: Can produce a forest if multiple top-level items exist
//! - **Use case**: Table of contents where you want direct parent-child relationships
//!
//! ## `build_padded_tree`
//!
//! - **Absolute levels**: Respects actual level values (1-6 for headings)
//! - **Gap-filling**: Creates implicit nodes for level gaps (H1 → H3 inserts implicit H2)
//! - **Single root**: Always produces exactly one root at the specified minimum level
//! - **Indexed**: Assigns hierarchical indices (e.g., "0.1.2") to each node
//! - **Use case**: Document sections where structural gaps should be preserved
//!
//! # Example
//!
//! ```ignore
//! // Items: H1, H3, H2 (note the gap from H1 to H3)
//! let items = vec![Item::new(1), Item::new(3), Item::new(2)];
//!
//! // Compact: H3 becomes direct child of H1
//! // H1 ─┬─ H3
//! //     └─ H2
//!
//! // Padded: Implicit H2 inserted
//! // H1 ─┬─ H2 (implicit) ── H3
//! //     └─ H2
//! ```

use crate::export::utils::get_relative_dest;
use crate::link::Referenceable;
use std::collections::BTreeSet;
use std::fmt;
use std::path::{Path, PathBuf};

/// Trait for types that have a hierarchical level.
///
/// Implementors must provide a `level()` method returning the item's level
/// in the hierarchy (e.g., 1-6 for Markdown headings).
pub trait Hierarchical {
    /// Returns the hierarchical level of this item.
    fn level(&self) -> usize;
}

/// Trait for hierarchical types that can create placeholder items.
///
/// Used by `build_padded_tree` to fill gaps in the hierarchy.
/// For example, if H1 is followed directly by H3, an implicit H2 is created
/// using `default_at_level(2, ...)`.
pub trait HierarchicalWithDefaults: Hierarchical {
    /// Create a default/implicit item at the specified level.
    ///
    /// # Arguments
    ///
    /// - `level`: The hierarchical level for the new item
    /// - `index`: Optional hierarchical index (e.g., `[0, 1, 2]` for "0.1.2")
    fn default_at_level(level: usize, index: Option<Vec<usize>>) -> Self;
}

/// A node in a hierarchical tree structure.
///
/// # Fields
///
/// - `value`: The wrapped item of type `T`
/// - `children`: Child nodes at deeper levels
/// - `index`: Hierarchical position as a path (e.g., `[0, 1, 2]` = "0.1.2")
///
/// # Index Format
///
/// The index is a vector representing the path from root:
/// - `[0]` = root node
/// - `[0, 1]` = second child of root (1-indexed for real items)
/// - `[0, 1, 0]` = implicit child (0 indicates gap-filled node)
#[derive(Debug)]
pub struct HierarchyItem<T> {
    /// The wrapped value
    pub value: T,
    /// Child nodes (items at deeper hierarchical levels)
    pub children: Vec<HierarchyItem<T>>,
    /// Hierarchical index path (e.g., `[0, 1, 2]` represents "0.1.2")
    pub index: Option<Vec<usize>>,
}

impl<T> HierarchyItem<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            children: Vec::new(),
            index: None,
        }
    }

    /// Chained indexing - navigate through the tree using a path of child indices
    /// Example: query_by_index(&[0, 1]) returns the second child (index 1) of the first child (index 0)
    pub fn query_by_index(
        &self,
        indices: &[usize],
    ) -> Option<&HierarchyItem<T>> {
        if indices.is_empty() {
            return Some(self);
        }

        let first = indices[0];
        if first >= self.children.len() {
            return None;
        }

        self.children[first].query_by_index(&indices[1..])
    }

    /// Helper function to format the tree with a prefix
    fn fmt_with_prefix(
        &self,
        f: &mut fmt::Formatter<'_>,
        prefix: &str,
    ) -> fmt::Result
    where
        T: fmt::Display,
    {
        // Print current node with optional index
        if let Some(ref idx) = self.index {
            let idx_str = idx
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(".");
            writeln!(f, "{} ({})", self.value, idx_str)?;
        } else {
            writeln!(f, "{}", self.value)?;
        }

        // Print children
        let child_count = self.children.len();
        for (i, child) in self.children.iter().enumerate() {
            let is_last_child = i == child_count - 1;

            // Print the branch character
            if is_last_child {
                write!(f, "{}└── ", prefix)?;
            } else {
                write!(f, "{}├── ", prefix)?;
            }

            // Determine the prefix for the child's children
            let child_prefix = if is_last_child {
                format!("{}    ", prefix)
            } else {
                format!("{}│   ", prefix)
            };

            child.fmt_with_prefix(f, &child_prefix)?;
        }

        Ok(())
    }
}

impl<T: fmt::Display> fmt::Display for HierarchyItem<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_prefix(f, "")
    }
}

/// Build a tree from a flat list of hierarchical items
///
/// This function takes an ordered list of items that implement `Hierarchical`
/// and builds a tree structure based on their levels. Items with higher levels
/// become children of items with lower levels.
///
/// Explanation of "compact"
/// - the absolute value of level doesn't matter, only the relative order.
///
/// # Example
/// Given headings: H1, H2, H3, H2
/// The tree will be: H1 -> [H2 -> [H3], H2]
pub fn build_relative_tree<T: Hierarchical>(
    items: Vec<T>,
) -> Vec<HierarchyItem<T>> {
    let mut roots = Vec::new();
    let mut stack: Vec<HierarchyItem<T>> = Vec::new();

    for item in items {
        let curr_level = item.level();
        let node = HierarchyItem::new(item);

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

/// Build a single-rooted tree from a flat list of hierarchical items.
///
/// This function creates a tree with gap-filling: if items skip levels
/// (e.g., H1 → H3), implicit nodes are inserted at the skipped levels.
///
/// # Arguments
///
/// - `items`: Flat list of hierarchical items in document order
/// - `min_level`: Optional root level for the tree
///   - `None`: Root at level 1 (default)
///   - `Some(0)`: Root at level 0 (useful for document root before headings)
///   - `Some(n)`: Root at level n
///
/// # Returns
///
/// A single [`HierarchyItem<T>`] representing the root of the tree.
///
/// # Index Assignment
///
/// Each node receives a hierarchical index:
/// - **Real items**: Indices start at 1 (e.g., first H1 = "0.1", second H1 = "0.2")
/// - **Implicit items**: Index is 0 (e.g., gap-filled H2 = "0.1.0")
///
/// # Examples
///
/// ```text
/// Input: A(1), B(3), C(2)
///
/// Output tree:
///   [0] Root
///   └── [0.1] A (level 1)
///       ├── [0.1.0] implicit (level 2)
///       │   └── [0.1.0.1] B (level 3)
///       └── [0.1.1] C (level 2)
/// ```
///
/// ```text
/// Input: A(2), B(4), C(2) with min_level=Some(0)
///
/// Output tree:
///   [0] Root (level 0, implicit)
///   └── [0.0] implicit (level 1)
///       └── [0.0.1] A (level 2)
///           └── [0.0.1.0] implicit (level 3)
///               └── [0.0.1.0.1] B (level 4)
/// ```
///
/// # Errors
///
/// Returns an error if:
/// - Input is empty
/// - Any item has a level less than the specified `min_level`
pub fn build_padded_tree<T: HierarchicalWithDefaults>(
    items: Vec<T>,
    min_level: Option<usize>,
) -> Result<HierarchyItem<T>, String> {
    if items.is_empty() {
        return Err("Cannot build tree from empty input".to_string());
    }

    // Find min level
    let existing_min_level =
        items.iter().map(|item| item.level()).min().unwrap();
    // Root level: where the tree should start
    // - If min_level is specified, use it (allows level 0 for document root)
    // - If not specified, default to 1 (ensures single root)
    let root_level = min_level.unwrap_or(1);

    if existing_min_level < root_level {
        return Err(format!(
            "Cannot build tree with root level {} as items start at level {}",
            root_level, existing_min_level
        ));
    }

    let mut roots = Vec::new();
    let mut stack: Vec<(usize, HierarchyItem<T>, bool)> = Vec::new(); // (level, node, is_default)

    // Track the number of real children at each level in the current path
    let mut real_child_counts: Vec<usize> = vec![0; 10]; // Assuming max 10 levels

    // Insert defaults from root_level to existing_min_level - 1
    for level in root_level..existing_min_level {
        // Index length is depth + 1, where depth = level - root_level
        let index: Vec<usize> = vec![0; level - root_level + 1];
        let mut default_node =
            HierarchyItem::new(T::default_at_level(level, Some(index.clone())));
        default_node.index = Some(index);
        stack.push((level, default_node, true));
    }

    for item in items {
        let curr_level = item.level();

        // Pop items at same or deeper level
        while let Some((top_level, _, _)) = stack.last() {
            if *top_level >= curr_level {
                let (level, popped, _) = stack.pop().unwrap();
                if let Some((_, parent, _)) = stack.last_mut() {
                    parent.children.push(popped);
                } else {
                    roots.push(popped);
                }
                // Reset count only for levels strictly deeper than current
                if level > curr_level && stack.len() < real_child_counts.len() {
                    real_child_counts[stack.len()] = 0;
                }
            } else {
                break;
            }
        }

        // Insert default items for level gaps between stack top and current
        if let Some((top_level, _, _)) = stack.last() {
            for level in (top_level + 1)..curr_level {
                // Calculate index based on current stack path + 0
                let mut index = Vec::new();
                for (i, (_, _, is_default)) in stack.iter().enumerate() {
                    if *is_default {
                        index.push(0);
                    } else {
                        index.push(real_child_counts[i]);
                    }
                }
                index.push(0);
                let mut default_node = HierarchyItem::new(T::default_at_level(
                    level,
                    Some(index.clone()),
                ));
                default_node.index = Some(index);
                stack.push((level, default_node, true));
            }
        } else if curr_level > root_level {
            // Stack is empty but current item is not at root_level
            for level in root_level..curr_level {
                // Index length is depth + 1, where depth = level - root_level
                let index: Vec<usize> = vec![0; level - root_level + 1];
                let mut default_node = HierarchyItem::new(T::default_at_level(
                    level,
                    Some(index.clone()),
                ));
                default_node.index = Some(index);
                stack.push((level, default_node, true));
            }
        }

        // Calculate section number for this real item
        let mut section = Vec::new();
        for (i, (_, _, is_default)) in stack.iter().enumerate() {
            if *is_default {
                section.push(0);
            } else {
                // This is a real item in the path
                section.push(real_child_counts[i]);
            }
        }
        // Add the current item's index
        real_child_counts[stack.len()] += 1;
        section.push(real_child_counts[stack.len()]);

        let mut node = HierarchyItem::new(item);
        node.index = Some(section);
        stack.push((curr_level, node, false));
    }

    // Pop remaining items from stack
    while let Some((_, node, _)) = stack.pop() {
        if let Some((_, parent, _)) = stack.last_mut() {
            parent.children.push(node);
        } else {
            roots.push(node);
        }
    }

    // By construction, there's always exactly one root
    Ok(roots.into_iter().next().unwrap())
}

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

impl HierarchicalWithDefaults for FileTreeItem {
    fn default_at_level(level: usize, _index: Option<Vec<usize>>) -> Self {
        Self {
            name: String::new(),
            path: PathBuf::new(),
            slug: None,
            depth: level,
        }
    }
}

/// Build a file tree (note paths only)
pub fn build_file_tree<F>(
    vault_slug: &Path,
    referenceables: &[Referenceable],
    vault_path_to_slug: &F,
) -> Vec<HierarchyItem<FileTreeItem>>
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
            get_relative_dest(vault_slug, Path::new(&absolute_slug));
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
    build_relative_tree(items)
}

// Test
// ====================

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

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

    impl HierarchicalWithDefaults for TestItem {
        fn default_at_level(level: usize, _index: Option<Vec<usize>>) -> Self {
            Self {
                level,
                name: "<implicit>".to_string(),
            }
        }
    }

    impl std::fmt::Display for TestItem {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let level_prefix: String =
                vec!['#'.to_string(); self.level].join("");
            write!(f, "{} {}", level_prefix, self.name)
        }
    }

    /// Helper function to create test items from (level, name) tuples
    fn items(specs: &[(usize, &str)]) -> Vec<TestItem> {
        specs
            .iter()
            .map(|(level, name)| TestItem {
                level: *level,
                name: name.to_string(),
            })
            .collect()
    }

    #[test]
    fn test_build_compact_tree_simple() {
        let tree =
            build_relative_tree(items(&[(1, "H1"), (2, "H2"), (3, "H3")]));
        assert_eq!(tree.len(), 1);
        let output = format!("{}", tree[0]);
        assert_snapshot!(output, @r"
        # H1
        └── ## H2
            └── ### H3
        ");
    }

    #[test]
    fn test_build_compact_tree_with_level_jump() {
        let tree =
            build_relative_tree(items(&[(1, "H1"), (3, "H3"), (2, "H2")]));
        assert_eq!(tree.len(), 1);
        let output = format!("{}", tree[0]);
        assert_snapshot!(output, @r"
        # H1
        ├── ### H3
        └── ## H2
        ");
    }

    #[test]
    fn test_build_compact_tree_multiple_roots() {
        let tree =
            build_relative_tree(items(&[(1, "H1a"), (2, "H2"), (1, "H1b")]));
        assert_eq!(tree.len(), 2);
        let output = format!("{}\n\n{}", tree[0], tree[1]);
        assert_snapshot!(output, @r"
        # H1a
        └── ## H2


        # H1b
        ");
    }

    #[test]
    fn test_display_hierarchy() {
        let tree = build_relative_tree(items(&[
            (1, "H1"),
            (2, "H2"),
            (3, "H3"),
            (2, "H2b"),
        ]));
        let output = format!("{}", tree[0]);
        assert_snapshot!(output, @r"
        # H1
        ├── ## H2
        │   └── ### H3
        └── ## H2b
        ");
    }

    // Build loose tree
    // --------------------

    #[test]
    fn test_build_loose_tree_example1() {
        // Example 1: A(1), B(3), C(2)
        let root =
            build_padded_tree(items(&[(1, "A"), (3, "B"), (2, "C")]), None)
                .unwrap();
        let output = format!("{}", root);
        assert_snapshot!(output, @r"
        # A (1)
        ├── ## <implicit> (1.0)
        │   └── ### B (1.0.1)
        └── ## C (1.1)
        ");
    }

    #[test]
    fn test_build_loose_tree_example2() {
        // Example 2: A(2), B(4), B1(3), C(2), D(3), E(3), F(4)
        let root = build_padded_tree(
            items(&[
                (2, "A"),
                (4, "B"),
                (3, "B1"),
                (2, "C"),
                (3, "D"),
                (3, "E"),
                (4, "F"),
            ]),
            None,
        )
        .unwrap();
        let output = format!("{}", root);
        assert_snapshot!(output, @r"
        # <implicit> (0)
        ├── ## A (0.1)
        │   ├── ### <implicit> (0.1.0)
        │   │   └── #### B (0.1.0.1)
        │   └── ### B1 (0.1.1)
        └── ## C (0.2)
            ├── ### D (0.2.1)
            └── ### E (0.2.2)
                └── #### F (0.2.2.1)
        ");
    }

    #[test]
    fn test_build_padded_tree_with_root_level_zero() {
        let root = build_padded_tree(
            items(&[(3, "H1"), (6, "H2"), (4, "H1b")]),
            Some(0),
        )
        .unwrap();
        let output = format!("{}", root);
        assert_snapshot!(output, @r"
         <implicit> (0)
        └── # <implicit> (0.0)
            └── ## <implicit> (0.0.0)
                └── ### H1 (0.0.0.1)
                    ├── #### <implicit> (0.0.0.1.0)
                    │   └── ##### <implicit> (0.0.0.1.0.0)
                    │       └── ###### H2 (0.0.0.1.0.0.1)
                    └── #### H1b (0.0.0.1.1)
        ");
    }

    #[test]
    fn test_build_padded_tree_root_level_error() {
        // Items start at level 0, but we specify root_level=1 - should error
        let result = build_padded_tree(items(&[(0, "Invalid")]), Some(1));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cannot build"));
    }

    #[test]
    fn test_build_loose_tree_empty() {
        let items: Vec<TestItem> = vec![];
        let result = build_padded_tree(items, None);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("empty"));
    }
}
