//! Generic trait and utilities for building hierarchical tree structures
//! The absolute value of level doesn't matter, only the relative order.
use crate::export::utils::get_relative_dest;
use crate::link::Referenceable;
use std::collections::BTreeSet;
use std::fmt;
use std::path::{Path, PathBuf};

/// Trait for types that have a hierarchical level
pub trait Hierarchical {
    fn level(&self) -> usize;
}

/// Trait for hierarchical types that can create default items at specific levels
/// This is used for gap-filling in loose tree construction
pub trait HierarchicalWithDefaults: Hierarchical {
    /// Create a default item at a specific level for gap filling
    fn default_at_level(level: usize) -> Self;
}

/// A sub-tree in a hierarchy that contains the node itself and its children
#[derive(Debug)]
pub struct HierarchyItem<T> {
    pub value: T,
    pub children: Vec<HierarchyItem<T>>,
}

impl<T> HierarchyItem<T> {
    pub fn new(value: T) -> Self {
        Self {
            value,
            children: Vec::new(),
        }
    }

    /// Chained indexing
    pub fn query_by_index(&self, index: &[usize]) -> Option<&HierarchyItem<T>> {
        todo!()
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
        // Print current node
        writeln!(f, "{}", self.value)?;

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
pub fn build_compact_tree<T: Hierarchical>(
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

/// Build a tree from a flat list of hierarchical items
///
/// Empty level gets index 0
/// Real items get indices starting from 1 based on their order among siblings at each level
///
/// Example:
/// - eg: 1
///   input: A(1), B(3), C(2)
///   output:
///     - A (1)
///       - default as H2 (1.0)
///         - B (1.0.1)
///       - C (1.1)
/// - eg: 2
///   input: A(2), B(4), B1(3), C(2), D(3), E(3), F(4)
///   output: |
///     | number  | title |
///     | ------- | :---: |
///     | 0.1     |   A   |
///     | 0.1.0.1 |   B   |
///     | 0.1.1   |  B1   |
///     | 0.2     |   C   |
///     | 0.2.1   |   D   |
///     | 0.2.2   |   E   |
///     | 0.2.2.1 |   F   |
///
/// Contract:
/// - the return items covers all levels from the minimum to the maximum of
///     the input items. We create new items for empty levels.
/// - len(return items) >= len(input items)
/// - raise if minimum level is non-positive
pub fn build_loose_tree<T: HierarchicalWithDefaults>(
    items: Vec<T>,
) -> Result<(Vec<HierarchyItem<T>>, Vec<Vec<usize>>), String> {
    if items.is_empty() {
        return Ok((Vec::new(), Vec::new()));
    }

    // Find min level
    let min_level = items.iter().map(|item| item.level()).min().unwrap();

    // Check if minimum level is non-positive
    if min_level <= 0 {
        return Err("Minimum level must be positive (> 0)".to_string());
    }

    let mut roots = Vec::new();
    let mut stack: Vec<(usize, HierarchyItem<T>, bool)> = Vec::new(); // (level, node, is_default)
    let mut section_numbers = Vec::new(); // Store section number for each input item

    // Track the number of real children at each level in the current path
    let mut real_child_counts: Vec<usize> = vec![0; 10]; // Assuming max 10 levels

    // Ensure level 1 exists - insert defaults from 1 to min_level - 1
    for level in 1..min_level {
        let default_node = HierarchyItem::new(T::default_at_level(level));
        stack.push((level, default_node, true));
    }

    for item in items {
        let curr_level = item.level();

        // Pop items at same or deeper level
        while let Some((top_level, _, _)) = stack.last() {
            if *top_level >= curr_level {
                let (_, popped, _) = stack.pop().unwrap();
                if let Some((_, parent, _)) = stack.last_mut() {
                    parent.children.push(popped);
                } else {
                    roots.push(popped);
                }
                // Reset count for this level
                if stack.len() < real_child_counts.len() {
                    real_child_counts[stack.len()] = 0;
                }
            } else {
                break;
            }
        }

        // Insert default items for level gaps between stack top and current
        if let Some((top_level, _, _)) = stack.last() {
            for level in (top_level + 1)..curr_level {
                let default_node =
                    HierarchyItem::new(T::default_at_level(level));
                stack.push((level, default_node, true));
            }
        } else if curr_level > 1 {
            // Stack is empty but current item is not at level 1
            for level in 1..curr_level {
                let default_node =
                    HierarchyItem::new(T::default_at_level(level));
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
        section_numbers.push(section);

        let node = HierarchyItem::new(item);
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

    Ok((roots, section_numbers))
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
    fn default_at_level(level: usize) -> Self {
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
    build_compact_tree(items)
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
        fn default_at_level(level: usize) -> Self {
            Self {
                level,
                name: "<default>".to_string(),
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
            build_compact_tree(items(&[(1, "H1"), (2, "H2"), (3, "H3")]));
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
            build_compact_tree(items(&[(1, "H1"), (3, "H3"), (2, "H2")]));
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
            build_compact_tree(items(&[(1, "H1a"), (2, "H2"), (1, "H1b")]));
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
        let tree = build_compact_tree(items(&[
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

    #[test]
    fn test_build_loose_tree_example1() {
        // Example 1: A(1), B(3), C(2)
        let (roots, section_numbers) =
            build_loose_tree(items(&[(1, "A"), (3, "B"), (2, "C")])).unwrap();
        assert_eq!(roots.len(), 1);
        let output = format!("{}", roots[0]);
        assert_snapshot!(output, @r"
        # A
        ├── ## <default>
        │   └── ### B
        └── ## C
        ");

        // Verify section numbers
        let section_output = format!("{:?}", section_numbers);
        assert_snapshot!(section_output, @"[[1], [1, 0, 1], [1, 1]]");
    }

    #[test]
    fn test_build_loose_tree_example2() {
        // Example 2: A(2), B(4), B1(3), C(2), D(3), E(3), F(4)
        let (roots, section_numbers) = build_loose_tree(items(&[
            (2, "A"),
            (4, "B"),
            (3, "B1"),
            (2, "C"),
            (3, "D"),
            (3, "E"),
            (4, "F"),
        ]))
        .unwrap();
        assert_eq!(roots.len(), 1);
        let output = format!("{}", roots[0]);
        assert_snapshot!(output, @r"
        # <default>
        ├── ## A
        │   ├── ### <default>
        │   │   └── #### B
        │   └── ### B1
        └── ## C
            ├── ### D
            └── ### E
                └── #### F
        ");

        // Verify section numbers
        let section_output = format!("{:?}", section_numbers);
        assert_snapshot!(section_output, @"[[0, 1], [0, 1, 0, 1], [0, 1, 1], [0, 1], [0, 1, 1], [0, 1, 1], [0, 1, 1, 1]]");
    }

    #[test]
    fn test_build_loose_tree_non_positive_level() {
        let result = build_loose_tree(items(&[(0, "Invalid")]));
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("positive"));
    }

    #[test]
    fn test_build_loose_tree_empty() {
        let items: Vec<TestItem> = vec![];
        let (roots, section_numbers) = build_loose_tree(items).unwrap();
        assert_eq!(roots.len(), 0);
        assert_eq!(section_numbers.len(), 0);
    }
}
