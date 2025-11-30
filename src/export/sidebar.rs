use super::home::FileTreeItem;
use super::utils::get_relative_dest;
use crate::heading::{TreeNode, build_tree};
use crate::link::Referenceable;
use maud::{Markup, html};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Render sidebar file explorer for individual pages
pub fn render_sidebar_explorer(
    current_page_slug: &Path,
    referenceables: &[Referenceable],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
) -> Markup {
    use std::collections::BTreeSet;

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
        let absolute_slug = vault_path_to_slug_map.get(note_path).unwrap();
        // Convert to relative path from current page
        let relative_slug =
            get_relative_dest(current_page_slug, Path::new(absolute_slug));
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

    html! {
        aside .sidebar-explorer {
            nav .file-tree {
                @for (idx, node) in tree.iter().enumerate() {
                    (render_file_tree_node(node, idx == tree.len() - 1, ""))
                }
            }
        }
    }
}

/// Render a file tree node recursively with Unicode tree characters
fn render_file_tree_node(
    node: &TreeNode<FileTreeItem>,
    is_last: bool,
    prefix: &str,
) -> Markup {
    let connector = if is_last { "└── " } else { "├── " };
    let new_prefix =
        format!("{}{}", prefix, if is_last { "    " } else { "│   " });

    if node.value.is_directory() {
        // Directory with children
        html! {
            div tree-item.directory {
                details {
                    summary { (prefix)(connector)(node.value.name)"/" }
                    @if !node.children.is_empty() {
                        @for (idx, child) in node.children.iter().enumerate() {
                            (render_file_tree_node(child, idx == node.children.len() - 1, &new_prefix))
                        }
                    }
                }
            }
        }
    } else {
        // File (note)
        let slug = node.value.slug.as_ref().unwrap();
        html! {
            div tree-item.file {
                span { (prefix)(connector) }
                a href=(slug) { (node.value.name) }
            }
        }
    }
}
