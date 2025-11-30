use crate::hierarchy::{FileTreeItem, TreeNode, build_file_tree};
use crate::link::Referenceable;
use maud::{Markup, html};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Render a file tree
///
/// Arguments:
/// - `vault_slug`: path in the vault of the current page
/// - `referenceables`: all referenceables in the vault
/// - `vault_path_to_slug_map`
pub fn render_file_tree(
    vault_slug: &Path,
    referenceables: &[Referenceable],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
) -> Markup {
    let tree =
        build_file_tree(vault_slug, referenceables, vault_path_to_slug_map);
    html! {
        nav .file-tree {
            span { "." }  // Root
            @for (idx, node) in tree.iter().enumerate() {
                (render_file_tree_node(node, idx == tree.len() - 1, ""))
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
            div .tree-item.directory {
                span .connector-prefix { (prefix)(connector) }
                details {
                    summary { (node.value.name)"/" }
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
            div .tree-item.file {
                span .connector-prefix { (prefix)(connector) }
                a href=(slug) { (node.value.name) }
            }
        }
    }
}
