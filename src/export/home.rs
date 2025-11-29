use super::utils::get_relative_dest;
use crate::heading::{Hierarchical, TreeNode, build_tree};
use crate::link::Referenceable;
use maud::{Markup, html};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Render the link to the home page
pub fn render_simple_home_back_nav(
    note_slug_path: &Path,
    home_slug_path: &Path,
) -> Markup {
    // Calculate relative path to home (index.html)
    let home_href = get_relative_dest(note_slug_path, home_slug_path);
    let home_href = format!("{}.html", home_href);

    html! {a href=(home_href) class="breadcrumb" { "Home" }}
}

pub fn render_breadcrumb(
    note_slug_path: &Path,
    home_slug_path: &Path,
    // vault_path_to_slug_map: &HashMap<PathBuf, String>,
) -> Markup {
    let home_href = get_relative_dest(note_slug_path, home_slug_path);
    let home_href = format!("{}.html", home_href);
    let home_anchor =
        html! {a href=(home_href) .breadcrumb.home-link { "Home" }};

    let mut crumbs = Vec::new();
    for ancestor in note_slug_path.ancestors().skip(1) {
        if ancestor.file_name().is_none() {
            continue;
        };
        let ancestor_file_name =
            ancestor.file_name().unwrap().to_string_lossy();
        let ancestor_href = get_relative_dest(note_slug_path, ancestor);
        let ancestor_href = format!("{}.html", ancestor_href);
        let ancestor_anchor = html! {
            a href=(ancestor_href) { (ancestor_file_name) }
        };
        crumbs.push(ancestor_anchor);
    }

    html! {
        nav class="breadcrumb" {
            p { (home_anchor)
            @for crumb in crumbs {
                (" > ") (crumb) }
            }
        }
    }
}

/// Render the home page
/// File tree item for home page
#[derive(Debug)]
struct FileTreeItem {
    name: String,
    path: PathBuf,
    slug: Option<String>, // None for directories, Some for files
    depth: usize,
}

impl FileTreeItem {
    fn is_directory(&self) -> bool {
        self.slug.is_none()
    }
}

impl Hierarchical for FileTreeItem {
    fn level(&self) -> usize {
        self.depth
    }
}

/// Render a file tree node recursively
fn render_file_tree_node(node: &TreeNode<FileTreeItem>) -> Markup {
    if node.value.is_directory() {
        // Directory with children
        html! {
            li class="directory" {
                details closed {
                    summary { (node.value.name) "/" }
                    @if !node.children.is_empty() {
                        ul {
                            @for child in &node.children {
                                (render_file_tree_node(child))
                            }
                        }
                    }
                }
            }
        }
    } else {
        // File (note)
        let slug = node.value.slug.as_ref().unwrap();
        html! {
            li class="file" {
                a href=(slug) { (node.value.name) }
            }
        }
    }
}

pub fn render_home_page(
    referenceables: &[Referenceable],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
    home_slug_path: &Path,
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

        // Add the file itself
        let file_depth = note_path.components().count() - 1;
        let name = note_path.file_stem().unwrap().to_string_lossy().to_string();
        let slug = vault_path_to_slug_map.get(note_path).unwrap().clone();
        items.push(FileTreeItem {
            name,
            path: note_path.clone(),
            slug: Some(slug),
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
        nav class="file-tree" {
            ul {
                @for node in tree {
                    (render_file_tree_node(&node))
                }
            }
        }
    }
}
