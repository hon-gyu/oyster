use super::utils::get_relative_dest;
use crate::hierarchy::{FileTreeItem, TreeNode, build_file_tree};
use crate::link::Referenceable;
use maud::{Markup, html};
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

/// Render a file tree node recursively
fn render_file_tree_node(node: &TreeNode<FileTreeItem>) -> Markup {
    if node.value.is_directory() {
        // Directory with children
        html! {
            li class="directory" {
                details open {
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

pub fn render_home_page_file_tree<F>(
    referenceables: &[Referenceable],
    vault_path_to_slug: F,
    home_slug_path: &Path,
) -> Markup
where
    F: Fn(&PathBuf) -> Option<String>,
{
    let tree =
        build_file_tree(home_slug_path, referenceables, &vault_path_to_slug);

    html! {
        nav .home-page.file-tree {
            ul {
                @for node in tree {
                    (render_file_tree_node(&node))
                }
            }
        }
    }
}
