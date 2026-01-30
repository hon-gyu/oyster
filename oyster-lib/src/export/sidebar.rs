use super::file_tree_component;
use crate::link::Referenceable;
use maud::{Markup, html};
use std::path::{Path, PathBuf};

/// Render file explorer for individual pages
pub fn render_explorer<F>(
    vault_slug: &Path,
    referenceables: &[Referenceable],
    vault_path_to_slug: F,
) -> Markup
where
    F: Fn(&PathBuf) -> Option<String>,
{
    let explorer = file_tree_component::render_file_tree(
        vault_slug,
        referenceables,
        vault_path_to_slug,
    );

    html! {
        aside .file-explorer {
            ( explorer )
        }
    }
}
