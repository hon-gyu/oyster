use super::utils::build_in_note_anchor_id_map;
use crate::ast::Tree;
use crate::link::types::*;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Render content
///
/// Arguments:
/// - file paths in vault to slug map
fn render_content(
    tree: &Tree,
    vault_path: &Path,
    resolved_links: &[Link],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
) -> String {
    // Find referenceable that presents in resolved links in this note
    // and generate anchor ids for them
    let matched_innote_refables = resolved_links
        .iter()
        .filter(|link| link.src_path_eq(vault_path))
        .map(|link| &link.to)
        .collect::<Vec<_>>();
    let in_note_anchor_id_map =
        build_in_note_anchor_id_map(&matched_innote_refables);

    let matched_references = resolved_links
        .iter()
        .filter(|link| link.tgt_path_eq(vault_path))
        .map(|link| &link.from)
        .collect::<Vec<_>>();
    todo!()
}
