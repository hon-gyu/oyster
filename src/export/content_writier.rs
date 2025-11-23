use super::utils::build_in_note_anchor_id_map;
use crate::ast::{Node, NodeKind, Tree};
use crate::link::types::*;
use std::collections::HashMap;
use std::ops::Range;
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

    // Find referenceable that presents in resolved links in other notes
    // and obtain the slugs of destination
    let matched_references = resolved_links
        .iter()
        .filter(|link| link.tgt_path_eq(vault_path))
        .map(|link| &link.from)
        .collect::<Vec<_>>();
    let reference_slug_dest_map: HashMap<Range<usize>, String> =
        matched_references
            .iter()
            .map(|reference| {
                let range = &reference.range;
                let slug = vault_path_to_slug_map
                    .get(&reference.path)
                    .expect("reference path not found");
                (range.clone(), slug.clone())
            })
            .collect();

    todo!()
}

fn render_node(
    node: &Node,
    vault_path: &Path,
    matched_references: Vec<&Reference>,
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
    anchor_id_map: &HashMap<Range<usize>, String>,
    buffer: &mut String,
) {
    let range = node.start_byte..node.end_byte;
}
