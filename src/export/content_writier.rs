use super::utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
};
use crate::ast::{Node, NodeKind, Tree};
use crate::link::types::*;
use crate::link::{build_links, scan_vault};
use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

///
///
/// Placeholder for the replacement of `generate_site`
/// TODO(refactor): move this
fn render_vault(
    vault_path: &Path,
    output_dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all(output_dir)?;

    // Scan the vault and build links
    let (referenceables, references) = scan_vault(vault_path, vault_path);
    let (links, _unresolved) = build_links(references, referenceables.clone());

    // Build vault file path to slug map
    let vault_file_paths = referenceables
        .iter()
        .filter(|referenceable| !referenceable.is_innote())
        .map(|referenceable| referenceable.path().as_path())
        .collect::<Vec<_>>();
    let vault_path_to_slug_map =
        build_vault_paths_to_slug_map(&vault_file_paths);

    // Build in-note anchor id map
    let referenceable_refs = referenceables.iter().collect::<Vec<_>>();
    let innote_refable_anchor_id_map =
        build_in_note_anchor_id_map(&referenceable_refs);

    Ok(())
}

/// Render content
///
/// Input:
/// - info about this page
///   - vault path
///   - tree
/// - link info
///   - resolved links
///   - valut path to slug map
///   - referenceable to anchor id map
fn render_content(
    tree: &Tree,
    vault_path: &Path,
    resolved_links: &[Link],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
    innote_refable_anchor_id_map: &HashMap<
        PathBuf,
        HashMap<Range<usize>, String>,
    >,
) -> String {
    // Outgoing links
    // build a map of: src (this) reference's byte range |-> tgt slug
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

    // Incoming links
    // obtain a map of: tgt (this) referable's byte range |-> anchor id
    let in_note_anchor_id_map: &HashMap<Range<usize>, String> =
        innote_refable_anchor_id_map
            .get(vault_path)
            .expect("vault path not found");

    let mut buffer = String::new();
    render_node(
        &tree.root_node,
        vault_path,
        &reference_slug_dest_map,
        &in_note_anchor_id_map,
        &mut buffer,
    );
    todo!()
}

fn render_node(
    node: &Node,
    vault_path: &Path,
    ref_slug_map: &HashMap<Range<usize>, String>,
    refable_anchor_id_map: &HashMap<Range<usize>, String>,
    buffer: &mut String,
) {
    let range = node.start_byte..node.end_byte;
}
