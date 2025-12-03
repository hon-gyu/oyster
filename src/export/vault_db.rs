//! ...
//!
//! File-level information:
//!   - file referenceable: note and asset
//!   - in-note referenceable: heading, block
//!   - reference: outgoing edges
//!   - frontmatter: note referenceable path |-> frontmatter
//!
//! Vault-level information:
//!   - links: matched edges
//!   - unresolved references
//!   - map: file vault path |-> slug path
//!   - map: file vault path |-> title
//!   - map: file valut path |-> in-note referenceable range |-> anchod id
//!   - map: reference path |-> reference range |-> anchor id
//!     - where the anchor id = the byte range of the reference
//!
//! Render a note (file referenceable) to HTML
//! - Args
//!   - its vault path
//!   - its content (Tree)
//!   - vault-level info
use super::frontmatter;
use super::utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
    get_relative_dest, range_to_anchor_id, title_from_path,
};
use crate::link::{
    Link as ResolvedLink, Reference, Referenceable, build_links,
};
use serde_yaml::Value;
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub struct FileLevelInfo {
    /// Both file (note and asset) referenceables,
    /// and in-note (heading and block) referenceables
    pub referenceables: Vec<Referenceable>,
    pub references: Vec<Reference>,
    /// Map: file vault path |-> frontmatter
    pub fronmatters: HashMap<PathBuf, Option<Value>>,
}

pub struct VaultLevelInfo {
    links: Vec<ResolvedLink>,
    unresolved: Vec<Reference>,
    // File referenceables
    file_vault_path_to_slug_map: HashMap<PathBuf, String>,
    note_vault_path_to_title_map: HashMap<PathBuf, String>,
    /// file vault path (parent referenceable)
    /// |-> in-note referenceable range
    /// |-> anchor id
    innote_refable_anchor_id_map:
        HashMap<PathBuf, HashMap<Range<usize>, String>>,
    /// reference path |-> (reference range, anchor id)
    reference_anchor_id_map: HashMap<PathBuf, (Range<usize>, String)>,
}

impl VaultLevelInfo {
    // From file-level information to vault-level information
    pub fn new(
        referenceables: &Vec<Referenceable>,
        references: &Vec<Reference>,
        fronmatters: &HashMap<PathBuf, Option<Value>>,
    ) -> VaultLevelInfo {
        let (links, unresolved) =
            // TODO: can we avoid the clone?
            build_links(references.clone(), referenceables.clone());

        // Build map: vault file path |-> slug
        let vault_file_paths = referenceables
            .iter()
            .filter(|referenceable| !referenceable.is_innote())
            .map(|referenceable| referenceable.path().as_path())
            .collect::<Vec<_>>();
        let file_vault_path_to_slug_map =
            build_vault_paths_to_slug_map(&vault_file_paths);

        let note_vault_paths = referenceables
            .iter()
            .filter(|referenceable| {
                matches!(referenceable, Referenceable::Note { .. })
            })
            .map(|referenceable| referenceable.path().as_path())
            .collect::<Vec<_>>();

        // Build map for notes: vault file path |-> title
        let note_vault_path_to_title_map = note_vault_paths
            .iter()
            .map(|&path| {
                let title = fronmatters
                    .get(path)
                    .expect("Did not find maybe frontmatter for note")
                    .as_ref()
                    .and_then(|fm| frontmatter::get_title(&fm))
                    .unwrap_or_else(|| title_from_path(path));
                (path.to_path_buf(), title)
            })
            .collect::<HashMap<_, _>>();

        // Build map: vault file path |-> in-note refable range |-> anchor id map
        let referenceable_refs = referenceables.iter().collect::<Vec<_>>();
        let innote_refable_anchor_id_map =
            build_in_note_anchor_id_map(&referenceable_refs);

        // There's an implicit map: reference path |-> reference range |-> anchor id
        // where the anchor id IS the byte range of the reference
        let reference_to_range_and_anchor_id_map: HashMap<
            PathBuf,
            (Range<usize>, String),
        > = references
            .iter()
            .map(|r| {
                let ref_path = r.path.clone();
                let ref_range = r.range.clone();
                let anchor_id = range_to_anchor_id(&ref_range);
                (ref_path, (ref_range, anchor_id))
            })
            .collect();

        VaultLevelInfo {
            links,
            unresolved,
            file_vault_path_to_slug_map,
            note_vault_path_to_title_map,
            innote_refable_anchor_id_map,
            reference_anchor_id_map: reference_to_range_and_anchor_id_map,
        }
    }
}

pub trait VaultDB {
    // Getter
    // ====================
    fn get_referenceables(&self) -> &[Referenceable];
    fn get_resolved_links(&self) -> &[ResolvedLink];
    fn get_unresolved_references(&self) -> &[Reference];
    // File referenceables
    fn get_slug_from_file_vault_path(&self, path: &PathBuf) -> Option<String>;
    fn get_title_from_note_vault_path(&self, path: &PathBuf) -> Option<String>;
    /// file vault path (parent referenceable)
    /// |-> in-note referenceable range
    /// |-> anchor id
    fn get_innote_refable_anchor_id(
        &self,
        path: &PathBuf,
        refable_range: &Range<usize>,
    ) -> Option<String>;

    fn get_reference_anchor_id(
        &self,
        path: &PathBuf,
        ref_range: &Range<usize>,
    ) -> Option<String>;

    // Derived
    // ====================

    /// Not very efficient.
    /// We override this with optimized O(1) lookup using pre-computed map
    /// in static store
    fn get_tgt_slug_from_src(
        &self,
        src_note_vault_path: &Path,
        range: &Range<usize>,
    ) -> Option<String> {
        self.get_resolved_links()
            .iter()
            .filter(|link| link.src_path_eq(src_note_vault_path))
            .filter_map(|link| {
                let src_range = &link.from.range;
                if src_range != range {
                    None
                } else {
                    let tgt = &link.to;
                    let tgt_slug = self
                        .get_slug_from_file_vault_path(tgt.path())
                        .expect("link target path not found");
                    let base_slug = self
                        .get_slug_from_file_vault_path(tgt.path())
                        .expect("vault path not found");
                    let rel_tgt_slug = get_relative_dest(
                        Path::new(&base_slug),
                        Path::new(&tgt_slug),
                    );
                    let tgt_anchor_id = match tgt {
                        Referenceable::Block {
                            path,
                            range: tgt_range,
                            ..
                        }
                        | Referenceable::Heading {
                            path,
                            range: tgt_range,
                            ..
                        } => self.get_innote_refable_anchor_id(path, tgt_range),
                        _ => None,
                    };
                    let dest = if let Some(tgt_anchor_id) = tgt_anchor_id {
                        format!("{}#{}", rel_tgt_slug, tgt_anchor_id.clone())
                    } else {
                        format!("{}", rel_tgt_slug)
                    };
                    Some(dest)
                }
            })
            .next()
    }
}

/// Stores file-level information and vault-level information
/// as well as some helper maps
pub struct StaticVaultStore {
    file_level_info: FileLevelInfo,
    vault_level_info: VaultLevelInfo,
    /// Derived map of
    /// src (this) reference's byte range
    /// |->
    /// (
    //    tgt_slug.html#anchor_id | tgt_slug.html | tgt_slug.png,
    ///   resolved link
    //  )
    ref_to_tgt_slug_and_link_map:
        HashMap<(PathBuf, Range<usize>), (String, ResolvedLink)>,
}

impl StaticVaultStore {
    pub fn new(
        file_level_info: FileLevelInfo,
        vault_level_info: VaultLevelInfo,
    ) -> Self {
        // Unpack
        let references = &file_level_info.references;
        let resolved_links = &vault_level_info.links;
        let vault_path_to_slug_map =
            &vault_level_info.file_vault_path_to_slug_map;
        let innote_refable_anchor_id_map =
            &vault_level_info.innote_refable_anchor_id_map;

        // Outgoing links
        // build a map of:
        //   (
        //      src reference's vault path
        //      src reference's byte range
        //   )
        //   |->
        //   (
        //     tgt_slug.html#anchor_id | tgt_slug.html | tgt_slug.png,
        //     resolved link
        //   )
        let src_to_tgt_slug_and_link_map: HashMap<
            (PathBuf, Range<usize>),
            (String, ResolvedLink),
        > = resolved_links
            .iter()
            .map(|link| {
                let src_note_vault_path = link.from.path.clone();
                let src_ref_range = link.from.range.clone();
                let tgt = &link.to;

                let tgt_slug = vault_path_to_slug_map
                    .get(tgt.path())
                    .expect("link target path not found");
                let base_slug = vault_path_to_slug_map
                    .get(&src_note_vault_path)
                    .expect("vault path not found");
                let rel_tgt_slug = get_relative_dest(
                    Path::new(base_slug),
                    Path::new(tgt_slug),
                );
                let tgt_anchor_id = match tgt {
                    Referenceable::Block {
                        path,
                        range: tgt_range,
                        ..
                    }
                    | Referenceable::Heading {
                        path,
                        range: tgt_range,
                        ..
                    } => innote_refable_anchor_id_map
                        .get(path)
                        .and_then(|anchor_id_map| anchor_id_map.get(tgt_range)),
                    _ => None,
                };
                let dest = if let Some(tgt_anchor_id) = tgt_anchor_id {
                    format!("{}#{}", rel_tgt_slug, tgt_anchor_id.clone())
                } else {
                    format!("{}", rel_tgt_slug)
                };
                ((src_note_vault_path, src_ref_range), (dest, link.clone()))
            })
            .collect();

        StaticVaultStore {
            file_level_info,
            vault_level_info,
            ref_to_tgt_slug_and_link_map: src_to_tgt_slug_and_link_map,
        }
    }
}

impl VaultDB for StaticVaultStore {
    fn get_referenceables(&self) -> &[Referenceable] {
        &self.file_level_info.referenceables
    }

    fn get_resolved_links(&self) -> &[ResolvedLink] {
        &self.vault_level_info.links
    }

    fn get_unresolved_references(&self) -> &[Reference] {
        &self.vault_level_info.unresolved
    }

    fn get_slug_from_file_vault_path(&self, path: &PathBuf) -> Option<String> {
        self.vault_level_info
            .file_vault_path_to_slug_map
            .get(path)
            .cloned()
    }

    fn get_title_from_note_vault_path(&self, path: &PathBuf) -> Option<String> {
        self.vault_level_info
            .note_vault_path_to_title_map
            .get(path)
            .cloned()
    }

    fn get_innote_refable_anchor_id(
        &self,
        path: &PathBuf,
        refable_range: &Range<usize>,
    ) -> Option<String> {
        self.vault_level_info
            .innote_refable_anchor_id_map
            .get(path)
            .and_then(|anchor_id_map| anchor_id_map.get(refable_range))
            .cloned()
    }

    fn get_reference_anchor_id(
        &self,
        path: &PathBuf,
        ref_range: &Range<usize>,
    ) -> Option<String> {
        self.vault_level_info
            .reference_anchor_id_map
            .get(path)
            .and_then(|(range, anchor_id)| {
                if range == ref_range {
                    Some(anchor_id.clone())
                } else {
                    None
                }
            })
    }

    // Override with optimized O(1) lookup using pre-computed map
    fn get_tgt_slug_from_src(
        &self,
        src_note_vault_path: &Path,
        range: &Range<usize>,
    ) -> Option<String> {
        self.ref_to_tgt_slug_and_link_map
            .get(&(src_note_vault_path.to_path_buf(), range.clone()))
            .map(|(dest, _link)| dest.clone())
    }
}
