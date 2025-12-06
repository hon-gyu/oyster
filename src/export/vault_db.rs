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
use crate::ast::Tree as ASTTree;
use crate::link::{
    Link as ResolvedLink, Reference, Referenceable, build_links, scan_vault,
};
use serde_yaml::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
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
    /// reference path |-> reference range |-> anchor id
    reference_anchor_id_map: HashMap<PathBuf, HashMap<Range<usize>, String>>,
}

impl VaultLevelInfo {
    // From file-level information to vault-level information
    pub fn new(
        referenceables: &Vec<Referenceable>,
        references: &Vec<Reference>,
        fronmatters: &HashMap<PathBuf, Option<Value>>,
    ) -> VaultLevelInfo {
        let (links, unresolved) = build_links(references, referenceables);

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
                    .and_then(frontmatter::get_title)
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
        let mut reference_to_range_and_anchor_id_map: HashMap<
            PathBuf,
            HashMap<Range<usize>, String>,
        > = HashMap::new();
        for r in references {
            let ref_path = r.path.clone();
            let ref_range = &r.range;
            match reference_to_range_and_anchor_id_map.entry(ref_path) {
                Entry::Occupied(mut entry) => {
                    let range_map = entry.get_mut();
                    range_map.insert(
                        ref_range.clone(),
                        range_to_anchor_id(ref_range),
                    );
                }
                Entry::Vacant(entry) => {
                    let mut range_map = HashMap::new();
                    range_map.insert(
                        ref_range.clone(),
                        range_to_anchor_id(ref_range),
                    );
                    entry.insert(range_map);
                }
            }
        }

        // references
        //     .iter()
        //     .map(|r| {
        //         let ref_path = r.path.clone();
        //         let ref_range = r.range.clone();
        //         let anchor_id = range_to_anchor_id(&ref_range);
        //         (ref_path, (ref_range, anchor_id))
        //     })
        //     .collect();

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
    fn get_vault_root_dir(&self) -> &Path;
    fn get_ast_tree_from_note_vault_path(
        &self,
        path: &PathBuf,
    ) -> Option<ASTTree>;
    // File-level info getters
    fn get_referenceables(&self) -> &[Referenceable];
    fn get_resolved_links(&self) -> &[ResolvedLink];
    fn get_unresolved_references(&self) -> &[Reference];

    // File referenceables
    fn get_slug_from_file_vault_path(&self, path: &PathBuf) -> Option<&str>;
    fn get_title_from_note_vault_path(&self, path: &PathBuf) -> Option<&str>;

    /// file vault path (parent referenceable)
    /// |-> in-note referenceable range
    /// |-> anchor id
    fn get_innote_refable_anchor_id(
        &self,
        path: &PathBuf,
        refable_range: &Range<usize>,
    ) -> Option<&str>;
    fn get_reference_anchor_id(
        &self,
        path: &PathBuf,
        ref_range: &Range<usize>,
    ) -> Option<&str>;
    fn get_frontmatter(&self, path: &PathBuf) -> Option<&Value>;

    // Derived
    fn get_note_vault_paths(&self) -> Vec<&PathBuf> {
        self.get_referenceables()
            .iter()
            .filter(|referenceable| {
                matches!(referenceable, Referenceable::Note { .. })
            })
            .map(|referenceable| referenceable.path())
            .collect()
    }

    fn get_tgt_from_src(
        &self,
        src_note_vault_path: &Path,
        range: &Range<usize>,
    ) -> Option<&Referenceable> {
        self.get_resolved_links()
            .iter()
            .filter(|link| link.src_path_eq(src_note_vault_path))
            .filter_map(|link| {
                let src_range = &link.from.range;
                if src_range != range {
                    None
                } else {
                    Some(&link.to)
                }
            })
            .next()
    }

    /// The default implentation is not very efficient.
    /// We override this with optimized O(1) lookup using pre-computed map
    /// in static store
    fn get_tgt_slug_from_src(
        &self,
        src_note_vault_path: &Path,
        range: &Range<usize>,
    ) -> Option<String> {
        let tgt = self.get_tgt_from_src(src_note_vault_path, range)?;

        let tgt_slug = self
            .get_slug_from_file_vault_path(tgt.path())
            .expect("link target path not found");
        let base_slug = self
            .get_slug_from_file_vault_path(tgt.path())
            .expect("vault path not found");
        let rel_tgt_slug =
            get_relative_dest(Path::new(&base_slug), Path::new(&tgt_slug));

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
            format!("{}#{}", rel_tgt_slug, tgt_anchor_id)
        } else {
            rel_tgt_slug.to_string()
        };
        Some(dest)
    }
}

/// Stores file-level information and vault-level information
/// as well as some helper maps
pub struct StaticVaultStore {
    vault_root_dir: PathBuf,
    ast_tree_cache: RefCell<HashMap<PathBuf, ASTTree<'static>>>,
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
        vault_root_dir: &Path,
        file_level_info: FileLevelInfo,
        vault_level_info: VaultLevelInfo,
    ) -> Self {
        // Unpack
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
                let tgt = &link.to;

                let tgt_slug = vault_path_to_slug_map
                    .get(tgt.path())
                    .expect("link target path not found");
                let base_slug = vault_path_to_slug_map
                    .get(&link.from.path)
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
                    format!("{}#{}", rel_tgt_slug, tgt_anchor_id)
                } else {
                    rel_tgt_slug.to_string()
                };
                (
                    (link.from.path.clone(), link.from.range.clone()),
                    (dest, link.clone()),
                )
            })
            .collect();

        StaticVaultStore {
            vault_root_dir: vault_root_dir.to_path_buf(),
            ast_tree_cache: RefCell::new(HashMap::new()),
            file_level_info,
            vault_level_info,
            ref_to_tgt_slug_and_link_map: src_to_tgt_slug_and_link_map,
        }
    }

    pub fn new_from_dir(vault_root_dir: &Path, filter_publish: bool) -> Self {
        // Scan the vault
        let (fronmatters_vec, referenceables, references) =
            scan_vault(vault_root_dir, vault_root_dir, filter_publish);

        // Build frontmatters map
        let fronmatters = referenceables
            .iter()
            .zip(fronmatters_vec)
            .map(|(referenceable, fm)| (referenceable.path().to_path_buf(), fm))
            .collect();

        // Build file and vault level info
        let file_level_info = FileLevelInfo {
            referenceables,
            references,
            fronmatters,
        };
        let vault_level_info = VaultLevelInfo::new(
            &file_level_info.referenceables,
            &file_level_info.references,
            &file_level_info.fronmatters,
        );

        // Create vault DB
        StaticVaultStore::new(vault_root_dir, file_level_info, vault_level_info)
    }
}

impl VaultDB for StaticVaultStore {
    fn get_vault_root_dir(&self) -> &Path {
        &self.vault_root_dir
    }

    fn get_ast_tree_from_note_vault_path(
        &self,
        path: &PathBuf,
    ) -> Option<ASTTree> {
        // Check cache first
        if let Some(cached_tree) = self.ast_tree_cache.borrow().get(path) {
            return Some(cached_tree.clone());
        }

        // Cache miss: read file, parse, and convert to 'static
        let full_path = self.vault_root_dir.join(path);
        let md_src = std::fs::read_to_string(full_path).ok()?;
        let tree = ASTTree::new(&md_src).into_static();

        // Insert into cache and return clone
        self.ast_tree_cache
            .borrow_mut()
            .insert(path.clone(), tree.clone());
        Some(tree)
    }

    fn get_referenceables(&self) -> &[Referenceable] {
        &self.file_level_info.referenceables
    }

    fn get_resolved_links(&self) -> &[ResolvedLink] {
        &self.vault_level_info.links
    }

    fn get_unresolved_references(&self) -> &[Reference] {
        &self.vault_level_info.unresolved
    }

    fn get_slug_from_file_vault_path(&self, path: &PathBuf) -> Option<&str> {
        self.vault_level_info
            .file_vault_path_to_slug_map
            .get(path)
            .map(|s| s.as_str())
    }

    fn get_title_from_note_vault_path(&self, path: &PathBuf) -> Option<&str> {
        self.vault_level_info
            .note_vault_path_to_title_map
            .get(path)
            .map(|s| s.as_str())
    }

    fn get_innote_refable_anchor_id(
        &self,
        path: &PathBuf,
        refable_range: &Range<usize>,
    ) -> Option<&str> {
        self.vault_level_info
            .innote_refable_anchor_id_map
            .get(path)
            .and_then(|anchor_id_map| anchor_id_map.get(refable_range))
            .map(|s| s.as_str())
    }

    fn get_reference_anchor_id(
        &self,
        path: &PathBuf,
        ref_range: &Range<usize>,
    ) -> Option<&str> {
        self.vault_level_info
            .reference_anchor_id_map
            .get(path)
            .and_then(|range_to_anchor_id_map| {
                range_to_anchor_id_map.get(ref_range)
            })
            .map(|s| s.as_str())
    }

    fn get_frontmatter(&self, path: &PathBuf) -> Option<&Value> {
        self.file_level_info
            .fronmatters
            .get(path)
            .and_then(|opt| opt.as_ref())
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
