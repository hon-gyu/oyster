mod extract;
mod resolve;
mod types;
mod utils;

pub use extract::{mut_transform_referenceable_path, scan_note, scan_vault};
pub use resolve::build_links;
pub use types::{Link, Reference, ReferenceKind, Referenceable};
pub use utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map, percent_decode,
};
