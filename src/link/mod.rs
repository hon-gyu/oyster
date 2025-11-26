pub mod extract;
pub mod resolve;
pub mod types;
mod utils;

pub use extract::{mut_transform_referenceable_path, scan_note, scan_vault};
pub use resolve::build_links;
pub use types::{Link, Reference, ReferenceKind, Referenceable};
pub use utils::percent_decode;
