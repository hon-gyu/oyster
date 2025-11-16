mod extract;
mod resolve;
mod types;
mod utils;

pub use extract::{scan_note, scan_vault};
pub use resolve::build_links;
pub use types::{Link, Reference, ReferenceKind, Referenceable};
