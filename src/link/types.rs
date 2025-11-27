/// Extracts references and referenceables from a Markdown AST.
/// Referenceable can be
///   - items in a note: headings, block
///   - notes: markdown files
///   - assets other than notes: images, videos, audios, PDFs, etc.
use pulldown_cmark::HeadingLevel;
use std::ops::Range;
use std::path::{Path, PathBuf};

// ====================
// Referenceable
// ====================

#[derive(Clone, Debug, PartialEq)]
pub enum BlockReferenceableKind {
    InlineParagraph,
    InlineListItem,
    Paragraph,
    List,
    BlockQuote,
    Table,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Referenceable {
    Asset {
        path: PathBuf,
    },
    Note {
        path: PathBuf,
        children: Vec<Referenceable>,
    },
    Heading {
        path: PathBuf,
        level: HeadingLevel,
        text: String,
        /// The exact range of the heading event from start to end
        range: Range<usize>,
    },
    Block {
        path: PathBuf,
        identifier: String,
        kind: BlockReferenceableKind,
        /// The exact range of the event, including
        /// - paragraph
        /// - list item
        /// - block quote
        /// - table
        /// - list
        range: Range<usize>,
    },
}

impl Referenceable {
    pub fn path(&self) -> &PathBuf {
        match self {
            Referenceable::Asset { path, .. } => path,
            Referenceable::Note { path, .. } => path,
            Referenceable::Heading { path, .. } => path,
            Referenceable::Block { path, .. } => path,
        }
    }

    pub fn is_innote(&self) -> bool {
        match self {
            Referenceable::Heading { .. } => true,
            Referenceable::Block { .. } => true,
            _ => false,
        }
    }

    pub fn add_in_note_referenceables(
        &mut self,
        referenceables: Vec<Referenceable>,
    ) -> () {
        match self {
            Referenceable::Note { children, .. } => {
                children.extend(referenceables);
            }
            _ => {}
        }
    }
}

// ====================
// Reference
// ====================
#[derive(Clone, Debug, PartialEq)]
pub enum ReferenceKind {
    WikiLink,
    MarkdownLink,
    Embed,
}

/// A wikilink or inline markdown link in markdown file
///
/// `[[x#y|z]]` or `[a](b)`
#[derive(Clone, Debug, PartialEq)]
pub struct Reference {
    pub kind: ReferenceKind,
    pub path: PathBuf,
    /// The byte range of the raw link in markdown file
    pub range: Range<usize>,
    /// The `dest_url` of the raw link in markdown file
    /// percent-decoded if it's a inline link (markdown link)
    pub dest: String,
    pub display_text: String,
}

impl std::fmt::Display for Referenceable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Referenceable::Asset { path } => {
                write!(f, "Asset: {}", path.display())
            }
            Referenceable::Note { path, .. } => {
                write!(f, "Note: {}", path.display())
            }
            Referenceable::Heading {
                path, level, text, ..
            } => {
                write!(
                    f,
                    "Heading: {} level: {}, text: {}",
                    path.display(),
                    level,
                    text
                )
            }
            Referenceable::Block {
                path,
                identifier,
                kind,
                range,
            } => {
                write!(
                    f,
                    "Block: {}, {}, {:?}, range: {:?}",
                    path.display(),
                    identifier,
                    kind,
                    range
                )
            }
        }
    }
}

// ====================
// Link
// ====================

/// A reference to a referenceable
#[derive(Clone, Debug, PartialEq)]
pub struct Link {
    pub from: Reference,
    pub to: Referenceable,
}

impl Link {
    pub fn src_path_eq(&self, path: &Path) -> bool {
        self.from.path == path
    }

    pub fn tgt_path_eq(&self, path: &Path) -> bool {
        self.to.path() == path
    }

    /// Return true if the link is pointing to an in-note referenceable
    pub fn is_in_note(&self) -> bool {
        match &self.to {
            Referenceable::Heading { .. } => true,
            Referenceable::Block { .. } => true,
            _ => false,
        }
    }
    /// The byte range of the target in-note referenceable
    ///
    /// Return None if the link is pointing to a file
    fn tgt_range(&self) -> Option<Range<usize>> {
        match &self.to {
            Referenceable::Heading { range, .. } => Some(range.clone()),
            Referenceable::Block { range, .. } => Some(range.clone()),
            _ => None,
        }
    }
}
