/// Extracts references and referenceables from a Markdown AST.
/// Referenceable can be
///   - items in a note: headings, block
///   - notes: markdown files
///   - assets other than notes: images, videos, audios, PDFs, etc.
use pulldown_cmark::HeadingLevel;
use std::ops::Range;
use std::path::PathBuf;

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
        range: Range<usize>,
    },
    Block {
        path: PathBuf,
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

#[derive(Clone, Debug, PartialEq)]
pub enum ReferenceKind {
    WikiLink,
    MarkdownLink,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Reference {
    pub path: PathBuf,
    pub range: Range<usize>,
    pub dest: String,
    pub kind: ReferenceKind,
    pub display_text: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Link {
    pub from: Reference,
    pub to: Referenceable,
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
            Referenceable::Block { path } => {
                write!(f, "Block: {}", path.display())
            }
        }
    }
}
