use super::utils::text_to_anchor_id;
/// Extracts references and referenceables from a Markdown AST.
/// Referenceable can be
///   - items in a note: headings, block
///   - notes: markdown files
///   - assets other than notes: images, videos, audios, PDFs, etc.
use pulldown_cmark::HeadingLevel;
use std::ops::Range;
use std::path::PathBuf;

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
pub enum InNoteReferenceable {
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

impl InNoteReferenceable {
    fn get_anchor_id(&self) -> String {
        match self {
            Self::Heading { text, .. } => text_to_anchor_id(text),
            Self::Block { identifier, .. } => identifier.clone(),
        }
    }
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
    InNote(InNoteReferenceable),
    // Heading {
    //     path: PathBuf,
    //     level: HeadingLevel,
    //     text: String,
    //     // The exact range of the heading event from start to end
    //     range: Range<usize>,
    // },
    // Block {
    //     path: PathBuf,
    //     identifier: String,
    //     kind: BlockReferenceableKind,
    //     // The exact range of the event, including
    //     // - paragraph
    //     // - list item
    //     // - block quote
    //     // - table
    //     // - list
    //     range: Range<usize>,
    // },
}

impl Referenceable {
    pub fn path(&self) -> &PathBuf {
        match self {
            Self::Asset { path, .. } | Self::Note { path, .. } => path,
            Self::InNote(
                InNoteReferenceable::Heading { path, .. }
                | InNoteReferenceable::Block { path, .. },
            ) => path,
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
    /// The `dest_url` of the raw link in markdown file
    /// percent-decoded if it's a inline link (markdown link)
    pub dest: String,
    pub kind: ReferenceKind,
    pub display_text: String,
}

impl std::fmt::Display for Referenceable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Asset { path } => {
                write!(f, "Asset: {}", path.display())
            }
            Self::Note { path, .. } => {
                write!(f, "Note: {}", path.display())
            }
            Self::InNote(InNoteReferenceable::Heading {
                path,
                level,
                text,
                ..
            }) => {
                write!(
                    f,
                    "Heading: {} level: {}, text: {}",
                    path.display(),
                    level,
                    text
                )
            }
            Self::InNote(InNoteReferenceable::Block {
                path,
                identifier,
                kind,
                range,
            }) => {
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

#[derive(Clone, Debug, PartialEq)]
pub struct Link {
    pub from: Reference,
    pub to: Referenceable,
}

impl Link {
    fn tgt_range(&self) -> Range<usize> {
        match &self.to {
            Referenceable::InNote(InNoteReferenceable::Heading {
                range,
                ..
            }) => range.clone(),
            Referenceable::InNote(InNoteReferenceable::Block {
                range, ..
            }) => range.clone(),
            _ => panic!(
                "Invalid arguments: No target range for non-in-note referenceable. Only heading and block are valid."
            ),
        }
    }
}
