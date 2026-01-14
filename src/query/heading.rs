use crate::hierarchy::Hierarchical;
use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Heading {
    pub level: HeadingLevel,
    pub text: String,
    pub start_byte: usize,
    pub end_byte: usize,
    /// column and row
    pub start_point: (usize, usize),
    /// column and row
    pub end_point: (usize, usize),
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub classes: Vec<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub attrs: Vec<(String, Option<String>)>,
}

impl Hierarchical for Heading {
    fn level(&self) -> usize {
        self.level as usize
    }
}
