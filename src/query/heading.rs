use pulldown_cmark::HeadingLevel;
use serde::{Deserialize, Serialize};
use tree_sitter::Point;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Heading {
    pub level: HeadingLevel,
    pub text: String,
    pub start_byte: usize,
    pub end_byte: usize,
    pub start_point: (usize, usize),
    pub end_point: (usize, usize),
    pub id: Option<String>,
    pub classes: Vec<String>,
    pub attrs: Vec<(String, Option<String>)>,
}
