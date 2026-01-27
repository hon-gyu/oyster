use super::types::Range;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeBlock {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<String>,
    pub range: Range,
}
