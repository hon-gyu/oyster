use super::types::Range;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeBlock {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub language: Option<String>, // The first word of the info string
    pub extra: Option<String>, // The rest of the info string
    pub range: Range,
}

impl std::fmt::Display for CodeBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ser_range = serde_json::to_string(&self.range).unwrap();
        let lang_str = self.language.clone().unwrap_or("None".to_string());
        let extra_str = self.extra.clone().unwrap_or("None".to_string());
        let buf = format!(
            r#"
            CodeBlock
            - language: {lang_str}
            - extra: {extra_str}
            - range: {ser_range}"#
        );
        write!(f, "{}", buf)
    }
}
