use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub enum Value {
    Null,
    // A paragraph of text.
    String(String),
    // A list of strings.
    A(Vec<String>),
    // A list of key-value pairs.
    O(Vec<(String, Value)>),
}
