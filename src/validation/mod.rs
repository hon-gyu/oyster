#[allow(dead_code)] // TODO: remove
mod nb_local;
use crate::value::Value;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct ChangelogItem {
    version: String,
    date: Option<String>,
    content: Value,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Changelog {
    header: String,
    items: Vec<ChangelogItem>,
}

fn parse_standard_changelog(text: &str) -> Changelog {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Tree;
    use insta::{assert_debug_snapshot, assert_snapshot};
    use std::fs;
}
