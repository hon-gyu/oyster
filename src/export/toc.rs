use crate::heading::{Hierarchical, TreeNode, build_tree};
use crate::link::Referenceable;
use maud::{Markup, html};
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Table of contents item
#[derive(Debug)]
struct TocItem {
    text: String,
    anchor_id: String,
    level: usize,
}

impl TocItem {
    fn new(text: String, anchor_id: String, level: usize) -> Self {
        Self {
            text,
            anchor_id,
            level,
        }
    }
}

impl Hierarchical for TocItem {
    fn level(&self) -> usize {
        self.level
    }
}

/// Recursively render a TOC tree node and its children as markup
fn render_toc_node(node: &TreeNode<TocItem>) -> Markup {
    html! {
        li {
            a href=(format!("#{}", node.value.anchor_id)) { (node.value.text) }
            @if !node.children.is_empty() {
                ul {
                    @for child in &node.children {
                        (render_toc_node(child))
                    }
                }
            }
        }
    }
}

/// Render the table of contents.
///
/// For each note, we need:
///   - heading texts
///   - heading anchor id
pub fn render_toc(
    vault_path: &Path,
    referenceables: &[Referenceable],
    innote_refable_anchor_id_map: &HashMap<
        PathBuf,
        HashMap<Range<usize>, String>,
    >,
) -> Option<Markup> {
    let refable_anchor_id_map = innote_refable_anchor_id_map.get(vault_path)?;

    let this_referenceables = referenceables
        .iter()
        .filter(|refable| refable.path() == vault_path)
        .collect::<Vec<_>>();

    fn get_heading(
        referenceable: &Referenceable,
        toc_items: &mut Vec<TocItem>,
        refable_anchor_id_map: &HashMap<Range<usize>, String>,
    ) {
        match referenceable {
            Referenceable::Heading {
                path: _,
                level,
                text,
                range,
            } => {
                let anchor_id = refable_anchor_id_map.get(range).unwrap();
                let toc_item = TocItem::new(
                    text.to_string(),
                    anchor_id.clone(),
                    *level as usize,
                );
                toc_items.push(toc_item);
            }
            Referenceable::Note { children, .. } => {
                for child in children {
                    get_heading(child, toc_items, refable_anchor_id_map);
                }
            }
            _ => {}
        }
    }

    let mut toc_items = Vec::new();
    for referenceable in this_referenceables {
        get_heading(referenceable, &mut toc_items, refable_anchor_id_map);
    }

    if toc_items.is_empty() {
        return None;
    }

    let tree = build_tree(toc_items);

    Some(html! {
        nav class="toc" {
            details open {
                summary { "Table of Contents" }
                ul {
                    @for root in tree {
                        (render_toc_node(&root))
                    }
                }
            }
        }
    })
}
