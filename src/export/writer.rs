use super::content::render_content;
use super::utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
};
use crate::ast::Tree;
use crate::link::{Referenceable, build_links, scan_vault};
use maud::{DOCTYPE, PreEscaped, html};
use std::fs;
use std::path::Path;

pub fn render_vault(
    vault_root_dir: &Path,
    output_dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    // Scan the vault and build links
    let (referenceables, references) =
        scan_vault(vault_root_dir, vault_root_dir);
    let (links, _unresolved) = build_links(references, referenceables.clone());

    // Build vault file path to slug map
    let vault_file_paths = referenceables
        .iter()
        .filter(|referenceable| !referenceable.is_innote())
        .map(|referenceable| referenceable.path().as_path())
        .collect::<Vec<_>>();
    let vault_path_to_slug_map =
        build_vault_paths_to_slug_map(&vault_file_paths);

    // Build in-note anchor id map
    let referenceable_refs = referenceables.iter().collect::<Vec<_>>();
    let innote_refable_anchor_id_map =
        build_in_note_anchor_id_map(&referenceable_refs);

    fs::create_dir_all(output_dir)?;
    let note_vault_paths = referenceables
        .iter()
        .filter(|referenceable| {
            matches!(referenceable, Referenceable::Note { .. })
        })
        .map(|referenceable| referenceable.path().as_path())
        .collect::<Vec<_>>();
    for note_vault_path in note_vault_paths {
        let md_src = fs::read_to_string(vault_root_dir.join(note_vault_path))?;
        let tree = Tree::new(&md_src);
        let title = title_from_path(note_vault_path);
        let content = render_content(
            &tree,
            note_vault_path,
            &links,
            &vault_path_to_slug_map,
            &innote_refable_anchor_id_map,
        );

        let html = render_page(&title, &content);
        let note_slug_path =
            vault_path_to_slug_map.get(note_vault_path).unwrap();
        let output_path = output_dir.join(format!("{}.html", note_slug_path));

        if let Some(parent) = output_path.parent() {
            let _ = fs::create_dir_all(parent);
        }

        fs::write(&output_path, html)?;
    }
    Ok(())
}

fn title_from_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap()
        .to_string()
}

fn render_page(title: &str, content: &str) -> String {
    html! {
        (DOCTYPE)
        header {
            title { (title) }
        }
        body {
            (PreEscaped(content))
        }
    }
    .into_string()
}
