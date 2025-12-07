mod test_utils;

use insta::*;
use markdown_tools::ast::Tree;
use markdown_tools::export::content;
use markdown_tools::export::vault_db::StaticVaultStore;
use markdown_tools::export::{
    MermaidRenderMode, NodeRenderConfig, QuiverRenderMode, TikzRenderMode,
};
use std::fs;
use std::path::Path;
use test_utils::{format_html_simple, render_full_html};

#[test]
fn test_render_image_resize() {
    let vault_root_dir = Path::new("tests/data/vaults/embed_file");
    let vault_db = StaticVaultStore::new_from_dir(vault_root_dir, false);

    // Render note
    let note_path = Path::new("note.md");
    let md_src = fs::read_to_string(vault_root_dir.join(note_path)).unwrap();
    let tree = Tree::new(&md_src);
    let node_render_config = NodeRenderConfig {
        mermaid_render_mode: MermaidRenderMode::from_str("client-side")
            .unwrap(),
        tikz_render_mode: TikzRenderMode::from_str("client-side").unwrap(),
        quiver_render_mode: QuiverRenderMode::from_str("raw").unwrap(),
    };
    let rendered = content::render_content(
        &tree,
        note_path,
        &vault_db,
        &node_render_config,
        0,
        1,
    );

    let rendered = format_html_simple(&rendered.into_string());
    let full_html = render_full_html(&rendered);

    assert_snapshot!(full_html, @r#"
    <!DOCTYPE html><body><article>

    <p>Image <span class="internal-link" id="7-24">
    <a href="blue-image.png">blue-image.png</a>
    </span>
    </p>

    <p>Embeded Image <img class="embed-file image" id="42-60" embed-depth="0" src="blue-image.png" alt="blue-image">
    </img>
    </p>

    <p>Scale width according to original aspect ratio <img class="embed-file image" id="111-135" embed-depth="0" src="blue-image.png" alt="blue-image" width="200">
    </img>
    </p>

    <p>Resize with width and height <img class="embed-file image" id="167-195" embed-depth="0" src="blue-image.png" alt="blue-image" width="100" height="150">
    </img>
    </p>

    <p>Embed note: <code>![[note2]]</code> <div class="embed-file note" embed-depth="1">
    <div class="header">

    <p class="embed-file header">📑 note2</p>
    </div>
    <div class="content">
    <article>

    <p>Something</p>
    <ul id="60a916">
    <li id="60a916">A list ^60a916</li>
    </ul>

    <p id="7e162c">a paragraph ^7e162c</p>

    <p>A callout</p>
    <callout id="warning" data-callout="warning">
    <callout-title>
    <callout-icon>
    </callout-icon>
    <callout-title-text>Warning</callout-title-text>
    </callout-title>
    <callout-content>

    <p>[!Warning] some warning</p>
    </callout-content>
    </callout>

    <p>^warning</p>

    <h2 id="heading-in-note-2">Heading in note 2</h2>

    <p>something?</p>

    </article>
    </div>
    </div>
    </p>

    <p>Embed heading: <code>![[note2#Heading in note 2]]</code> <div class="embed-file heading" embed-depth="1">
    <div class="header">

    <p class="embed-file header">📑 note2</p>
    </div>
    <div class="content">

    <h2 id="heading-in-note-2">Heading in note 2</h2>
    </div>
    </div>
    </p>

    <p>Embed block - a list: <code>![[note2#^60a916]]</code> <div class="embed-file block" embed-depth="1">
    <div class="header">

    <p class="embed-file header">📑 note2</p>
    </div>
    <div class="content">
    <ul id="60a916">
    <li id="60a916">A list ^60a916</li>
    </ul>
    </div>
    </div>
    </p>

    <p>Embed block - a paragraph : <code>![[note2#^7e162c]]</code> <div class="embed-file block" embed-depth="1">
    <div class="header">

    <p class="embed-file header">📑 note2</p>
    </div>
    <div class="content">

    <p id="7e162c">a paragraph ^7e162c</p>
    </div>
    </div>
    </p>

    <p>Embed block - callout: <code>![[note2#^warning]]</code> <div class="embed-file block" embed-depth="1">
    <div class="header">

    <p class="embed-file header">📑 note2</p>
    </div>
    <div class="content">
    <callout id="warning" data-callout="warning">
    <callout-title>
    <callout-icon>
    </callout-icon>
    <callout-title-text>Warning</callout-title-text>
    </callout-title>
    <callout-content>

    <p>[!Warning] some warning</p>
    </callout-content>
    </callout>
    </div>
    </div>
    </p>

    </article></body>
    "#);
}
