//! Tests here can be fearlessly updated
#![allow(dead_code, unused_imports)]
use super::codeblock::{
    MermaidRenderMode, QuiverRenderMode, TikzRenderMode, render_mermaid,
    render_quiver, render_tikz,
};
use super::content::{NodeRenderConfig, render_content};
use super::latex::render_latex;
use super::utils;
use super::vault_db::VaultDB;
use crate::ast::{
    Node,
    NodeKind::{self, *},
    Tree,
};
use crate::export::utils::range_to_anchor_id;
use crate::export::vault_db::{
    FileLevelInfo, StaticVaultStore, VaultLevelInfo,
};
use crate::link::Referenceable;
use crate::link::scan_vault;
use insta::*;
use maud::DOCTYPE;
use maud::{Markup, PreEscaped, html};
use pulldown_cmark::{CodeBlockKind, LinkType};
use std::path::{Path, PathBuf};

use std::fs;

fn format_html_simple(html: &str) -> String {
    html.replace("><", ">\n<")
        .replace("<h", "\n<h")
        .replace("<p", "\n<p")
        .replace("</article>", "\n</article>")
}

fn prettify_html(html: &str) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};

    let mut child = Command::new("prettier")
        .arg("--parser")
        .arg("html")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("prettier not found");

    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(html.as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    String::from_utf8(output.stdout).unwrap()
}

fn render_full_html(content: &str) -> String {
    html! {
        (DOCTYPE)
        body {
            (PreEscaped(content))
        }
    }
    .into_string()
}

fn render_single_page(note_vault_path: &Path) -> String {
    use tempfile::tempdir;
    let temp_dir = tempdir().unwrap();
    let temp_dir_path = temp_dir.path();

    // Copy note to temp dir
    let temp_note_path =
        temp_dir_path.join(note_vault_path.file_name().unwrap());
    fs::copy(note_vault_path, &temp_note_path).unwrap();

    let vault_db = StaticVaultStore::new_from_dir(temp_dir_path, false);
    let node_render_config = NodeRenderConfig::default();

    let md_src = fs::read_to_string(&temp_note_path).unwrap();
    let tree = Tree::new_with_default_opts(&md_src);
    let markup = render_content(
        &tree,
        temp_dir_path,
        &vault_db,
        &node_render_config,
        0,
        1,
    );

    prettify_html(&markup.into_string())
}

#[test]
fn parse_softbreak() {
    let path = std::path::PathBuf::from("tests/data/notes/softbreak.md");
    let md_src = std::fs::read_to_string(&path).unwrap();
    let tree = Tree::new_with_default_opts(&md_src);
    assert_snapshot!(&tree.root_node, @r#"
    Document [0..76]
      Paragraph [0..15]
        Text(Borrowed("something")) [0..9]
        SoftBreak [9..10]
        Text(Borrowed("else")) [10..14]
      Paragraph [16..33]
        Text(Borrowed("something")) [16..25]
        HardBreak [25..28]
        Text(Borrowed("else")) [28..32]
      BlockQuote [35..54]
        Paragraph [37..54]
          Text(Borrowed("something")) [37..46]
          SoftBreak [46..47]
          Text(Borrowed("else")) [49..53]
      BlockQuote [56..76]
        Paragraph [58..76]
          Text(Borrowed("something")) [58..67]
          HardBreak [67..70]
          Text(Borrowed("else")) [72..76]
    "#);
}

#[test]
fn parse_callout() {
    let path = std::path::PathBuf::from("tests/data/notes/callout.md");
    let md_src = std::fs::read_to_string(&path).unwrap();
    let tree = Tree::new_with_default_opts(&md_src);
    assert_snapshot!(&tree.root_node, @r#"
    Document [0..477]
      BlockQuote [0..17]
        Paragraph [2..17]
          Text(Borrowed("This is a note")) [2..16]
      Callout [18..48]
        CalloutDeclaraion { kind: GFM(Tip), title: None, foldable: None } [20..27]
          Paragraph [20..27]
            Text(Borrowed("[")) [20..21]
            Text(Borrowed("!Tip")) [21..25]
            Text(Borrowed("]")) [25..26]
            SoftBreak [26..27]
        CalloutContent [29..48]
          Paragraph [29..48]
            Text(Borrowed("tip with one space")) [29..47]
      Callout [50..80]
        CalloutDeclaraion { kind: GFM(Tip), title: None, foldable: None } [51..58]
          Paragraph [51..58]
            Text(Borrowed("[")) [51..52]
            Text(Borrowed("!Tip")) [52..56]
            Text(Borrowed("]")) [56..57]
            SoftBreak [57..58]
        CalloutContent [60..80]
          Paragraph [60..80]
            Text(Borrowed("tip with zero space")) [60..79]
      BlockQuote [82..145]
        Paragraph [85..145]
          Text(Borrowed("[")) [85..86]
          Text(Borrowed("!Tip")) [86..90]
          Text(Borrowed("]")) [90..91]
          SoftBreak [91..92]
          Text(Borrowed("tip with 2 space -> not a callout, just blockquote")) [94..144]
      Callout [147..189]
        CalloutDeclaraion { kind: GFM(Tip), title: Some("title"), foldable: None } [149..162]
          Paragraph [149..162]
            Text(Borrowed("[")) [149..150]
            Text(Borrowed("!Tip")) [150..154]
            Text(Borrowed("]")) [154..155]
            Text(Borrowed(" title")) [155..161]
            SoftBreak [161..162]
        CalloutContent [164..189]
          Paragraph [164..189]
            Text(Borrowed("This is a tip with title")) [164..188]
      Callout [191..206]
        CalloutDeclaraion { kind: GFM(Tip), title: Some("title"), foldable: None } [193..206]
          Paragraph [193..206]
            Text(Borrowed("[")) [193..194]
            Text(Borrowed("!Tip")) [194..198]
            Text(Borrowed("]")) [198..199]
            Text(Borrowed(" title")) [199..205]
      BlockQuote [207..240]
        Paragraph [209..240]
          Text(Borrowed("This is a separate block quote")) [209..239]
      Callout [241..274]
        CalloutDeclaraion { kind: GFM(Warning), title: None, foldable: None } [243..254]
          Paragraph [243..254]
            Text(Borrowed("[")) [243..244]
            Text(Borrowed("!WARNING")) [244..252]
            Text(Borrowed("]")) [252..253]
            SoftBreak [253..254]
        CalloutContent [256..274]
          Paragraph [256..274]
            Text(Borrowed("This is a warning")) [256..273]
      Callout [275..311]
        CalloutDeclaraion { kind: Custom(Llm), title: None, foldable: None } [277..284]
          Paragraph [277..284]
            Text(Borrowed("[")) [277..278]
            Text(Borrowed("!LLM")) [278..282]
            Text(Borrowed("]")) [282..283]
            SoftBreak [283..284]
        CalloutContent [286..311]
          Paragraph [286..311]
            Text(Borrowed("This is generated by LLM")) [286..310]
      Callout [313..442]
        CalloutDeclaraion { kind: Obsidian(Question), title: Some("Can callouts be nested?"), foldable: None } [315..351]
          Paragraph [315..351]
            Text(Borrowed("[")) [315..316]
            Text(Borrowed("!question")) [316..325]
            Text(Borrowed("]")) [325..326]
            Text(Borrowed(" Can callouts be nested?")) [326..350]
        CalloutContent [353..442]
          Callout [353..442]
            CalloutDeclaraion { kind: Obsidian(Todo), title: Some("Yes!, they can."), foldable: None } [355..379]
              Paragraph [355..379]
                Text(Borrowed("[")) [355..356]
                Text(Borrowed("!todo")) [356..361]
                Text(Borrowed("]")) [361..362]
                Text(Borrowed(" Yes!, they can.")) [362..378]
            CalloutContent [383..442]
              Callout [383..442]
                CalloutDeclaraion { kind: Obsidian(Example), title: Some("You can even use multiple layers of nesting."), foldable: None } [385..442]
                  Paragraph [385..442]
                    Text(Borrowed("[")) [385..386]
                    Text(Borrowed("!example")) [386..394]
                    Text(Borrowed("]")) [394..395]
                    Text(Borrowed("  You can even use multiple layers of nesting.")) [395..441]
      Callout [443..477]
        CalloutDeclaraion { kind: GFM(Note), title: None, foldable: None } [445..453]
          Paragraph [445..453]
            Text(Borrowed("[")) [445..446]
            Text(Borrowed("!NOTE")) [446..451]
            Text(Borrowed("]")) [451..452]
        CalloutContent [455..477]
          CodeBlock(Fenced(Borrowed("python"))) [455..477]
            Text(Borrowed("code\n")) [467..472]
    "#);
}

#[test]
fn render_callout() {
    let path = std::path::PathBuf::from("tests/data/notes/callout.md");
    let out = render_single_page(&path);
    assert_snapshot!(out, @r#"
    <article>
      <blockquote><p>This is a note</p></blockquote>
      <callout
        ><callout-declaration callout-kind="tip"
          ><span class="callout-icon"></span
          ><span class="callout-title">Tip</span></callout-declaration
        ><callout><p>tip with one space</p></callout></callout
      ><callout
        ><callout-declaration callout-kind="tip"
          ><span class="callout-icon"></span
          ><span class="callout-title">Tip</span></callout-declaration
        ><callout><p>tip with zero space</p></callout></callout
      >
      <blockquote>
        <p>[!Tip]<br />tip with 2 space -&gt; not a callout, just blockquote</p>
      </blockquote>
      <callout
        ><callout-declaration callout-kind="tip"
          ><span class="callout-icon"></span
          ><span class="callout-title">title</span></callout-declaration
        ><callout><p>This is a tip with title</p></callout></callout
      ><callout
        ><callout-declaration callout-kind="tip"
          ><span class="callout-icon"></span
          ><span class="callout-title">title</span></callout-declaration
        ></callout
      >
      <blockquote><p>This is a separate block quote</p></blockquote>
      <callout
        ><callout-declaration callout-kind="warning"
          ><span class="callout-icon"></span
          ><span class="callout-title">Warning</span></callout-declaration
        ><callout><p>This is a warning</p></callout></callout
      ><callout
        ><callout-declaration callout-kind="llm"
          ><span class="callout-icon"></span
          ><span class="callout-title">LLM</span></callout-declaration
        ><callout><p>This is generated by LLM</p></callout></callout
      ><callout
        ><callout-declaration callout-kind="question"
          ><span class="callout-icon"></span
          ><span class="callout-title"
            >Can callouts be nested?</span
          ></callout-declaration
        ><callout
          ><callout
            ><callout-declaration callout-kind="todo"
              ><span class="callout-icon"></span
              ><span class="callout-title"
                >Yes!, they can.</span
              ></callout-declaration
            ><callout
              ><callout
                ><callout-declaration callout-kind="example"
                  ><span class="callout-icon"></span
                  ><span class="callout-title"
                    >You can even use multiple layers of nesting.</span
                  ></callout-declaration
                ></callout
              ></callout
            ></callout
          ></callout
        ></callout
      ><callout
        ><callout-declaration callout-kind="note"
          ><span class="callout-icon"></span
          ><span class="callout-title">Note</span></callout-declaration
        ><callout>
          <pre><code class="language-python">code
    </code></pre>
        </callout></callout
      >
    </article>
    "#);
}
