//! Render content
//!
//! See `write.rs` for information used
//!
//! We don't pass in the referenceable info as it's inside the ASTLink
use super::codeblock::{
    MermaidRenderMode, QuiverRenderMode, TikzRenderMode, render_mermaid,
    render_quiver, render_tikz,
};
use super::latex::render_latex;
use super::utils;
use super::vault_db::VaultDB;
use crate::ast::{
    Node,
    NodeKind::{self, *},
    Tree,
};
use crate::export::utils::range_to_anchor_id;
use maud::{Markup, PreEscaped, html};
use pulldown_cmark::{BlockQuoteKind, CodeBlockKind, LinkType};
use std::path::Path;

pub struct NodeRenderConfig {
    pub mermaid_render_mode: MermaidRenderMode,
    pub tikz_render_mode: TikzRenderMode,
    pub quiver_render_mode: QuiverRenderMode,
}

/// Render content
///
/// Input:
/// - info about this page
///   - vault path
///   - tree
/// - link info
///   - resolved links
///   - valut path to slug map
///   - referenceable to anchor id map
pub fn render_content(
    tree: &Tree,
    vault_path: &Path,
    vault_db: &dyn VaultDB,
    node_render_config: &NodeRenderConfig,
    embed_depth: usize,     // Current embed depth
    max_embed_depth: usize, // Max embed depth
) -> Markup {
    let rendered = render_node(
        &tree.root_node,
        vault_path,
        vault_db,
        node_render_config,
        embed_depth,
        max_embed_depth,
    );
    rendered
}

pub enum EmbededKind {
    Image,
    Video,
    Audio,
    PDF,
    Note,
    Heading,
    Block,
}

fn render_embedded_content(
    _tgt_tree: &Tree,
    _tgt_vault_path: &Path,
    _vault_db: &dyn VaultDB,
    _node_render_config: &NodeRenderConfig,
    _embed_depth: usize,     // Current embed depth
    _max_embed_depth: usize, // Max embed depth
) -> (Markup, EmbededKind) {
    todo!()
}

fn render_nodes(
    nodes: &[Node],
    vault_path: &Path,
    vault_db: &dyn VaultDB,
    node_render_config: &NodeRenderConfig,
    embed_depth: usize,     // Current embed depth
    max_embed_depth: usize, // Max embed depth
) -> String {
    let mut buffer = String::new();
    for node in nodes {
        let rendered = render_node(
            node,
            vault_path,
            vault_db,
            node_render_config,
            embed_depth,
            max_embed_depth,
        );
        buffer.push_str(&rendered.into_string());
    }

    buffer
}

const IMAGE_EXTENSIONS: [&str; 3] = ["png", "jpg", "jpeg"];

fn render_node(
    node: &Node,
    vault_path: &Path,
    vault_db: &dyn VaultDB,
    node_render_config: &NodeRenderConfig,
    embed_depth: usize,     // Current embed depth
    max_embed_depth: usize, // Max embed depth
) -> Markup {
    let range = node.start_byte..node.end_byte;
    let markup = match &node.kind {
        // Tree root
        Document => {
            let children_rendered = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                article {
                    (PreEscaped(children_rendered))
                }
            }
        }
        // Reference nodes
        Link {
            link_type: LinkType::WikiLink { .. } | LinkType::Inline,
            dest_url,
            title,
            ..
        } => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            let title_opt = if title.is_empty() {
                None
            } else {
                Some(title.as_ref())
            };

            // Find matched reference's resolved destination
            if let Some(dest) =
                vault_db.get_tgt_slug_from_src(vault_path, &range)
            {
                let anchor_markup = html! {
                    a href=(dest) title=[title_opt] {
                        (PreEscaped(children))
                    }
                };
                // Extra internal-link span and anchor id (byte-range) for resolved links
                // TODO: add more link info to attributes
                let id = range_to_anchor_id(&range);
                html! {
                    span .internal-link #(id) {
                        (anchor_markup)
                    }
                }
            } else {
                // Reference is unresolved
                let href = dest_url.to_string();

                let is_abs_url = url::Url::parse(href.as_ref()).is_ok();
                if is_abs_url {
                    html! {
                        a href=(href) title=[title_opt] {
                            (PreEscaped(children))
                        }
                    }
                } else {
                    // Unresolved internal link: use span instead of anchor to make it not clickable
                    html! {
                        span .internal-link.unresolved title=[title_opt] {
                            (PreEscaped(children))
                        }
                    }
                }
            }
        }
        Image {
            link_type: LinkType::WikiLink { .. } | LinkType::Inline,
            dest_url,
            title,
            ..
        } => {
            // Note: this is `![[]]` embed, not necessarily an image
            // Could be a note, a block, and etc.
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );

            let title_opt = if title.is_empty() {
                None
            } else {
                Some(title.as_ref())
            };

            // Find matched tgt destination
            let matched_tgt_slug_path_opt =
                vault_db.get_tgt_slug_from_src(vault_path, &range);

            if let Some(tgt_slug_path) = matched_tgt_slug_path_opt {
                // Case: matched link
                let backlink_anchor_id = range_to_anchor_id(&range);

                // Check if this is an image
                let is_image = Path::new(&tgt_slug_path)
                    .extension()
                    .and_then(|ext| {
                        IMAGE_EXTENSIONS
                            .iter()
                            .find(|&&e| e == ext.to_str().unwrap_or(""))
                    })
                    .is_some();
                if is_image {
                    // Case: image
                    let resize_spec = &children;
                    let (width, height) = utils::parse_resize_spec(resize_spec);
                    let alt_text = Path::new(&tgt_slug_path)
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap_or("");
                    html! {
                        img .embed-file.image src=(tgt_slug_path) alt=(alt_text) #(backlink_anchor_id) width=[width] height=[height] {}
                    }
                } else if embed_depth < max_embed_depth {
                    // let (embedded, embedeed_class) = render_embedded_content(

                    // )
                    todo!()
                } else {
                    // Terminate recursion and render as anchor
                    let anchor_markup = html! {
                        a href=(tgt_slug_path) title=[title_opt] {
                            (PreEscaped(&children))
                        }
                    };
                    html! {
                        span .embed-file #(backlink_anchor_id) {
                            (anchor_markup)
                        }
                    }
                }
            } else {
                // No matched, fallback to raw url
                let href = dest_url.to_string();
                // Just an anchor
                html! {
                    a href=(href) title=[title_opt] {
                        (PreEscaped(children))
                    }
                }
            }
        }
        // Referenceable nodes
        Paragraph => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            // Inject anchor id for matched referenceable
            if let Some(id) = vault_db
                .get_innote_refable_anchor_id(&vault_path.to_path_buf(), &range)
            {
                html! {
                    p #(id) { (PreEscaped(children)) }
                }
            } else {
                html! {
                    p { (PreEscaped(children)) }
                }
            }
        }
        Heading {
            level,
            id: _,
            classes,
            attrs,
        } => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );

            let tag = format!("h{}", *level as usize);

            // id: anchor id from matched referenceable takes precedence
            let id_attr = {
                let id_from_matched_referable = vault_db
                    .get_innote_refable_anchor_id(
                        &vault_path.to_path_buf(),
                        &range,
                    )
                    .expect("Heading should always have anchor id");
                format!(" id=\"{}\"", id_from_matched_referable)
            };

            // Class attribute
            let class_attr = if classes.is_empty() {
                String::new()
            } else {
                let class_str = classes
                    .iter()
                    .map(|c| c.as_ref())
                    .collect::<Vec<_>>()
                    .join(" ");
                format!(" class=\"{}\"", class_str)
            };

            // Other attributes
            let other_attrs = attrs
                .iter()
                .map(|(name, val)| match val {
                    Some(v) => format!(" {}=\"{}\"", name.as_ref(), v.as_ref()),
                    None => format!(" {}=\"\"", name.as_ref()),
                })
                .collect::<String>();

            html! {
                (PreEscaped(format!("<{}{}{}{}>{}</{}>",
                    tag, id_attr, class_attr, other_attrs, children, tag)))
            }
        }
        BlockQuote(kind) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );

            // Extract class name determination
            let class_name = kind.as_ref().map(|bq_kind| match bq_kind {
                BlockQuoteKind::Note => "markdown-alert-note",
                BlockQuoteKind::Tip => "markdown-alert-tip",
                BlockQuoteKind::Important => "markdown-alert-important",
                BlockQuoteKind::Warning => "markdown-alert-warning",
                BlockQuoteKind::Caution => "markdown-alert-caution",
            });

            let id_opt = vault_db.get_innote_refable_anchor_id(
                &vault_path.to_path_buf(),
                &range,
            );

            // Inject anchor id for matched referenceable
            if let Some(id) = id_opt {
                match class_name {
                    Some(class) => html! {
                        blockquote #(id) class=(class) {
                            (PreEscaped(children))
                        }
                    },
                    None => html! {
                        blockquote #(id) {
                            (PreEscaped(children))
                        }
                    },
                }
            } else {
                match class_name {
                    Some(class) => html! {
                        blockquote class=(class) {
                            (PreEscaped(children))
                        }
                    },
                    None => html! {
                        blockquote {
                            (PreEscaped(children))
                        }
                    },
                }
            }
        }
        List(start) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );

            let id_opt = vault_db.get_innote_refable_anchor_id(
                &vault_path.to_path_buf(),
                &range,
            );

            // Extract list type and start attribute determination
            if let Some(id) = id_opt {
                // Inject anchor id for matched referenceable
                match start {
                    // Ordered list starting from 1
                    Some(1) => html! {
                        ol #(id) { (PreEscaped(children)) }
                    },
                    // Ordered list starting from specified number
                    Some(start_num) => html! {
                        ol #(id) start=(start_num) { (PreEscaped(children)) }
                    },
                    // Unordered list
                    None => html! {
                        ul #(id) { (PreEscaped(children)) }
                    },
                }
            } else {
                match start {
                    // Ordered list starting from 1
                    Some(1) => html! {
                        ol { (PreEscaped(children)) }
                    },
                    // Ordered list starting from specified number
                    Some(start_num) => html! {
                        ol start=(start_num) { (PreEscaped(children)) }
                    },
                    // Unordered list
                    None => html! {
                        ul { (PreEscaped(children)) }
                    },
                }
            }
        }
        Item => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            // Inject anchor id for matched referenceable
            if let Some(id) = vault_db
                .get_innote_refable_anchor_id(&vault_path.to_path_buf(), &range)
            {
                html! {
                    li id=(id) { (PreEscaped(children)) }
                }
            } else {
                html! {
                    li { (PreEscaped(children)) }
                }
            }
        }
        // Nodes unrelated to resolved links
        // Non-container nodes (leaf nodes)
        Text(text) => {
            html! {
                (text.as_ref())
            }
        }
        Code(text) => {
            html! {
                code { (text.as_ref()) }
            }
        }
        InlineMath(text) => {
            html! {
                (PreEscaped(render_latex(text.as_ref(), false)))
            }
        }
        DisplayMath(text) => {
            html! {
                (PreEscaped(render_latex(text.as_ref(), true)))
            }
        }
        Html(text) | InlineHtml(text) => {
            html! {
                (PreEscaped(text.as_ref()))
            }
        }
        SoftBreak => {
            html! {
                " "
            }
        }
        HardBreak => {
            html! {
                br;
            }
        }
        Rule => {
            html! {
                hr;
            }
        }
        FootnoteReference(name) => {
            html! {
                sup class="footnote-reference" {
                    a href=(format!("#{}", name)) {
                        (name.as_ref())
                    }
                }
            }
        }
        TaskListMarker(checked) => match checked {
            true => html! {
                input type="checkbox" disabled checked;
            },
            false => html! {
                input type="checkbox" disabled;
            },
        },
        // Container nodes (except for Document)
        CodeBlock(kind) => {
            let code_src = &node
                .children
                .iter()
                .map(|child| match &child.kind {
                    NodeKind::Text(text) => text.as_ref(),
                    _ => panic!("CodeBlock should only contain Text nodes"),
                })
                .collect::<Vec<_>>()
                .join("");
            // Extract language
            let lang = match kind {
                CodeBlockKind::Fenced(info) => {
                    let lang = info.split(' ').next().unwrap();
                    if lang.is_empty() { None } else { Some(lang) }
                }
                CodeBlockKind::Indented => None,
            };

            // Render code block
            match lang {
                Some(lang) if lang == "mermaid" => {
                    let mermaid = render_mermaid(
                        &code_src,
                        node_render_config.mermaid_render_mode,
                    );
                    html! {
                        (mermaid)
                    }
                }
                Some(lang) if lang == "tikz" => {
                    let tikz = render_tikz(
                        &code_src,
                        node_render_config.tikz_render_mode,
                    );
                    html! {
                        (tikz)
                    }
                }
                Some(lang) if lang == "quiver" => {
                    let quiver = render_quiver(
                        &code_src,
                        node_render_config.quiver_render_mode,
                    );
                    html! {
                        (quiver)
                    }
                }
                _ => {
                    let language_class =
                        lang.map(|lang| format!("language-{}", lang));
                    html! {
                        pre {
                            code class=[language_class] { (PreEscaped(code_src)) }
                        }
                    }
                }
            }
        }
        HtmlBlock => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                (PreEscaped(children))
            }
        }
        Emphasis => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                em { (PreEscaped(children)) }
            }
        }
        Strong => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                strong { (PreEscaped(children)) }
            }
        }
        Strikethrough => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                del { (PreEscaped(children)) }
            }
        }
        Superscript => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                sup { (PreEscaped(children)) }
            }
        }
        Subscript => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                sub { (PreEscaped(children)) }
            }
        }
        Link {
            link_type: LinkType::Email,
            dest_url,
            title,
            ..
        } => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            let href = format!("mailto:{}", dest_url.as_ref());
            let title_opt = if title.is_empty() {
                None
            } else {
                Some(title.as_ref())
            };
            html! {
                a href=(href) title=[title_opt] {
                    (PreEscaped(children))
                }
            }
        }
        Link {
            link_type: _,
            dest_url,
            title,
            ..
        } => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            let href = dest_url.to_string();

            let title_opt = if title.is_empty() {
                None
            } else {
                Some(title.as_ref())
            };
            html! {
                a href=(href) title=[title_opt] {
                    (PreEscaped(children))
                }
            }
        }
        // Image with link type other than wikilink and inline
        Image {
            dest_url, title, ..
        } => {
            // For images, children contain the alt text
            let alt_text = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            let title_attr = if title.is_empty() {
                String::new()
            } else {
                format!(" title=\"{}\"", title.as_ref())
            };
            html! {
                (PreEscaped(format!("<img src=\"{}\" alt=\"{}\"{}/>", dest_url.as_ref(), alt_text, title_attr)))
            }
        }
        Table(_alignments) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                table { (PreEscaped(children)) }
            }
        }
        TableHead => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                thead { (PreEscaped(children)) }
            }
        }
        TableRow => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                tr { (PreEscaped(children)) }
            }
        }
        TableCell => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            // TODO: handle table alignment based on cell index
            html! {
                td { (PreEscaped(children)) }
            }
        }
        DefinitionList => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                dl { (PreEscaped(children)) }
            }
        }
        DefinitionListTitle => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                dt { (PreEscaped(children)) }
            }
        }
        DefinitionListDefinition => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                dd { (PreEscaped(children)) }
            }
        }
        FootnoteDefinition(name) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );
            html! {
                div class="footnote-definition" id=(name.as_ref()) {
                    sup class="footnote-definition-label" { (name.as_ref()) }
                    (PreEscaped(children))
                }
            }
        }
        MetadataBlock(_) => {
            // Metadata blocks should not be rendered
            html! {}
        }
    };

    markup
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::export::frontmatter;
    use crate::export::vault_db::{
        FileLevelInfo, StaticVaultStore, VaultLevelInfo,
    };
    use crate::link::scan_vault;
    use insta::*;
    use maud::DOCTYPE;
    use serde_yaml::Value;
    use std::collections::HashMap;
    use std::fs;
    use std::path::{Path, PathBuf};

    fn format_html_simple(html: &str) -> String {
        html.replace("><", ">\n<")
            .replace("<h", "\n<h")
            .replace("<p", "\n<p")
            .replace("</article>", "\n</article>")
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

    /// Helper to write HTML to a file for visual inspection
    fn write_html_preview(html: &str, filename: &str) {
        let output_dir = PathBuf::from("target/test_output");
        fs::create_dir_all(&output_dir).unwrap();
        let output_path = output_dir.join(filename);

        fs::write(&output_path, html).unwrap();
        println!("HTML preview written to: {}", output_path.display());
    }

    #[test]
    fn test_render_note() {
        let vault_root_dir = Path::new("tests/data/vaults/minimal");

        // Scan the vault
        let (fronmatters_vec, referenceables, references) =
            scan_vault(vault_root_dir, vault_root_dir, false);

        // Build frontmatters map
        let fronmatters = referenceables
            .iter()
            .zip(fronmatters_vec)
            .map(|(referenceable, fm)| (referenceable.path().to_path_buf(), fm))
            .collect();

        // Build file and vault level info
        let file_level_info = FileLevelInfo {
            referenceables,
            references,
            fronmatters,
        };
        let vault_level_info = VaultLevelInfo::new(
            &file_level_info.referenceables,
            &file_level_info.references,
            &file_level_info.fronmatters,
        );

        // Create vault DB
        let vault_db = StaticVaultStore::new(file_level_info, vault_level_info);

        // Render note
        let note_path = Path::new("Note 1.md");
        let md_src =
            fs::read_to_string(vault_root_dir.join(note_path)).unwrap();
        let tree = Tree::new(&md_src);
        let node_render_config = NodeRenderConfig {
            mermaid_render_mode: MermaidRenderMode::from_str("client-side")
                .unwrap(),
            tikz_render_mode: TikzRenderMode::from_str("client-side").unwrap(),
            quiver_render_mode: QuiverRenderMode::from_str("raw").unwrap(),
        };
        let rendered = render_content(
            &tree,
            note_path,
            &vault_db,
            &node_render_config,
            0,
            5,
        );

        let rendered = format_html_simple(&rendered.into_string());
        let full_html = render_full_html(&rendered);

        // Write HTML to file for visual inspection
        write_html_preview(&full_html, "test_render_note.html");

        assert_snapshot!(full_html, @r#"
        <!DOCTYPE html><body><article>

        <p>This is the first note with various reference types.</p>

        <p>For more details, see <span class="internal-link" id="76-95">
        <a href="note-1.html#additional-info">#Additional Info</a>
        </span> below.</p>

        <p>You can also reference this specific point: <span class="internal-link" id="149-163">
        <a href="note-1.html#key-point">#^key-point</a>
        </span>
        </p>

        <h2 id="direct-note-reference">Direct Note Reference</h2>

        <p>Check out <span class="internal-link" id="202-211">
        <a href="note-2.html">Note 2</a>
        </span> for more information.</p>

        <h2 id="heading-reference">Heading Reference</h2>

        <p>See the section on <span class="internal-link" id="277-302">
        <a href="note-2.html#getting-started">Note 2#Getting Started</a>
        </span> for details.</p>

        <h2 id="block-reference">Block Reference</h2>

        <p>Here’s a reference to a specific block: <span class="internal-link" id="378-404">
        <a href="note-2.html#important-block">Note 2#^important-block</a>
        </span>
        </p>

        <h2 id="image-embed">Image Embed</h2>

        <p>
        <img class="embed-file image" id="423-441" src="blue-image.png" alt="blue-image">
        </img>
        </p>

        <h2 id="additional-info">Additional Info</h2>

        <p>This section is referenced from the top of this note using a same-note heading reference.</p>

        <p id="key-point">This is a key point within the same note. ^key-point</p>

        <p>The line above has a block ID that’s referenced from earlier in this note.</p>

        </article></body>
        "#);
    }
}
