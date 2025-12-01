use super::codeblock::{MermaidRenderMode, render_mermaid};
use super::latex::render_latex;
use super::utils;
use crate::ast::{Node, NodeKind::*, Tree};
use crate::export::utils::{get_relative_dest, range_to_anchor_id};
use crate::link::types::{Link as ResolvedLink, Referenceable};
use maud::{PreEscaped, html};
use pulldown_cmark::{BlockQuoteKind, CodeBlockKind, LinkType};
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub struct NodeRenderConfig {
    mermaid_render_mode: MermaidRenderMode,
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
    resolved_links: &[ResolvedLink],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
    innote_refable_anchor_id_map: &HashMap<
        PathBuf,
        HashMap<Range<usize>, String>,
    >,
    node_render_config: &NodeRenderConfig,
) -> String {
    // Outgoing links
    // build a map of:
    //   src (this) reference's byte range
    //   |->
    //   tgt_slug.html#anchor_id | tgt_slug.html | tgt_slug.png
    let ref_dest_map: HashMap<Range<usize>, String> = resolved_links
        .iter()
        .filter(|link| link.src_path_eq(vault_path))
        .map(|link| {
            let src_range = &link.from.range;
            let tgt = &link.to;
            let tgt_slug = vault_path_to_slug_map
                .get(tgt.path())
                .expect("link target path not found");
            let base_slug = vault_path_to_slug_map
                .get(vault_path)
                .expect("vault path not found");
            let rel_tgt_slug =
                get_relative_dest(Path::new(base_slug), Path::new(tgt_slug));
            let tgt_anchor_id = match tgt {
                Referenceable::Block {
                    path,
                    range: tgt_range,
                    ..
                }
                | Referenceable::Heading {
                    path,
                    range: tgt_range,
                    ..
                } => innote_refable_anchor_id_map
                    .get(path)
                    .and_then(|anchor_id_map| anchor_id_map.get(tgt_range)),
                _ => None,
            };
            let dest = if let Some(tgt_anchor_id) = tgt_anchor_id {
                format!("{}#{}", rel_tgt_slug, tgt_anchor_id.clone())
            } else {
                format!("{}", rel_tgt_slug)
            };
            (src_range.clone(), dest)
        })
        .collect();

    // Incoming links
    // obtain a map of: tgt (this) referable's byte range |-> anchor id
    let in_note_anchor_id_map: &HashMap<Range<usize>, String> =
        innote_refable_anchor_id_map
            .get(vault_path)
            .expect("vault path not found");

    let rendered = render_node(
        &tree.root_node,
        vault_path,
        &ref_dest_map,
        in_note_anchor_id_map,
        node_render_config,
    );
    rendered
}

fn render_nodes(
    nodes: &[Node],
    vault_path: &Path,
    ref_map: &HashMap<Range<usize>, String>,
    refable_anchor_id_map: &HashMap<Range<usize>, String>,
    node_render_config: &NodeRenderConfig,
) -> String {
    let mut buffer = String::new();
    for node in nodes {
        let rendered = render_node(
            node,
            vault_path,
            ref_map,
            refable_anchor_id_map,
            node_render_config,
        );
        buffer.push_str(rendered.as_str());
    }

    buffer
}

const IMAGE_EXTENSIONS: [&str; 3] = ["png", "jpg", "jpeg"];

fn render_node(
    node: &Node,
    vault_path: &Path,
    // TODO: maybe we should include original vault path as data-href as well
    ref_map: &HashMap<Range<usize>, String>,
    refable_anchor_id_map: &HashMap<Range<usize>, String>,
    node_render_config: &NodeRenderConfig,
) -> String {
    let range = node.start_byte..node.end_byte;
    let markup = match &node.kind {
        // Tree root
        Document => {
            let children_rendered = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            let href = dest_url.to_string();
            // Find matched reference's resolved destination
            let matched_reference_dest = ref_map.get(&range);
            let href = if let Some(dest) = matched_reference_dest {
                dest.clone()
            } else {
                href
            };

            let title_opt = if title.is_empty() {
                None
            } else {
                Some(title.as_ref())
            };

            // Extra internal-link span and anchor id (byte-range) for resolved links
            if matched_reference_dest.is_some() {
                let anchor_markup = html! {
                    a href=(href) title=[title_opt] {
                        (PreEscaped(children))
                    }
                };
                let id = range_to_anchor_id(&range);
                html! {
                    span .internal-link #(id) {
                        (anchor_markup)
                    }
                }
            } else {
                // Reference is unresolved
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );

            let title_opt = if title.is_empty() {
                None
            } else {
                Some(title.as_ref())
            };

            // Find matched tgt destination
            let matched_tgt_slug_path_opt = ref_map.get(&range);

            if let Some(tgt_slug_path) = matched_tgt_slug_path_opt {
                let tgt_slug = tgt_slug_path.clone();
                let anchor_id = range_to_anchor_id(&range);
                let anchor_markup = html! {
                    a href=(tgt_slug) title=[title_opt] {
                        (PreEscaped(&children))
                    }
                };

                // Check if this is an image
                if Path::new(&tgt_slug)
                    .extension()
                    .and_then(|ext| {
                        IMAGE_EXTENSIONS
                            .iter()
                            .find(|&&e| e == ext.to_str().unwrap_or(""))
                    })
                    .is_some()
                {
                    let resize_spec = &children;
                    let (width, height) = utils::parse_resize_spec(resize_spec);
                    let alt_text = Path::new(&tgt_slug)
                        .file_stem()
                        .unwrap()
                        .to_str()
                        .unwrap_or("");
                    html! {
                        img .embed-file.image src=(tgt_slug) alt=(alt_text) #(anchor_id) width=[width] height=[height] {}
                    }
                } else {
                    // TODO(feature): handle other embedding types
                    // Fallback to raw url
                    html! {
                        span .embed-file #(anchor_id) {
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            // Inject anchor id for matched referenceable
            if let Some(id) = refable_anchor_id_map.get(&range) {
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );

            let tag = format!("h{}", *level as usize);

            // id: anchor id from matched referenceable takes precedence
            let id_attr = {
                let id_from_matched_referable = refable_anchor_id_map
                    .get(&range)
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );

            // Extract class name determination
            let class_name = kind.as_ref().map(|bq_kind| match bq_kind {
                BlockQuoteKind::Note => "markdown-alert-note",
                BlockQuoteKind::Tip => "markdown-alert-tip",
                BlockQuoteKind::Important => "markdown-alert-important",
                BlockQuoteKind::Warning => "markdown-alert-warning",
                BlockQuoteKind::Caution => "markdown-alert-caution",
            });

            let id_opt = refable_anchor_id_map.get(&range);

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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );

            let id_opt = refable_anchor_id_map.get(&range);

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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            // Inject anchor id for matched referenceable
            if let Some(id) = refable_anchor_id_map.get(&range) {
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
            let code_src = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );

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
                Some(lang) if lang == "mermaid" => render_mermaid(
                    &code_src,
                    node_render_config.mermaid_render_mode,
                ),
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                (PreEscaped(children))
            }
        }
        Emphasis => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                em { (PreEscaped(children)) }
            }
        }
        Strong => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                strong { (PreEscaped(children)) }
            }
        }
        Strikethrough => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                del { (PreEscaped(children)) }
            }
        }
        Superscript => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                sup { (PreEscaped(children)) }
            }
        }
        Subscript => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                table { (PreEscaped(children)) }
            }
        }
        TableHead => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                thead { (PreEscaped(children)) }
            }
        }
        TableRow => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                tr { (PreEscaped(children)) }
            }
        }
        TableCell => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
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
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                dl { (PreEscaped(children)) }
            }
        }
        DefinitionListTitle => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                dt { (PreEscaped(children)) }
            }
        }
        DefinitionListDefinition => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
            );
            html! {
                dd { (PreEscaped(children)) }
            }
        }
        FootnoteDefinition(name) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_map,
                refable_anchor_id_map,
                node_render_config,
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

    markup.into_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::export::utils::{
        build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
    };
    use crate::link::{build_links, scan_vault};
    use insta::*;
    use maud::DOCTYPE;
    use std::fs;

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
        let (_, referenceables, references) =
            scan_vault(vault_root_dir, vault_root_dir, false);
        let (links, _unresolved) =
            build_links(references, referenceables.clone());

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

        // Render note
        let note_path = Path::new("Note 1.md");
        let md_src =
            fs::read_to_string(vault_root_dir.join(note_path)).unwrap();
        let tree = Tree::new(&md_src);
        let node_render_config = NodeRenderConfig {
            mermaid_render_mode: MermaidRenderMode::from_str("client-side")
                .unwrap(),
        };
        let rendered = render_content(
            &tree,
            note_path,
            &links,
            &vault_path_to_slug_map,
            &innote_refable_anchor_id_map,
            &node_render_config,
        );

        let rendered = format_html_simple(&rendered);
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
