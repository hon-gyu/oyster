use super::utils::{
    build_in_note_anchor_id_map, build_vault_paths_to_slug_map,
};
use crate::ast::{Node, NodeKind::*, Tree};
use crate::link::types::{Link as ResolvedLink, Reference, Referenceable};
use crate::link::{build_links, scan_vault};
use maud::{Markup, PreEscaped, html};
use pulldown_cmark::{BlockQuoteKind, CodeBlockKind, LinkType};
use std::collections::HashMap;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};

///
///
/// Placeholder for the replacement of `generate_site`
/// TODO(refactor): move this
fn render_vault(
    vault_path: &Path,
    output_dir: &Path,
) -> Result<(), Box<dyn std::error::Error>> {
    fs::create_dir_all(output_dir)?;

    // Scan the vault and build links
    let (referenceables, references) = scan_vault(vault_path, vault_path);
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

    Ok(())
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
fn render_content(
    tree: &Tree,
    vault_path: &Path,
    resolved_links: &[ResolvedLink],
    vault_path_to_slug_map: &HashMap<PathBuf, String>,
    innote_refable_anchor_id_map: &HashMap<
        PathBuf,
        HashMap<Range<usize>, String>,
    >,
) -> String {
    // Outgoing links
    // build a map of: src (this) reference's byte range |-> tgt slug
    let matched_references = resolved_links
        .iter()
        .filter(|link| link.tgt_path_eq(vault_path))
        .map(|link| &link.from)
        .collect::<Vec<_>>();
    let reference_slug_dest_map: HashMap<Range<usize>, String> =
        matched_references
            .iter()
            .map(|reference| {
                let range = &reference.range;
                let slug = vault_path_to_slug_map
                    .get(&reference.path)
                    .expect("reference path not found");
                (range.clone(), slug.clone())
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
        &reference_slug_dest_map,
        &in_note_anchor_id_map,
    );
    rendered
}

fn render_nodes(
    nodes: &[Node],
    vault_path: &Path,
    ref_slug_map: &HashMap<Range<usize>, String>,
    refable_anchor_id_map: &HashMap<Range<usize>, String>,
) -> String {
    let mut buffer = String::new();
    for node in nodes {
        let rendered =
            render_node(node, vault_path, ref_slug_map, refable_anchor_id_map);
        buffer.push_str(rendered.as_str());
    }

    buffer
}

fn render_node(
    node: &Node,
    vault_path: &Path,
    ref_slug_map: &HashMap<Range<usize>, String>,
    refable_anchor_id_map: &HashMap<Range<usize>, String>,
) -> String {
    let range = node.start_byte..node.end_byte;
    let markup = match &node.kind {
        Document => {
            let children_rendered = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                article {
                    (PreEscaped(children_rendered))
                }
            }
        }
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
                span class="math math-inline" { (text.as_ref()) }
            }
        }
        DisplayMath(text) => {
            html! {
                span class="math math-display" { (text.as_ref()) }
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
        // Container nodes
        Paragraph => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
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
            id: id_from_md_src,
            classes,
            attrs,
        } => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );

            let tag = format!("h{}", level);

            // Build attributes as string parts
            let id_attr = if let Some(id_from_matched_referable) =
                refable_anchor_id_map.get(&range)
            {
                format!(" id=\"{}\"", id_from_matched_referable)
            } else {
                id_from_md_src
                    .as_ref()
                    .map(|id_val| format!(" id=\"{}\"", id_val.as_ref()))
                    .unwrap_or_default()
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
                ref_slug_map,
                refable_anchor_id_map,
            );

            // Extract class name determination
            let class_name = kind.as_ref().map(|bq_kind| match bq_kind {
                BlockQuoteKind::Note => "markdown-alert-note",
                BlockQuoteKind::Tip => "markdown-alert-tip",
                BlockQuoteKind::Important => "markdown-alert-important",
                BlockQuoteKind::Warning => "markdown-alert-warning",
                BlockQuoteKind::Caution => "markdown-alert-caution",
            });

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
        CodeBlock(kind) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );

            // Extract language class determination
            let language_class = match kind {
                CodeBlockKind::Fenced(info) => {
                    let lang = info.split(' ').next().unwrap();
                    if lang.is_empty() {
                        None
                    } else {
                        Some(format!("language-{}", lang))
                    }
                }
                CodeBlockKind::Indented => None,
            };

            match language_class {
                Some(class) => html! {
                    pre {
                        code class=(class) { (PreEscaped(children)) }
                    }
                },
                None => html! {
                    pre {
                        code { (PreEscaped(children)) }
                    }
                },
            }
        }
        HtmlBlock => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                (PreEscaped(children))
            }
        }
        List(start) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );

            // Extract list type and start attribute determination
            match start {
                Some(1) => html! {
                    ol { (PreEscaped(children)) }
                },
                Some(start_num) => html! {
                    ol start=(start_num) { (PreEscaped(children)) }
                },
                None => html! {
                    ul { (PreEscaped(children)) }
                },
            }
        }
        Item => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                li { (PreEscaped(children)) }
            }
        }
        Emphasis => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                em { (PreEscaped(children)) }
            }
        }
        Strong => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                strong { (PreEscaped(children)) }
            }
        }
        Strikethrough => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                del { (PreEscaped(children)) }
            }
        }
        Superscript => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                sup { (PreEscaped(children)) }
            }
        }
        Subscript => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                sub { (PreEscaped(children)) }
            }
        }
        Link {
            link_type,
            dest_url,
            title,
            ..
        } => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            let href = if matches!(link_type, LinkType::Email) {
                format!("mailto:{}", dest_url.as_ref())
            } else {
                dest_url.to_string()
            };
            let title_attr = if title.is_empty() {
                String::new()
            } else {
                format!(" title=\"{}\"", title.as_ref())
            };
            html! {
                (PreEscaped(format!("<a href=\"{}\"{}>{}</a>", href, title_attr, children)))
            }
        }
        Image {
            dest_url, title, ..
        } => {
            // For images, children contain the alt text
            let alt_text = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
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
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                table { (PreEscaped(children)) }
            }
        }
        TableHead => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                thead { (PreEscaped(children)) }
            }
        }
        TableRow => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                tr { (PreEscaped(children)) }
            }
        }
        TableCell => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
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
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                dl { (PreEscaped(children)) }
            }
        }
        DefinitionListTitle => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                dt { (PreEscaped(children)) }
            }
        }
        DefinitionListDefinition => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
            );
            html! {
                dd { (PreEscaped(children)) }
            }
        }
        FootnoteDefinition(name) => {
            let children = render_nodes(
                &node.children,
                vault_path,
                ref_slug_map,
                refable_anchor_id_map,
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
