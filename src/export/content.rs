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
use crate::ast::callout::FoldableState;
use crate::ast::{
    Node,
    NodeKind::{self, *},
    Tree,
};
use crate::export::utils::range_to_anchor_id;
use crate::link::Referenceable;
use maud::{Markup, PreEscaped, html};
use pulldown_cmark::{CodeBlockKind, LinkType};
use std::path::{Path, PathBuf};

pub struct NodeRenderConfig {
    /// Whether to render softbreak as `<br>`
    pub preserve_softbreak: bool,
    pub mermaid_render_mode: MermaidRenderMode,
    pub tikz_render_mode: TikzRenderMode,
    pub quiver_render_mode: QuiverRenderMode,
}

impl Default for NodeRenderConfig {
    fn default() -> Self {
        Self {
            preserve_softbreak: true,
            mermaid_render_mode: MermaidRenderMode::from_str("client-side")
                .unwrap(),
            tikz_render_mode: TikzRenderMode::from_str("client-side").unwrap(),
            quiver_render_mode: QuiverRenderMode::from_str("raw").unwrap(),
        }
    }
}

/// Find a node in the tree by its exact byte range
fn find_node_by_range<'a>(
    node: &'a Node,
    target_range: &std::ops::Range<usize>,
) -> Option<&'a Node<'a>> {
    // Check if this node matches the target range
    if node.start_byte == target_range.start
        && node.end_byte == target_range.end
    {
        return Some(node);
    }

    // Recursively search children
    for child in &node.children {
        if let Some(found) = find_node_by_range(child, target_range) {
            return Some(found);
        }
    }

    None
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
    render_node(
        &tree.root_node,
        vault_path,
        vault_db,
        node_render_config,
        embed_depth,
        max_embed_depth,
    )
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

    match &node.kind {
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

            fn get_note_header_elem(
                vault_db: &dyn VaultDB,
                tgt_vault_path: &PathBuf,
            ) -> Markup {
                let tgt_title = vault_db
                    .get_title_from_note_vault_path(tgt_vault_path)
                    .expect("Note should have a title");
                html! {
                    p .embed-file.header {
                        (format!("📑 {}", tgt_title))
                    }
                }
            }

            if let Some(tgt) = vault_db.get_tgt_from_src(vault_path, &range) {
                let tgt_vault_path = tgt.path();
                let tgt_slug_path = vault_db
                    .get_slug_from_file_vault_path(tgt_vault_path)
                    .expect("Referenceable should have a slug");

                if matches!(tgt, Referenceable::Asset { .. }) {
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
                        let (width, height) =
                            utils::parse_resize_spec(resize_spec);
                        let alt_text = Path::new(&tgt_slug_path)
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap_or("");
                        // src anchor id to be used as href in backlink
                        let src_anchor_id = vault_db
                            .get_reference_anchor_id(
                                &vault_path.to_path_buf(),
                                &range,
                            )
                            .expect("Image should have a src anchor id");
                        html! {
                            img
                                .embed-file.image
                                #(src_anchor_id)
                                embed-depth=(embed_depth)
                                src=(tgt_slug_path)
                                alt=(alt_text)
                                width=[width] height=[height]
                                {}
                        }
                    } else {
                        // Unhandled embeded asset
                        // TODO: handle audio, video, pdf, etc.
                        let href = dest_url.to_string();
                        // Just an anchor
                        html! {
                            .embed-file.unhandled-asset
                            embed-depth=(embed_depth) {
                                a
                                    href=(href)
                                    title=[title_opt]
                                    {
                                        (PreEscaped(children))
                                    }
                            }
                        }
                    }
                } else {
                    // Case: note
                    let note_header_elem =
                        get_note_header_elem(vault_db, tgt_vault_path);
                    if embed_depth >= max_embed_depth {
                        // Reach max embed depth, render as internal link instead
                        let tgt_slug = vault_db
                            .get_tgt_slug_from_src(vault_path, &range)
                            .expect(
                                "Should have relative slug for resolved link",
                            );
                        html! {
                            .embed-file.max-embed-depth embed-depth=(embed_depth) {
                                a  href=(tgt_slug) title=[title_opt] {
                                    (note_header_elem)
                                }
                            }
                        }
                    } else {
                        match tgt {
                            Referenceable::Note { .. } => {
                                let tgt_tree = vault_db
                                    .get_ast_tree_from_note_vault_path(tgt_vault_path).expect("Note should have an AST, either from cache or newly built");
                                let embed_depth = embed_depth + 1;
                                let embed_content = render_content(
                                    &tgt_tree,
                                    tgt_vault_path,
                                    vault_db,
                                    node_render_config,
                                    embed_depth,
                                    max_embed_depth,
                                );
                                html! {
                                    .embed-file.note embed-depth=(embed_depth) {
                                        .header {
                                            (note_header_elem)
                                        }

                                        .content {
                                            (embed_content)
                                        }
                                    }
                                }
                            }
                            Referenceable::Heading {
                                range: tgt_range, ..
                            } => {
                                let tgt_tree = vault_db
                                    .get_ast_tree_from_note_vault_path(tgt_vault_path).expect("Note should have an AST, either from cache or newly built");
                                // Get heading node by finding the node with exact range match
                                let heading_node =
                                    find_node_by_range(&tgt_tree.root_node, tgt_range)
                                        .expect(
                                            "Referenceable's range should match the heading node's range",
                                        );

                                // Render the heading and its children
                                let embed_depth = embed_depth + 1;
                                let heading_content = render_node(
                                    heading_node,
                                    tgt_vault_path,
                                    vault_db,
                                    node_render_config,
                                    embed_depth,
                                    max_embed_depth,
                                );
                                html! {
                                    .embed-file.heading embed-depth=(embed_depth) {
                                        .header {
                                            (note_header_elem)
                                        }
                                        .content {
                                            (heading_content)
                                        }
                                    }
                                }
                            }
                            Referenceable::Block {
                                range: tgt_range, ..
                            } => {
                                let tgt_tree = vault_db
                                    .get_ast_tree_from_note_vault_path(tgt_vault_path).expect("Note should have an AST, either from cache or newly built");
                                // Get block node by finding the node with exact range match
                                let block_node =
                                    find_node_by_range(&tgt_tree.root_node, tgt_range)
                                        .expect(
                                            "Referenceable's range should match the block node's range",
                                        );

                                // Render the block and its children
                                let embed_depth = embed_depth + 1;
                                let block_content = render_node(
                                    block_node,
                                    tgt_vault_path,
                                    vault_db,
                                    node_render_config,
                                    embed_depth,
                                    max_embed_depth,
                                );
                                html! {
                                    .embed-file.block embed-depth=(embed_depth) {
                                        .header {
                                            (note_header_elem)
                                        }
                                        .content {
                                            (block_content)
                                        }
                                    }
                                }
                            }
                            Referenceable::Asset { .. } => unreachable!(
                                "Asset should have been handled earlier"
                            ),
                        }
                    }
                }
            } else {
                // No matched, fallback to raw url
                let href = dest_url.to_string();
                // Just an anchor
                html! {
                    a .embed-file.unresolved href=(href) title=[title_opt] {
                        (PreEscaped(children))
                    }
                }
            }
        }
        // Referenceable nodes
        Paragraph => {
            let only_child_is_embeded_file = node.children.len() == 1
                && matches!(&node.children[0].kind, NodeKind::Image { .. });

            let rendered_children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );

            // Inject anchor id for matched referenceable
            let refable_anchor_id = vault_db.get_innote_refable_anchor_id(
                &vault_path.to_path_buf(),
                &range,
            );

            html! {
                @if only_child_is_embeded_file {
                    (PreEscaped(rendered_children))
                } @else {
                    p id=[refable_anchor_id] { (PreEscaped(rendered_children)) }
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
        BlockQuote => {
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

            // Inject anchor id for matched referenceable
            html! {
                blockquote id=[id_opt] {
                    (PreEscaped(children))
                }
            }
        }
        Callout(foldable_state) => {
            // Inject anchor id for matched referenceable
            let id_opt = vault_db.get_innote_refable_anchor_id(
                &vault_path.to_path_buf(),
                &range,
            );

            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );

            match foldable_state {
                None => {
                    html! {
                        div .callout id=[id_opt] {
                            (PreEscaped(children))
                        }
                    }
                }
                Some(FoldableState::Expanded) => {
                    html! {
                        details .callout id=[id_opt] open {
                            (PreEscaped(children))
                        }
                    }
                }
                Some(FoldableState::Collapsed) => {
                    html! {
                        details .callout id=[id_opt] {
                            (PreEscaped(children))
                        }
                    }
                }
            }
        }
        CalloutContent => {
            let children = render_nodes(
                &node.children,
                vault_path,
                vault_db,
                node_render_config,
                embed_depth,
                max_embed_depth,
            );

            html! {
                div .callout-content {
                    (PreEscaped(children))
                }
            }
        }
        CalloutDeclaraion {
            kind,
            title,
            foldable,
        } => {
            let callout_kind = kind.name();

            // Use custom title or default title
            let callout_title = title
                .as_ref()
                .map(|s| s.as_str())
                .unwrap_or_else(|| kind.default_title());

            if foldable.is_some() {
                html! {
                    summary .callout-declaration callout-kind=(callout_kind) {
                        span .callout-icon {}
                        span .callout-title {
                            (callout_title)
                        }
                    }
                }
            } else {
                html! {
                    div .callout-declaration callout-kind=(callout_kind) {
                        span .callout-icon {}
                        span .callout-title {
                            (callout_title)
                        }
                    }
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
            if node_render_config.preserve_softbreak {
                html! {
                    br;
                }
            } else {
                html! {
                    " "
                }
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
                        code_src,
                        node_render_config.mermaid_render_mode,
                    );
                    html! {
                        (mermaid)
                    }
                }
                Some(lang) if lang == "tikz" => {
                    let tikz = render_tikz(
                        code_src,
                        node_render_config.tikz_render_mode,
                    );
                    html! {
                        (tikz)
                    }
                }
                Some(lang) if lang == "quiver" => {
                    let quiver = render_quiver(
                        code_src,
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::export::vault_db::{
        FileLevelInfo, StaticVaultStore, VaultLevelInfo,
    };
    use crate::link::scan_vault;
    use insta::*;
    use maud::DOCTYPE;

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

        format_html_simple(&markup.into_string())
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
        let vault_db = StaticVaultStore::new(
            vault_root_dir,
            file_level_info,
            vault_level_info,
        );

        // Render note
        let note_path = Path::new("Note 1.md");
        let md_src =
            fs::read_to_string(vault_root_dir.join(note_path)).unwrap();
        let tree = Tree::new_with_default_opts(&md_src);
        let node_render_config = NodeRenderConfig::default();
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
        <img class="embed-file image" id="423-441" embed-depth="0" src="blue-image.png" alt="blue-image">
        </img>

        <h2 id="additional-info">Additional Info</h2>

        <p>This section is referenced from the top of this note using a same-note heading reference.</p>

        <p id="key-point">This is a key point within the same note. ^key-point</p>

        <p>The line above has a block ID that’s referenced from earlier in this note.</p>

        </article></body>
        "#);
    }
}
