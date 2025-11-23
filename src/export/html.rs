use crate::export::writer::push_html;
/// HTML conversion from markdown with link rewriting
use crate::link::Link;
use crate::link::Referenceable;
use crate::link::percent_decode;
use crate::parse::default_opts;
use pulldown_cmark::{CowStr, Event, LinkType, Parser, Tag};
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

/// Exports markdown to HTML
///
/// Arguments:
/// - `md_src`: The raw markdown content
/// - `path`: Path of the current file being converted
/// - `links`: resolved links
///
/// Returns: HTML string
///
/// We are usign pulldown-cmark's HTML writer to convert the markdown to HTML.
/// But with some modifications:
/// - for link events, we rewrite the destination to point to the resolved ones
/// - the html writer is customized to take in a byte-range-to-anchor-id map
///     with will inject anchors for certain event that corresponds to in-note
///     refereceable (headings and blocks)
pub fn export_to_html_body(
    md_src: &str,
    path: &Path,
    resolved_links: &[Link],
    path_to_slug_map: &HashMap<PathBuf, String>,
    in_note_anchor_id_map: &HashMap<Range<usize>, String>,
) -> String {
    // Build a lookup map: dest string -> out-going link
    let link_map: HashMap<&str, &Link> = resolved_links
        .iter()
        // Filter out links that are not in the current file
        .filter(|link| link.from.path == path)
        .map(|link| (link.from.dest.as_str(), link))
        .collect();

    // Parse with the same options as in link resolution
    let opts = default_opts();
    let parser = Parser::new_ext(md_src, opts);

    // Transform wikilinks and markdown links to .md files
    let transformed = parser.into_offset_iter().map(|(event, range)| {
        let transformed_event = match event {
            Event::Start(Tag::Link {
                link_type,
                dest_url,
                title,
                id,
            }) => {
                if !matches!(
                    link_type,
                    LinkType::Inline | LinkType::WikiLink { .. }
                ) {
                    Event::Start(Tag::Link {
                        link_type,
                        dest_url,
                        title,
                        id,
                    })
                } else {
                    // Decode percent-encoded URLs for inline links
                    let decoded = percent_decode(dest_url.as_ref());
                    let dest_str = if link_type == LinkType::Inline {
                        decoded.as_str()
                    } else {
                        dest_url.as_ref()
                    };

                    let resolved_link_opt = link_map.get(dest_str);

                    if let Some(resolved_link) = resolved_link_opt {
                        // Rewrite to point to generated HTML
                        let link_tgt = &resolved_link.to;
                        let tgt_slug =
                            path_to_slug_map.get(link_tgt.path()).unwrap();
                        let resolved_dest = match link_tgt {
                            // Non-in-note referenceable
                            Referenceable::Asset { .. }
                            | Referenceable::Note { .. } => tgt_slug.clone(),
                            // In-note referenceable
                            Referenceable::Block {
                                range: in_note_refable_range,
                                ..
                            }
                            | Referenceable::Heading {
                                range: in_note_refable_range,
                                ..
                            } => {
                                debug_assert!(in_note_refable_range == &range, "In-note referenceable range should match the event range. Guaranteed during extraction.");
                                let anchor_id = in_note_anchor_id_map
                                    .get(&range)
                                    .unwrap()
                                    .clone();
                                format!("{}#{}", tgt_slug, anchor_id)
                            }
                        };

                        Event::Start(Tag::Link {
                            link_type: LinkType::Inline,
                            dest_url: CowStr::from(resolved_dest),
                            title: title.clone(),
                            id: CowStr::from(""),
                        })
                    } else {
                        // Keep original link (unresolved)
                        Event::Start(Tag::Link {
                            link_type: LinkType::Inline,
                            dest_url,
                            title,
                            id: CowStr::from(""),
                        })
                    }
                }
            }
            other => other,
        };

        (transformed_event, range)
    });

    // Render to HTML
    let mut html_output = String::new();
    push_html(&mut html_output, transformed, in_note_anchor_id_map);

    html_output
}
