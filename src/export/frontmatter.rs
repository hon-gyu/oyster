use maud::{Markup, html};
use serde_yaml::Value;

pub fn render_yaml(value: &Value) -> Option<Markup> {
    let mapping = value.as_mapping()?;

    // Special handling for tags
    let tags: Option<Vec<String>> = mapping
        .get("tags")
        .and_then(|v| v.as_sequence())
        .map(|seq| {
            seq.iter()
                .filter_map(|tag| tag.as_str().map(|s| s.to_string()))
                .collect::<Vec<String>>()
        });
    let html = html! {};
    Some(html)
}

// Special handling for "tags" field in the top mapping
fn handle_tags_value(value: &Value) -> Option<Markup> {
    match value {
        // Scalar values
        Value::String(..) | Value::Bool(..) | Value::Number(..) => {
            serde_yaml::to_string(value).ok().map(|s| {
                let html = html! {
                    span { (s) }
                };
                html
            })
        }
        Value::Sequence(seq) => {
            let tags_html = seq
                .iter()
                .map(|v| {
                    // Force serialize tag values to string (ignoring depper structure)
                    serde_yaml::to_string(v)
                        .ok()
                        .expect("Could not convert a yaml value to a string")
                })
                // Trim as `to_string` adds trailing newline
                .map(|s| format!("#{}", s.trim()))
                .map(|s| {
                    html! {span .frontmatter-tag { (s) }}
                })
                .collect::<Vec<Markup>>();
            let html = html! {
                span.frontmatter-tags {
                    @for (i, tag) in tags_html.iter().enumerate() {
                        (tag)
                        @if i < tags_html.len() - 1 { ", " }
                    }
                }
            };
            Some(html)
        }
        Value::Mapping(..) | Value::Null | Value::Tagged(..) => None,
    }
}
