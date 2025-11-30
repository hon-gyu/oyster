use maud::{Markup, html};
use serde_yaml::Value;

const FRONTMATTER_IGNORE_KEYS: [&str; 3] = ["title", "publish", "draft"];

// Render fronmatter
//
// It differs from render_yaml in:
// - value must be a mapping
// - some fields are rendered differently
pub fn render_frontmatter(value: &Value) -> Option<Markup> {
    let mapping = value.as_mapping()?;

    let mut key_to_val_html_mapping: Vec<(&str, Markup)> = Vec::new();
    for (key, value) in mapping.iter() {
        let key = key
            .as_str()
            .expect("Yaml mapping key cannot be converted to string");
        match key {
            k if FRONTMATTER_IGNORE_KEYS.contains(&k) => {}
            "tags" => {
                let val_html = handle_tags_value(value);
                key_to_val_html_mapping.push(("tags", val_html?));
            }
            "title" => {
                let val_html = render_yaml(value);
                let html = html! {
                    .frontmatter-title datetime=(serde_yaml::to_string(&value).expect("Could not convert title yaml value to string")) {
                        (&val_html)
                    }
                };
                key_to_val_html_mapping.push(("date", html));
            }
            "date" => {
                let val_html = render_yaml(value);
                let html = html! {
                    time .frontmatter-date datetime=(serde_yaml::to_string(&value).expect("Could not convert date yaml value to string")) {
                        (&val_html)
                    }
                };
                key_to_val_html_mapping.push(("date", html));
            }
            other => {
                let val_html = render_yaml(value);
                key_to_val_html_mapping.push((other, val_html));
            }
        }
    }
    let html = html! {
        table .frontmatter {
            @for (key, val_markup) in key_to_val_html_mapping.iter() {
                tr {
                    td { (key) }
                    td { (val_markup) }
                }
            }
        }
    };
    Some(html)
}

// Render a yaml value in general
fn render_yaml(value: &Value) -> Markup {
    match value {
        Value::String(..) | Value::Bool(..) | Value::Number(..) => {
            let value_str = serde_yaml::to_string(value)
                .expect("Could not convert a yaml value to a string");
            html! { (value_str) }
        }
        Value::Null => html! { (String::from("null")) },
        Value::Sequence(seq) => {
            let seq_html =
                seq.iter().map(|v| render_yaml(v)).collect::<Vec<Markup>>();
            html! {
                ul {
                    @for item in seq_html.iter() {
                        ui { (item) }
                    }
                }
            }
        }
        Value::Mapping(map) => {
            html! {dl {
                @for (k, v) in map {
                    dt { (k.as_str().expect("Could not convert a key of a yaml mapping to a string")) }
                    dd { (render_yaml(v)) }
                }
            }}
        }
        Value::Tagged(tagged) => {
            let val = &tagged.value;
            render_yaml(val)
        }
    }
}

// Special handling for "tags" field in the top mapping
fn handle_tags_value(value: &Value) -> Option<Markup> {
    match value {
        // Scalar values
        Value::String(..) | Value::Bool(..) | Value::Number(..) => {
            let seq_ed = Value::Sequence(vec![value.clone()]);
            handle_tags_value(&seq_ed)
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

pub fn get_title(fm: &Value) -> Option<String> {
    let mapping = fm.as_mapping()?;
    mapping
        .get("title")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_render_frontmatter() {
        let fm_src = r#"title: Getting Started
author: Jane Doe
date: 2024-03-15
tags:
    - programming
    - tutorial
category: tutorials
publish: true"#;
        let fm = serde_yaml::from_str::<Value>(fm_src).unwrap();
        let rendered = render_frontmatter(&fm).unwrap();
        assert_snapshot!(rendered.into_string(), @r#"
        <dl class="frontmatter"><dt>date</dt><dd><div class="frontmatter-title" datetime="Getting Started
        ">Getting Started
        </div></dd><dt>author</dt><dd>Jane Doe
        </dd><dt>date</dt><dd><time class="frontmatter-date" datetime="2024-03-15
        ">2024-03-15
        </time></dd><dt>tags</dt><dd><span class="frontmatter-tags"><span class="frontmatter-tag">#programming</span>, <span class="frontmatter-tag">#tutorial</span></span></dd><dt>category</dt><dd>tutorials
        </dd></dl>
        "#);
    }
}
