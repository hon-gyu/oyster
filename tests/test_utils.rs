use maud::{DOCTYPE, PreEscaped, html};

pub fn format_html_simple(html: &str) -> String {
    html.replace("><", ">\n<")
        .replace("<h", "\n<h")
        .replace("<p", "\n<p")
        .replace("</article>", "\n</article>")
}

pub fn render_full_html(content: &str) -> String {
    html! {
        (DOCTYPE)
        body {
            (PreEscaped(content))
        }
    }
    .into_string()
}
