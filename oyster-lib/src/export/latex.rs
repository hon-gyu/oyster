use maud::html;

pub fn render_latex(latex: &str, display_mode: bool) -> String {
    let opts = katex::Opts::builder()
        .display_mode(display_mode)
        .throw_on_error(false)
        .build()
        .unwrap();

    match katex::render_with_opts(latex, opts) {
        Ok(rendered) => rendered,
        Err(e) => {
            // Fallback to raw LaTeX wrapped in span
            let err_msg = e.to_string();
            let class_name = if display_mode {
                "math-error math-display"
            } else {
                "math-error math-inline"
            };
            let html = html! {
                comment { (err_msg) }
                span class=(class_name) {(latex)}
            };

            html.into_string()
        }
    }
}
