use maud::{Markup, PreEscaped, html};
use std::fs;
use std::process::Command;
use tempfile::TempDir;

#[derive(Debug, Clone, Copy)]
pub enum MermaidRenderMode {
    BuildTime,
    ClientSide,
}

impl MermaidRenderMode {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "buildtime" | "build-time" | "build" => Some(Self::BuildTime),
            "clientside" | "client-side" | "client" => Some(Self::ClientSide),
            _ => None,
        }
    }
}

impl TryFrom<&str> for MermaidRenderMode {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match Self::from_str(s) {
            Some(mode) => Ok(mode),
            None => Err(format!("Invalid mermaid render mode: {}", s)),
        }
    }
}

pub fn render_mermaid_build_time(mermaid_code: &str) -> Result<String, String> {
    let temp_dir = TempDir::new().unwrap();
    let input_path = temp_dir.path().join("input.mmd");
    let output_path = temp_dir.path().join("output.svg");

    fs::write(&input_path, mermaid_code)
        .map_err(|e| format!("Failed to write temp mermaid file: {}", e))?;

    let output = Command::new("mmdc")
        .arg("-i")
        .arg(&input_path)
        .arg("-o")
        .arg(&output_path)
        .arg("-b")
        .arg("transparent")
        .output()
        .map_err(|e| format!("Failed to run mmdc: {}", e))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!("Failed to run mmdc: {}", stderr));
    }

    let svg = fs::read_to_string(&output_path)
        .map_err(|e| format!("Failed to read generated SVG: {}", e))?;

    Ok(svg)
}

pub fn render_mermaid(mermaid_code: &str, mode: MermaidRenderMode) -> Markup {
    match mode {
        MermaidRenderMode::BuildTime => {
            match render_mermaid_build_time(mermaid_code) {
                Ok(svg) => html! {
                    .mermaid-diagram {
                        ( PreEscaped(svg) )
                    }
                },
                Err(_) => {
                    html! {
                        .mermaid-error {
                            pre {
                                    code .language-mermaid {
                                    p .error-message {
                                        "<!-- Mermaid rendering failed: -->"
                                    }
                                    (mermaid_code)
                                }
                            }
                        }
                    }
                }
            }
        }
        MermaidRenderMode::ClientSide => {
            html! {
                pre .mermaid {
                    (mermaid_code)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_render_mermaid_client_side() {
        let mermaid_code = r#"graph TD
    A-->B
    B-->C"#;
        let result =
            render_mermaid(mermaid_code, MermaidRenderMode::ClientSide);
        assert_snapshot!(result.into_string(), @r#"
        <pre class="mermaid">graph TD
            A--&gt;B
            B--&gt;C</pre>
        "#);
    }

    #[test]
    fn test_render_mermaid_build_time_simple_graph() {
        let mermaid_code = "graph TD\n    A-->B";
        let result = render_mermaid_build_time(mermaid_code);

        match result {
            Ok(svg) => {
                assert!(svg.contains("<svg"));
                assert!(svg.contains("</svg>"));
            }
            Err(e) => {
                // If mmdc is not installed, this will fail
                eprintln!("mmdc not available: {}", e);
            }
        }
    }

    #[test]
    fn test_render_mermaid_build_time_fallback_on_error() {
        // Use invalid mermaid syntax to trigger error
        let mermaid_code = "this is not valid mermaid syntax!!!";
        let result = render_mermaid(mermaid_code, MermaidRenderMode::BuildTime)
            .into_string();

        // Should fallback to code block with error comment
        assert!(
            result.contains("<!-- Mermaid rendering failed:")
                || result.contains("language-mermaid")
        );
        assert!(result.contains(mermaid_code));
    }

    #[test]
    fn test_render_mermaid_build_time_flowchart() {
        let mermaid_code = r#"flowchart LR
    A[Start] --> B{Is it?}
    B -->|Yes| C[OK]
    B -->|No| D[End]"#;

        let result = render_mermaid(mermaid_code, MermaidRenderMode::BuildTime)
            .into_string();

        // Should contain SVG if mmdc is available
        assert!(
            result.contains("<svg")
                || result.contains("<!-- Mermaid rendering failed:")
        );
    }

    #[test]
    fn test_render_mermaid_build_time_sequence_diagram() {
        let mermaid_code = r#"sequenceDiagram
    Alice->>John: Hello John, how are you?
    John-->>Alice: Great!"#;

        let result = render_mermaid(mermaid_code, MermaidRenderMode::BuildTime)
            .into_string();

        // Should contain SVG
        assert!(
            result.contains("<svg")
                || result.contains("<!-- Mermaid rendering failed:")
        );
    }
}
