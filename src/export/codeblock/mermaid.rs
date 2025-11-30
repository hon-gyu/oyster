use maud::html;
use std::fs;
use std::io;
use std::process::Command;
use tempfile::{NamedTempFile, TempDir};

#[derive(Debug, Clone, Copy)]
pub enum MermaidRenderMode {
    BuildTime,
    ClientSide,
}

impl MermaidRenderMode {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "buildtime" | "build-time" => Some(Self::BuildTime),
            "clientside" | "client-side" => Some(Self::ClientSide),
            _ => None,
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

pub fn render_mermaid(mermaid_code: &str, mode: MermaidRenderMode) -> String {
    match mode {
        MermaidRenderMode::BuildTime => {
            match render_mermaid_build_time(mermaid_code) {
                Ok(svg) => svg,
                Err(e) => {
                    let html = html! {
                        "<!-- Mermaid rendering failed: " (e) " -->"
                        pre {
                                code .language-mermaid {
                                (mermaid_code)
                            }
                        }
                    };
                    html.into_string()
                }
            }
        }
        MermaidRenderMode::ClientSide => todo!(),
    }
}
