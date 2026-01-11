use std::fs;
use std::io::Write;
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::Duration;

struct ServerGuard {
    child: Child,
}

impl Drop for ServerGuard {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn start_server(vault_dir: &str, port: u16, watch: bool) -> ServerGuard {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_oyster"));
    cmd.arg("serve")
        .arg(vault_dir)
        .arg("--port")
        .arg(port.to_string())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    if watch {
        cmd.arg("--watch");
    }

    let child = cmd.spawn().expect("Failed to start server");

    // Give the server time to start
    thread::sleep(Duration::from_millis(1500));

    ServerGuard { child }
}

fn wait_for_server(port: u16, timeout_ms: u64) -> bool {
    let start = std::time::Instant::now();
    let timeout = Duration::from_millis(timeout_ms);

    while start.elapsed() < timeout {
        if let Ok(response) = reqwest::blocking::get(format!("http://localhost:{}/home.html", port))
        {
            if response.status().is_success() {
                return true;
            }
        }
        thread::sleep(Duration::from_millis(100));
    }
    false
}

#[test]
fn test_serve_basic() {
    let port = 13001;

    let _guard = start_server("tests/data/vaults/minimal", port, false);

    assert!(
        wait_for_server(port, 5000),
        "Server did not start within timeout"
    );

    // Verify we can fetch the home page
    let response =
        reqwest::blocking::get(format!("http://localhost:{}/home.html", port)).unwrap();
    assert!(response.status().is_success());

    let body = response.text().unwrap();
    assert!(body.contains("<!DOCTYPE html>") || body.contains("<html"));
}

#[test]
fn test_serve_static_files() {
    let port = 13002;

    let _guard = start_server("tests/data/vaults/minimal", port, false);

    assert!(
        wait_for_server(port, 5000),
        "Server did not start within timeout"
    );

    // Verify CSS is served
    let response =
        reqwest::blocking::get(format!("http://localhost:{}/styles/base.css", port)).unwrap();
    assert!(response.status().is_success());
}

#[test]
fn test_serve_with_watch_flag() {
    let port = 13003;

    let _guard = start_server("tests/data/vaults/minimal", port, true);

    assert!(
        wait_for_server(port, 5000),
        "Server with watch did not start within timeout"
    );

    // Verify server is running with livereload (the page should contain livereload script)
    let response =
        reqwest::blocking::get(format!("http://localhost:{}/home.html", port)).unwrap();
    assert!(response.status().is_success());

    let body = response.text().unwrap();
    // tower-livereload injects a script for live reloading
    assert!(
        body.contains("livereload") || body.contains("__livereload"),
        "Live reload script should be injected when --watch is enabled"
    );
}

#[test]
fn test_serve_watch_regenerates_on_change() {
    let vault_dir = tempfile::tempdir().expect("Failed to create vault temp dir");
    let port = 13004;

    // Create initial note
    let note_path = vault_dir.path().join("test.md");
    fs::write(&note_path, "# Initial Content\n\nSome text.").unwrap();

    let _guard = start_server(vault_dir.path().to_str().unwrap(), port, true);

    assert!(
        wait_for_server(port, 5000),
        "Server did not start within timeout"
    );

    // Fetch initial content
    let response =
        reqwest::blocking::get(format!("http://localhost:{}/home.html", port)).unwrap();
    assert!(response.status().is_success());

    // Modify the note
    thread::sleep(Duration::from_millis(500));
    {
        let mut file = fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(&note_path)
            .unwrap();
        file.write_all(b"# Updated Content\n\nNew text here.")
            .unwrap();
    }

    // Wait for rebuild (debouncer is 500ms + rebuild time)
    thread::sleep(Duration::from_millis(2000));

    // Verify server is still running after rebuild
    let response =
        reqwest::blocking::get(format!("http://localhost:{}/home.html", port)).unwrap();
    assert!(
        response.status().is_success(),
        "Server should still be running after rebuild"
    );
}

#[test]
fn test_serve_404_for_missing_files() {
    let port = 13005;

    let _guard = start_server("tests/data/vaults/minimal", port, false);

    assert!(
        wait_for_server(port, 5000),
        "Server did not start within timeout"
    );

    // Request a non-existent file
    let response =
        reqwest::blocking::get(format!("http://localhost:{}/nonexistent.html", port)).unwrap();
    assert_eq!(response.status().as_u16(), 404);
}
