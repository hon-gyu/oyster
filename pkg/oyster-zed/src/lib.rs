use zed_extension_api as zed;

struct OysterExtension;

impl zed::Extension for OysterExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> zed::Result<zed::Command> {
        let path = worktree
            .which("oystermark-lsp")
            .ok_or_else(|| "oystermark-lsp not found in PATH".to_string())?;

        Ok(zed::Command {
            command: path,
            args: vec![],
            env: worktree.shell_env(),
        })
    }
}

zed::register_extension!(OysterExtension);
