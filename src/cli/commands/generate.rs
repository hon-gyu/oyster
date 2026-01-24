use crate::cli::args::GenerateArgs;
use oyster::export::render_vault;
use std::path::PathBuf;

pub fn run(
    args: GenerateArgs,
    output: PathBuf,
) -> Result<(), Box<dyn std::error::Error>> {
    println!(
        "Generating site from vault: {}",
        args.vault_root_dir.display()
    );

    render_vault(
        &args.vault_root_dir,
        &output,
        &args.theme,
        args.filter_publish,
        args.home_note_path.as_deref(),
        args.home_name.as_deref(),
        &args.node_render_config(),
        args.custom_callout_css.as_deref(),
    )?;

    println!("Site generated to: {}", output.display());
    Ok(())
}

/// Generate site and return the home slug (used by serve command).
#[cfg(feature = "serve")]
pub fn run_with_home_slug(
    args: &GenerateArgs,
    output: &PathBuf,
) -> Result<String, Box<dyn std::error::Error>> {
    println!(
        "Generating site from vault: {}",
        args.vault_root_dir.display()
    );

    let home_slug = render_vault(
        &args.vault_root_dir,
        output,
        &args.theme,
        args.filter_publish,
        args.home_note_path.as_deref(),
        args.home_name.as_deref(),
        &args.node_render_config(),
        args.custom_callout_css.as_deref(),
    )?;

    println!("Site generated to: {}", output.display());
    Ok(home_slug)
}
