import { Plugin, parseLinktext, resolveSubpath } from "obsidian";
import { writeFileSync } from "fs";

interface ResolvedResult {
  file: string | null;
  subpath_type: "heading" | "block" | null;
  heading: string | null;
  heading_level: number | null;
  block_id: string | null;
}

interface LinkResult {
  source: string;
  link: string;
  resolved: ResolvedResult;
}

export default class LinkResolverPlugin extends Plugin {
  async onload() {
    this.registerObsidianProtocolHandler("link-resolver", (params) => {
      const output = params.output;
      if (!output) {
        console.error("[link-resolver] missing ?output= parameter");
        return;
      }
      this.app.workspace.onLayoutReady(() => this.run(output));
    });

    this.addCommand({
      id: "run",
      name: "Run",
      callback: () => {
        const output = `${this.app.vault.adapter.basePath}/link-resolver-output.json`;
        this.run(output);
      },
    });
  }

  run(outputPath: string) {
    const results: LinkResult[] = [];

    for (const file of this.app.vault.getMarkdownFiles()) {
      const cache = this.app.metadataCache.getFileCache(file);
      if (!cache) continue;

      const links = [...(cache.links ?? []), ...(cache.embeds ?? [])];
      for (const entry of links) {
        results.push({
          source: file.path,
          link: entry.link,
          resolved: this.resolve(entry.link, file.path),
        });
      }
    }

    writeFileSync(outputPath, JSON.stringify({ results }, null, 2));
    console.log(`[link-resolver] wrote ${outputPath}`);
  }

  resolve(link: string, source: string): ResolvedResult {
    const { path, subpath } = parseLinktext(link);

    let resolvedFile: string | null = null;
    let subpathType: "heading" | "block" | null = null;
    let heading: string | null = null;
    let headingLevel: number | null = null;
    let blockId: string | null = null;

    const target = (() => {
      if (path === "") return this.app.vault.getFileByPath(source) ?? null;
      const f = this.app.metadataCache.getFirstLinkpathDest(path, source);
      resolvedFile = f?.path ?? null;
      return f ?? null;
    })();

    if (path === "") resolvedFile = target?.path ?? null;

    if (target && subpath) {
      const cache = this.app.metadataCache.getFileCache(target);
      if (cache) {
        const sub = resolveSubpath(cache, subpath);
        if (sub?.type === "heading") {
          subpathType = "heading";
          heading = sub.current.heading;
          headingLevel = sub.current.level;
        } else if (sub?.type === "block") {
          subpathType = "block";
          blockId = sub.block.id;
        }
      }
    }

    return { file: resolvedFile, subpath_type: subpathType, heading, heading_level: headingLevel, block_id: blockId };
  }
}
