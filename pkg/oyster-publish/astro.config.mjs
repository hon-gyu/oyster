import { defineConfig } from "astro/config";
import { unified, rehypeHeadingIds } from "@astrojs/markdown-remark";
import mdx from "@astrojs/mdx";
import react from "@astrojs/react";
import rehypeKatex from "rehype-katex";
import rehypeAutolinkHeadings from "rehype-autolink-headings";
import remarkOymarkit from "./src/plugins/remark-oymarkit.ts";
import remarkOyWikilink from "./src/plugins/remark-oywikilink.ts";
import { outputDir } from "./src/lib/project-paths.ts";
import { publicDir } from "./src/lib/site-config.ts";

// The vault lives in ./vault (see src/content.config.ts). The markdown pipeline
// is configured via `markdown.processor` (Astro 7).
//
// `remarkOymarkit` replaces the parser: `.md` is parsed by the oymarkit OCaml
// parser (compiled to JS, see src/lib/oystermark) into mdast, so the project's
// CommonMark extensions (divs, djot attributes, callouts, wikilinks, tables)
// are understood at parse time. `remarkOyWikilink` then resolves wikilink /
// markdown-link targets to real hrefs against the vault. `rehypeKatex` renders
// the math nodes oymarkit emits as `span.math-inline` / `div.math-display`
// (KaTeX CSS is loaded in Base.astro).
//
// Note: `@astrojs/mdx` keeps its own MDX parser — only `.md` goes through
// oymarkit.
//
// `rehypeHeadingIds` is Astro's own plugin, and Astro runs it anyway — but only
// *after* the plugins listed here. `rehypeAutolinkHeadings` only touches
// headings that already carry an `id`, so it has to run second and we schedule
// the id pass ourselves. Astro's later pass then leaves those ids alone and
// just collects the heading list that `render()` hands to <Toc>.
//
// `content` is empty on purpose: that same later pass rebuilds each heading's
// TOC text from *all* its descendant text nodes, so a "#" added as a real node
// would also land in the outline. The visible "#" is drawn with CSS (see
// Base.astro), where — because "wrap" puts the heading's own text inside the
// anchor — the anchor must keep inheriting the heading's looks.
export default defineConfig({
  outDir: outputDir,
  // The vault's own `.oyster/public` when it has one, else the repo's `public/`
  // (see site-config.ts). Astro serves it at the site root and copies it into
  // the build, so a vault's favicon needs no plumbing of ours.
  publicDir,
  // Both caches default to `node_modules/`, which is read-only when installed
  // with Nix (see bin/cli.mjs). Keep them under `.astro/` with the rest of
  // Astro's generated state.
  cacheDir: "./.astro/cache",
  vite: { cacheDir: "./.astro/vite" },
  integrations: [mdx(), react()],
  markdown: {
    // Two themes so highlighted code follows the reader's colour scheme instead
    // of pasting a dark slab into a light page. `defaultColor: false` keeps
    // both palettes as `--shiki-light` / `--shiki-dark` custom properties
    // rather than writing one of them into an inline `color`, so Base.astro can
    // pick between them in CSS (and drop Shiki's backgrounds for the site's own
    // surface) without fighting inline styles.
    shikiConfig: {
      themes: { light: "github-light", dark: "github-dark" },
      defaultColor: false,
    },
    processor: unified({
      remarkPlugins: [remarkOymarkit, remarkOyWikilink],
      rehypePlugins: [
        rehypeKatex,
        rehypeHeadingIds,
        [
          rehypeAutolinkHeadings,
          {
            // Alternative: "append" puts the anchor after the heading text
            behavior: "wrap",
            content: [],
            properties: {
              className: ["heading-anchor"],
              ariaLabel: "Link to this section",
            },
          },
        ],
      ],
    }),
  },
});
