import fs from "node:fs";
import path from "node:path";
import type { Loader } from "astro/loaders";
import { glob } from "astro/loaders";
import { notePath, vaultDir } from "./project-paths.ts";
import type { PublicationRules } from "./publication-selection.ts";
import { defaultPublicationRules } from "./publication-selection.ts";
import { publishedOnly } from "./publish-policy";
import { buildVaultManifest } from "./vault-manifest.ts";
import { pathToId } from "./wikilink";

type GlobOptions = Parameters<typeof glob>[0];

// Let Astro's loader parse and schema-validate every entry, then apply the
// optional publication policy to the resulting collection data.
//
// `generateId` is ours on purpose. Astro's default derives an id from the file
// path with a rule of its own, while every link *into* a note (wikilinks, graph
// nodes, search hits, backlinks) derives its href from `pathToId`. Two rules for
// one identity is a bug waiting to happen — and was one: on a vault whose
// filenames aren't already slug-shaped, the two disagreed and every internal
// link 404'd. Handing Astro our rule leaves exactly one.
//
// Note this ignores a `slug:` in frontmatter, which Astro's default would honour
// but a path-only function cannot see. Path is the single source of truth.
export function publishedGlob(
	options: GlobOptions,
	publication: PublicationRules = defaultPublicationRules,
): Loader {
	const loader = glob({
		generateId: ({ entry }) => pathToId(entry),
		...options,
	});
	return {
		...loader,
		name: "published-glob-loader",
		async load(context) {
			await loader.load(context);
			const entries = [...context.store.entries()];
			const manifest = buildVaultManifest(
				entries.map(([, entry]) => ({
					path: notePath(entry),
					body: (entry as any).body ?? "",
					source: fs.readFileSync(path.join(vaultDir, notePath(entry)), "utf8"),
					data: entry.data,
				})),
				{ publishedOnly, publication },
			);
			const routed = new Set(manifest.notes.map((note) => note.path));
			for (const [id, entry] of entries) {
				if (!routed.has(notePath(entry))) context.store.delete(id);
			}
		},
	};
}
