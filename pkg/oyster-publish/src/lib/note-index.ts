// Bridges Astro's content collection to the plain `NoteFile[]` / link graph
// that lib/wikilink.ts works on. This is the only module here that imports from
// Astro, which keeps wikilink.ts framework-free and testable.

import { getCollection } from "astro:content";
import fs from "node:fs";
import path from "node:path";
import {
	type ContextualBacklink,
	contextualBacklinks,
} from "./contextual-backlinks.ts";
import { buildGraphData, type GraphData } from "./graph-data.ts";
import { notePath } from "./project-paths";
import { vaultDir } from "./project-paths.ts";
import { publishedOnly } from "./publish-policy.ts";
import { site } from "./site-config.ts";
import { scanVaultAttachments } from "./vault-attachments.ts";
import { buildVaultManifest, type VaultManifest } from "./vault-manifest.ts";

export type VaultIndex = VaultManifest;

// A backlink as the UI wants it: where to link, and what to call it.
export type Backlink = ContextualBacklink;

// A breadcrumb for a note's containing folder: "Modern AI/agents.md" ->
// "Modern AI", "a/b/note.md" -> "a / b", a root note -> "". Folder names are
// shown exactly as they are on disk (see the display rule in tree.ts). An index
// note belongs to its own directory, matching how it stands in elsewhere.
// Build the whole vault index once. `getCollection` already did the filesystem
// walk and frontmatter parsing; we read each note's raw `body` to extract its
// links, then build the link graph.
let vaultIndexPromise: Promise<VaultIndex> | undefined;

async function buildIndex(): Promise<VaultIndex> {
	const notes = await getCollection("vaultNotes");

	return buildVaultManifest(
		notes.map((note) => ({
			path: notePath(note),
			body: note.body ?? "",
			source: fs.readFileSync(path.join(vaultDir, notePath(note)), "utf8"),
			data: note.data,
		})),
		{
			publishedOnly,
			publication: site.publication,
			attachments: scanVaultAttachments(vaultDir),
		},
	);
}

// Astro components request the manifest independently during a build. Share
// one promise so the oymarkit parse happens exactly once per loaded note.
export function getVaultIndex(): Promise<VaultIndex> {
	if (!vaultIndexPromise) vaultIndexPromise = buildIndex();
	return vaultIndexPromise;
}

// The whole-vault graph payload for the client widget, derived from the index.
export function graphDataFor(index: VaultIndex): GraphData {
	return buildGraphData(
		index.files,
		index.graph,
		index.titleByPath,
		index.tagsByPath,
	);
}

// The backlinks ("linked mentions") for the note at a given URL path, resolved
// to link targets the UI can render.
export function backlinksFor(index: VaultIndex, slug: string): Backlink[] {
	const path = index.slugToPath.get(slug);
	if (!path) return [];
	return contextualBacklinks(index.notes, path);
}
