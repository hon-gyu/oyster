/**
 * Pure data derivations for the graph widget. No DOM, no d3, no
 * globals — everything here is a plain function of its inputs so it
 * can be covered by unit tests and reasoned about independently of
 * the visual layer.
 */

import { type Selector, selectorMatches } from "./selector";

export interface Cluster {
	key: string;
	kind: "folder" | "tag";
	label: string;
	color: string;
	nodes: GraphNode[];
}

// Adjacency
// ====================

/** Undirected neighbor map keyed by node id. */
export function buildAdjacency(
	nodes: GraphNode[],
	edges: GraphEdge[],
): Map<string, Set<string>> {
	const adj = new Map<string, Set<string>>();
	for (const n of nodes) adj.set(n.id, new Set());
	for (const e of edges) {
		const sid = typeof e.source === "string" ? e.source : e.source.id;
		const tid = typeof e.target === "string" ? e.target : e.target.id;
		adj.get(sid)!.add(tid);
		adj.get(tid)!.add(sid);
	}
	return adj;
}

// Colors
// ====================

/** Round-robin assignment of palette colors to distinct labels. */
export function assignColors(
	labels: string[],
	palette: string[],
): Map<string, string> {
	return new Map(labels.map((l, i) => [l, palette[i % palette.length]]));
}

// Clusters
// ====================

/**
 * Folder clusters: one per folder passing [dirSelector]. Membership
 * is recursive — a parent folder's cluster includes nodes from all
 * its subdirectories (so e.g. cluster [digital garden] covers
 * [digital garden/sub/note.md]).
 */
export function buildFolderClusters(
	nodes: GraphNode[],
	dirSelector: Selector,
	folderColor: Map<string, string>,
): Cluster[] {
	const folders = [...new Set(nodes.map((n) => n.folder))];
	return folders
		.filter((folder) => selectorMatches(dirSelector, folder))
		.map((folder) => ({
			key: `folder:${folder}`,
			kind: "folder" as const,
			label: folder,
			color: folderColor.get(folder)!,
			nodes: nodes.filter(
				(n) => n.folder === folder || n.folder.startsWith(`${folder}/`),
			),
		}));
}

/** Tag clusters: one per distinct tag passing [tagSelector]. */
export function buildTagClusters(
	nodes: GraphNode[],
	tagSelector: Selector,
	tagColor: Map<string, string>,
): Cluster[] {
	const allTags = [...new Set(nodes.flatMap((n) => n.tags || []))];
	return allTags
		.filter((tag) => selectorMatches(tagSelector, tag))
		.map((tag) => ({
			key: `tag:${tag}`,
			kind: "tag" as const,
			label: `#${tag}`,
			color: tagColor.get(tag)!,
			nodes: nodes.filter((n) => (n.tags || []).includes(tag)),
		}));
}

/**
 * Initial visibility: of the clusters that survived the dir/tag
 * filter, tick those matching [defaultDir] / [defaultTag]. Tag
 * defaults are matched against the bare tag name (no leading [#]).
 */
export function initialVisibleKeys(
	folderClusters: Cluster[],
	tagClusters: Cluster[],
	defaultDir: Selector,
	defaultTag: Selector,
): Set<string> {
	const keys = new Set<string>();
	for (const c of folderClusters) {
		if (selectorMatches(defaultDir, c.label)) keys.add(c.key);
	}
	for (const c of tagClusters) {
		const bare = c.label.replace(/^#/, "");
		if (selectorMatches(defaultTag, bare)) keys.add(c.key);
	}
	return keys;
}

/** Clusters currently rendered as hulls: visible and non-trivial. */
export function activeClusters(
	allClusters: Cluster[],
	visibleKeys: Set<string>,
): Cluster[] {
	return allClusters.filter(
		(c) => visibleKeys.has(c.key) && c.nodes.length >= 2,
	);
}
