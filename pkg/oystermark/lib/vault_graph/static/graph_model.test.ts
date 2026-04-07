import { describe, expect, it } from "vitest";
import { sampleEdges, sampleNodes } from "./fixtures";
import {
	activeClusters,
	assignColors,
	buildAdjacency,
	buildFolderClusters,
	buildTagClusters,
	type Cluster,
	initialVisibleKeys,
} from "./graph_model";

// Snapshots lock in current behavior so the upcoming widget.ts
// refactor cannot silently change data derivations.

/** Compact cluster shape for readable snapshots. */
function summarize(cs: Cluster[]) {
	return cs.map((c) => ({
		key: c.key,
		label: c.label,
		color: c.color,
		nodes: c.nodes.map((n) => n.id),
	}));
}

const palette = ["#aaa", "#bbb", "#ccc", "#ddd"];

describe("buildAdjacency", () => {
	it("is undirected and covers every node", () => {
		const adj = buildAdjacency(sampleNodes, sampleEdges);
		const obj = Object.fromEntries(
			[...adj.entries()].map(([k, v]) => [k, [...v].sort()]),
		);
		expect(obj).toMatchInlineSnapshot(`
			{
			  "a/root.md": [
			    "a/sub/child.md",
			    "b/note.md",
			  ],
			  "a/sub/child.md": [
			    "a/root.md",
			    "a/sub/deeper/leaf.md",
			  ],
			  "a/sub/deeper/leaf.md": [
			    "a/sub/child.md",
			  ],
			  "b/note.md": [
			    "a/root.md",
			    "secret/hidden.md",
			  ],
			  "lonely.md": [],
			  "secret/hidden.md": [
			    "b/note.md",
			  ],
			}
		`);
	});
});

describe("assignColors", () => {
	it("cycles palette round-robin", () => {
		const m = assignColors(["x", "y", "z", "w", "v"], palette);
		expect(Object.fromEntries(m)).toMatchInlineSnapshot(`
			{
			  "v": "#aaa",
			  "w": "#ddd",
			  "x": "#aaa",
			  "y": "#bbb",
			  "z": "#ccc",
			}
		`);
	});
});

describe("buildFolderClusters", () => {
	const folders = [...new Set(sampleNodes.map((n) => n.folder))];
	const folderColor = assignColors(folders, palette);

	it('"all" selector: parent folders include nested descendants', () => {
		const cs = buildFolderClusters(sampleNodes, "all", folderColor);
		expect(summarize(cs)).toMatchInlineSnapshot(`
			[
			  {
			    "color": "#aaa",
			    "key": "folder:a",
			    "label": "a",
			    "nodes": [
			      "a/root.md",
			      "a/sub/child.md",
			      "a/sub/deeper/leaf.md",
			    ],
			  },
			  {
			    "color": "#bbb",
			    "key": "folder:a/sub",
			    "label": "a/sub",
			    "nodes": [
			      "a/sub/child.md",
			      "a/sub/deeper/leaf.md",
			    ],
			  },
			  {
			    "color": "#ccc",
			    "key": "folder:a/sub/deeper",
			    "label": "a/sub/deeper",
			    "nodes": [
			      "a/sub/deeper/leaf.md",
			    ],
			  },
			  {
			    "color": "#ddd",
			    "key": "folder:b",
			    "label": "b",
			    "nodes": [
			      "b/note.md",
			    ],
			  },
			  {
			    "color": "#aaa",
			    "key": "folder:secret",
			    "label": "secret",
			    "nodes": [
			      "secret/hidden.md",
			    ],
			  },
			  {
			    "color": "#bbb",
			    "key": "folder:.",
			    "label": ".",
			    "nodes": [
			      "lonely.md",
			    ],
			  },
			]
		`);
	});

	it("exclude selector drops matching folders", () => {
		const cs = buildFolderClusters(
			sampleNodes,
			{ exclude: ["secret"] },
			folderColor,
		);
		expect(cs.map((c) => c.label)).toMatchInlineSnapshot(`
			[
			  "a",
			  "a/sub",
			  "a/sub/deeper",
			  "b",
			  ".",
			]
		`);
	});
});

describe("buildTagClusters", () => {
	const tags = [...new Set(sampleNodes.flatMap((n) => n.tags || []))];
	const tagColor = assignColors(tags, palette);

	it('"all" selector: one cluster per tag, membership by tag list', () => {
		const cs = buildTagClusters(sampleNodes, "all", tagColor);
		expect(summarize(cs)).toMatchInlineSnapshot(`
			[
			  {
			    "color": "#aaa",
			    "key": "tag:alpha",
			    "label": "#alpha",
			    "nodes": [
			      "a/root.md",
			      "a/sub/child.md",
			    ],
			  },
			  {
			    "color": "#bbb",
			    "key": "tag:shared",
			    "label": "#shared",
			    "nodes": [
			      "a/root.md",
			      "a/sub/deeper/leaf.md",
			    ],
			  },
			  {
			    "color": "#ccc",
			    "key": "tag:beta",
			    "label": "#beta",
			    "nodes": [
			      "a/sub/deeper/leaf.md",
			      "b/note.md",
			    ],
			  },
			  {
			    "color": "#ddd",
			    "key": "tag:private",
			    "label": "#private",
			    "nodes": [
			      "secret/hidden.md",
			    ],
			  },
			]
		`);
	});

	it("include selector keeps only listed tags", () => {
		const cs = buildTagClusters(
			sampleNodes,
			{ include: ["alpha", "beta"] },
			tagColor,
		);
		expect(cs.map((c) => c.label)).toMatchInlineSnapshot(`
			[
			  "#alpha",
			  "#beta",
			]
		`);
	});
});

describe("initialVisibleKeys", () => {
	const folders = [...new Set(sampleNodes.map((n) => n.folder))];
	const tags = [...new Set(sampleNodes.flatMap((n) => n.tags || []))];
	const fc = buildFolderClusters(
		sampleNodes,
		"all",
		assignColors(folders, palette),
	);
	const tc = buildTagClusters(sampleNodes, "all", assignColors(tags, palette));

	it("seeds from defaults; tag defaults match bare names", () => {
		const keys = initialVisibleKeys(
			fc,
			tc,
			{ include: ["a", "b"] },
			{ include: ["alpha"] },
		);
		expect([...keys].sort()).toMatchInlineSnapshot(`
			[
			  "folder:a",
			  "folder:b",
			  "tag:alpha",
			]
		`);
	});

	it('"none" defaults → empty set', () => {
		const keys = initialVisibleKeys(fc, tc, "none", "none");
		expect(keys.size).toBe(0);
	});
});

describe("activeClusters", () => {
	const folders = [...new Set(sampleNodes.map((n) => n.folder))];
	const fc = buildFolderClusters(
		sampleNodes,
		"all",
		assignColors(folders, palette),
	);

	it("keeps visible clusters with >=2 nodes", () => {
		const visible = new Set([
			"folder:a", // 3 nodes — kept
			"folder:a/sub/deeper", // 1 node — dropped
			"folder:b", // 1 node — dropped
		]);
		const labels = activeClusters(fc, visible).map((c) => c.label);
		expect(labels).toMatchInlineSnapshot(`
			[
			  "a",
			]
		`);
	});
});
