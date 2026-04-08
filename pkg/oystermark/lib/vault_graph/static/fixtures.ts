/**
 * Hand-written fixture graph used by tests. Covers the tricky cases:
 * nested folders (parent cluster must include children recursively),
 * multi-tag nodes, cross-folder edges, an isolated node, and a
 * folder/tag that a selector can filter out.
 */

export const sampleNodes: GraphNode[] = [
	{
		id: "a/root.md",
		title: "A Root",
		tags: ["alpha", "shared"],
		folder: "a",
		href: "/a/root/",
	},
	{
		id: "a/sub/child.md",
		title: "A Sub Child",
		tags: ["alpha"],
		folder: "a/sub",
		href: "/a/sub/child/",
	},
	{
		id: "a/sub/deeper/leaf.md",
		title: "A Deep Leaf",
		tags: ["beta", "shared"],
		folder: "a/sub/deeper",
		href: "/a/sub/deeper/leaf/",
	},
	{
		id: "b/note.md",
		title: "B Note",
		tags: ["beta"],
		folder: "b",
		href: "/b/note/",
	},
	{
		id: "secret/hidden.md",
		title: "Secret",
		tags: ["private"],
		folder: "secret",
		href: "/secret/hidden/",
	},
	{
		id: "lonely.md",
		title: "Lonely",
		tags: [],
		folder: ".",
		href: "/lonely/",
	},
];

export const sampleEdges: GraphEdge[] = [
	{ source: "a/root.md", target: "a/sub/child.md" },
	{ source: "a/sub/child.md", target: "a/sub/deeper/leaf.md" },
	{ source: "a/root.md", target: "b/note.md" },
	{ source: "b/note.md", target: "secret/hidden.md" },
];
