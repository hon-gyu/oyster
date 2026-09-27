import { describe, expect, test } from "vitest";
import { buildOystermarkIndex, parseToMdast } from "../lib/oystermark/index.ts";
import { attachmentRecord } from "../lib/vault-attachments.ts";
import type { NoteFile } from "../lib/wikilink.ts";
import { transformOyWikilinks } from "./remark-oywikilink.ts";

const files: NoteFile[] = [
	{ path: "notes/current.md", slug: "/notes/current" },
	{ path: "notes/other.md", slug: "/notes/other" },
];

function indexFor(
	markdown: string,
	fromPath: string,
	notes: NoteFile[],
	attachments: ReturnType<typeof attachmentRecord>[] = [],
	sources: Map<string, string> = new Map(),
) {
	return buildOystermarkIndex(
		notes.map((note) => ({
			path: note.path,
			content:
				note.path === fromPath ? markdown : (sources.get(note.path) ?? ""),
		})),
		attachments.map((attachment) => attachment.path),
	);
}

function links(markdown: string): any[] {
	const tree = parseToMdast(markdown);
	transformOyWikilinks(
		tree,
		files,
		"notes/current.md",
		[],
		new Map(),
		files,
		indexFor(markdown, "notes/current.md", files),
	);
	const found: any[] = [];
	const visit = (node: any) => {
		if (node.type === "link") found.push(node);
		for (const child of node.children ?? []) visit(child);
	};
	visit(tree);
	return found;
}

function direction(link: any): string | undefined {
	return link.data?.hProperties?.dataLinkDirection;
}

describe("rendered intra-note link directions", () => {
	test("shows line distance above and below for wikilinks", () => {
		const [below, above] = links(
			"# Top\n\n[[#Bottom]]\n\n## Bottom\n\n[[#Top]]\n",
		);
		expect(direction(below)).toBe("↓2");
		expect(direction(above)).toBe("↑6");
		expect(below.data.hProperties.className).toContain("link-direction");
		expect(above.data.hProperties.title).toBe("Target is above by 6 lines");
	});

	test("supports markdown links and same-line attribute anchors", () => {
		const [left, above] = links(
			"# Top\n\n[key]{#term} then [[#term]]\n\n[back](#Top)\n",
		);
		expect(direction(left)).toBe("←");
		expect(direction(above)).toBe("↑4");
	});

	test("supports the current note written through its filename", () => {
		const [link] = links("# Top\n\n[[current#Top]]\n");
		expect(direction(link)).toBe("↑2");
	});

	test("leaves cross-note and unresolved fragments bare", () => {
		const [crossNote, missing] = links(
			"# Top\n\n[[other#Top]] and [[#Missing]]\n",
		);
		expect(direction(crossNote)).toBeUndefined();
		expect(direction(missing)).toBeUndefined();
	});

	test("distinguishes an unpublished target from a missing note", () => {
		const tree: any = parseToMdast("[[other]] and [[absent]]");
		transformOyWikilinks(
			tree,
			[files[0]],
			"notes/current.md",
			[],
			new Map(),
			files,
			indexFor("[[other]] and [[absent]]", "notes/current.md", files),
		);
		const [unpublished, , missing] = tree.children[0].children;
		expect(unpublished.data.hProperties.title).toBe("Unpublished note: other");
		expect(missing.data.hProperties.title).toBe("Unresolved note: absent");
	});
});

describe("vault attachment rendering", () => {
	const attachments = [
		attachmentRecord("notes/image one.svg"),
		attachmentRecord("notes/audio.mp3"),
		attachmentRecord("notes/video.webm"),
		attachmentRecord("notes/document.pdf"),
	];

	function transformed(markdown: string): any[] {
		const tree: any = parseToMdast(markdown);
		transformOyWikilinks(
			tree,
			files,
			"notes/current.md",
			attachments,
			new Map(),
			files,
			indexFor(markdown, "notes/current.md", files, attachments),
		);
		return tree.children.map((paragraph: any) => paragraph.children[0]);
	}

	test("renders image dimensions and rewrites relative Markdown images", () => {
		const [embed, markdown] = transformed(
			"![[image one.svg|300x180]]\n\n![Alt](./image%20one.svg)",
		);
		expect(embed).toMatchObject({
			type: "image",
			url: attachments[0].url,
			alt: "image one.svg",
			data: { hProperties: { width: 300, height: 180 } },
		});
		expect(markdown).toMatchObject({
			type: "image",
			url: attachments[0].url,
			alt: "Alt",
		});
	});

	test("renders audio, video, and PDFs with native elements", () => {
		const [audio, video, pdf] = transformed(
			"![[audio.mp3]]\n\n![[video.webm]]\n\n![[document.pdf]]",
		);
		expect(audio).toMatchObject({
			type: "oyElement",
			data: {
				hName: "audio",
				hProperties: { src: attachments[1].url, controls: true },
			},
		});
		expect(video).toMatchObject({
			type: "oyElement",
			data: {
				hName: "video",
				hProperties: { src: attachments[2].url, controls: true },
			},
		});
		expect(pdf).toMatchObject({
			type: "oyElement",
			data: { hName: "iframe", hProperties: { src: attachments[3].url } },
		});
	});

	test("marks a missing file separately from a missing note", () => {
		const tree: any = parseToMdast("![[missing.png]] and [[missing-note]]");
		transformOyWikilinks(
			tree,
			files,
			"notes/current.md",
			attachments,
			new Map(),
			files,
			indexFor(
				"![[missing.png]] and [[missing-note]]",
				"notes/current.md",
				files,
				attachments,
			),
		);
		const [missingFile, , missingNote] = tree.children[0].children;
		expect(missingFile.data.hProperties.className).toContain(
			"attachment-missing",
		);
		expect(missingFile.data.hProperties.title).toBe(
			"Unresolved attachment: missing.png",
		);
		expect(missingNote.data.hProperties.className).not.toContain(
			"attachment-missing",
		);
		expect(missingNote.data.hProperties.title).toBe(
			"Unresolved note: missing-note",
		);
	});

	test("permits only explicit privacy-enhanced video embed URLs", () => {
		const tree: any = parseToMdast(
			"![Demo](https://www.youtube-nocookie.com/embed/abc123)\n\n![Image](https://example.com/file.png)",
		);
		transformOyWikilinks(
			tree,
			files,
			"notes/current.md",
			attachments,
			new Map(),
			files,
			indexFor(
				"![Demo](https://www.youtube-nocookie.com/embed/abc123)\n\n![Image](https://example.com/file.png)",
				"notes/current.md",
				files,
				attachments,
			),
		);
		const embed = tree.children[0].children[0];
		expect(embed).toMatchObject({
			type: "oyElement",
			data: { hName: "figure", hProperties: { className: ["external-embed"] } },
		});
		expect(embed.children[0]).toMatchObject({
			data: {
				hName: "iframe",
				hProperties: {
					src: "https://www.youtube-nocookie.com/embed/abc123",
					sandbox: "allow-scripts allow-same-origin allow-presentation",
				},
			},
		});
		expect(tree.children[1].children[0]).toMatchObject({
			type: "image",
			url: "https://example.com/file.png",
		});
	});
});

describe("note transclusion", () => {
	const transclusionFiles: NoteFile[] = [
		{ path: "notes/current.md", slug: "/notes/current" },
		{ path: "notes/other.md", slug: "/notes/other" },
		{ path: "notes/nested.md", slug: "/notes/nested" },
	];
	const sources = new Map([
		["notes/current.md", "# Current\n\n![[other#Section]]"],
		[
			"notes/other.md",
			"# Other\n\n## Section\n\n{#selected}\nSelected.\n\n![[nested]]\n\n## Later\n\nExcluded.",
		],
		["notes/nested.md", "# Nested\n\nNested body.\n\n![[current]]"],
	]);

	function transclude(
		markdown: string,
		fromPath = "notes/current.md",
		noteSources = sources,
	): any {
		const tree: any = parseToMdast(markdown);
		transformOyWikilinks(
			tree,
			transclusionFiles,
			fromPath,
			[],
			noteSources,
			transclusionFiles,
			indexFor(markdown, fromPath, transclusionFiles, [], noteSources),
		);
		return tree;
	}

	test("embeds a heading section with attribution and adjusted isolated ids", () => {
		const tree = transclude("![[other#Section]]");
		const embed = tree.children[0];
		expect(embed).toMatchObject({
			type: "oyElement",
			data: {
				hName: "aside",
				hProperties: {
					className: ["transclusion"],
					dataTransclusionSource: "notes/other.md",
				},
			},
		});
		expect(embed.children[0]).toMatchObject({
			type: "heading",
			depth: 2,
			data: { hProperties: { id: "embed-2-section" } },
		});
		expect(
			embed.children.some(
				(node: any) =>
					node.type === "heading" && node.children?.[0]?.value === "Later",
			),
		).toBe(false);
		expect(embed.children.at(-1)).toMatchObject({
			data: { hProperties: { className: ["transclusion-source"] } },
			children: [{ value: "From " }, { url: "/notes/other" }],
		});
	});

	test("embeds same-note blocks and isolates their DOM ids", () => {
		const noteSources = new Map(sources);
		noteSources.set("notes/current.md", "# Current\n\n{#local}\nLocal block.");
		const tree = transclude("![[#local]]", "notes/current.md", noteSources);
		expect(tree.children[0].children[0].data.hProperties.id).toBe(
			"embed-1-local",
		);
	});

	test("renders an explicit fallback for recursive cycles", () => {
		const tree = transclude("![[other]]");
		const fallbacks: any[] = [];
		const visit = (node: any) => {
			if (
				node.data?.hProperties?.className?.includes("transclusion-fallback")
			) {
				fallbacks.push(node);
			}
			for (const child of node.children ?? []) visit(child);
		};
		visit(tree);
		expect(fallbacks).toHaveLength(1);
		expect(fallbacks[0].children[0].children[0].value).toContain("cycle");
	});

	test("stops recursive embeds at the explicit maximum depth", () => {
		const chainFiles = [
			{ path: "current.md", slug: "/current" },
			...Array.from({ length: 7 }, (_, index) => ({
				path: `n${index}.md`,
				slug: `/n${index}`,
			})),
		];
		const chainSources = new Map(
			Array.from({ length: 7 }, (_, index) => [
				`n${index}.md`,
				index === 6 ? "End." : `![[n${index + 1}]]`,
			]),
		);
		const tree: any = parseToMdast("![[n0]]");
		transformOyWikilinks(
			tree,
			chainFiles,
			"current.md",
			[],
			chainSources,
			chainFiles,
			indexFor("![[n0]]", "current.md", chainFiles, [], chainSources),
		);
		const text = JSON.stringify(tree);
		expect(text).toContain("Transclusion depth limit: n5.md");
		expect(text).not.toContain("End.");
	});
});
