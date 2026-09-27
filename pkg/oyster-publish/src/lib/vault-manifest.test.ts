import { expect, test } from "vitest";
import { attachmentRecord } from "./vault-attachments.ts";
import { buildVaultManifest, type ManifestInput } from "./vault-manifest.ts";

const notes: ManifestInput[] = [
	{
		path: "笔记/示例.md",
		body: [
			"# Repeated",
			"# Repeated",
			"",
			"{#block-id}",
			"A paragraph with an [ordinary link](Target.md#Details).",
			"",
			"Highlighted{#inline-anchor}",
			"",
			"![[Target#quote|300]]",
		].join("\n"),
		data: {
			title: "示例",
			tags: ["demo"],
			publish: true,
			date: new Date("2026-08-01T00:00:00Z"),
			updated: new Date("2026-08-02T00:00:00Z"),
			aliases: ["/Example"],
			permalink: "/example",
			navOrder: 2,
			navHidden: true,
		},
	},
	{
		path: "Target.md",
		body: "# Details\n\nTarget body.",
		data: {},
	},
];

test("manifest records headings, explicit anchors, links, excerpts, and metadata", () => {
	const manifest = buildVaultManifest(notes);
	const note = manifest.notes.find((entry) => entry.path === "笔记/示例.md")!;

	expect(note.headings).toEqual([
		{ text: "Repeated", slug: "repeated" },
		{ text: "Repeated", slug: "repeated-1" },
	]);
	expect(note.anchors).toEqual(
		expect.arrayContaining([
			expect.objectContaining({
				id: "repeated",
				kind: "heading",
				text: "Repeated",
				depth: 1,
			}),
			expect.objectContaining({
				id: "repeated-1",
				kind: "heading",
				text: "Repeated",
				depth: 1,
			}),
			expect.objectContaining({ id: "block-id", kind: "attribute" }),
			expect.objectContaining({ id: "inline-anchor", kind: "attribute" }),
		]),
	);
	expect(note.links).toEqual([
		{
			target: "Target.md",
			fragment: "Details",
			embed: false,
			syntax: "markdown",
			status: "resolved",
			destinationPath: "Target.md",
			destinationKind: "note",
			context: "A paragraph with an ordinary link.",
			sourceFragment: "repeated-1",
		},
		{
			target: "Target",
			fragment: "quote",
			embed: true,
			syntax: "wikilink",
			status: "missing",
			destinationPath: "Target.md",
			destinationKind: "note",
			context: "300",
			sourceFragment: "repeated-1",
		},
	]);
	expect(note.excerpt).toContain("A paragraph with an ordinary link");
	expect(note).toMatchObject({
		slug: "/example",
		originalSlug: "/笔记/示例",
		title: "示例",
		tags: ["demo"],
		date: "2026-08-01T00:00:00.000Z",
		updated: "2026-08-02T00:00:00.000Z",
		aliases: ["/Example"],
		permalink: "/example",
		navOrder: 2,
		navHidden: true,
	});
});

test("resolves attachment embeds with vault proximity and image dimensions", () => {
	const manifest = buildVaultManifest(
		[
			{
				path: "Folder/Current.md",
				body: "![[diagram.png|300x200]]\n\n![wide](../Shared/photo.webp)",
				data: { publish: true },
			},
		],
		{
			attachments: [
				attachmentRecord("Elsewhere/diagram.png"),
				attachmentRecord("Folder/diagram.png"),
				attachmentRecord("Shared/photo.webp"),
			],
		},
	);

	expect(manifest.notes[0].links[0]).toMatchObject({
		target: "diagram.png",
		embed: true,
		width: 300,
		height: 200,
		status: "resolved",
		destinationPath: "Folder/diagram.png",
		destinationKind: "attachment",
		mediaKind: "image",
	});
	expect(manifest.notes[0].links[1]).toMatchObject({
		target: "../Shared/photo.webp",
		status: "resolved",
		destinationPath: "Shared/photo.webp",
		destinationKind: "attachment",
	});
	expect(manifest.graph.forward.get("Folder/Current.md")).toEqual([]);
	expect(
		manifest.reachableAttachments.map((attachment) => attachment.path),
	).toEqual(["Folder/diagram.png", "Shared/photo.webp"]);
});

test("manifest output order and graph adjacency are deterministic", () => {
	const forward = buildVaultManifest(notes);
	const reverse = buildVaultManifest([...notes].reverse());

	expect(forward.notes.map((note) => note.path)).toEqual([
		"Target.md",
		"笔记/示例.md",
	]);
	expect(reverse.notes.map((note) => note.path)).toEqual(
		forward.notes.map((note) => note.path),
	);
	expect(reverse.graph.forward).toEqual(forward.graph.forward);
	expect(forward.graph.forward.get("笔记/示例.md")).toEqual(["Target.md"]);
	expect(forward.graph.backlinks.get("Target.md")).toEqual(["笔记/示例.md"]);
});

test("published-only mode retains private targets without routing or indexing them", () => {
	const manifest = buildVaultManifest(
		[
			{
				path: "Public.md",
				body: "[[Private]] and [[Missing]] and [[#Local]]",
				data: { publish: true },
			},
			{
				path: "Private.md",
				body: "Secret text.",
				data: { publish: false },
			},
		],
		{ publishedOnly: true },
	);

	expect(manifest.allNotes.map((note) => note.path)).toEqual([
		"Private.md",
		"Public.md",
	]);
	expect(manifest.notes.map((note) => note.path)).toEqual(["Public.md"]);
	expect(manifest.files).toEqual([{ path: "Public.md", slug: "/public" }]);
	expect(manifest.searchDocs.map((doc) => doc.href)).toEqual(["/public"]);
	expect(manifest.graph.forward.get("Public.md")).toEqual([]);
	expect(manifest.allNotes[1].links).toEqual([
		expect.objectContaining({
			target: "Private",
			status: "unpublished",
			destinationPath: "Private.md",
		}),
		expect.objectContaining({ target: "Missing", status: "missing" }),
		expect.objectContaining({
			target: "",
			status: "same-note",
			destinationPath: "Public.md",
		}),
	]);
});

test("explicit publish false is never routed, even outside published-only mode", () => {
	const manifest = buildVaultManifest([
		{ path: "Visible.md", body: "[[Private]]", data: {} },
		{ path: "Private.md", body: "Secret", data: { publish: false } },
	]);
	expect(manifest.notes.map((note) => note.path)).toEqual(["Visible.md"]);
	expect(manifest.allNotes).toHaveLength(2);
	expect(
		manifest.allNotes.find((note) => note.path === "Visible.md")?.links[0],
	).toMatchObject({ status: "unpublished", destinationPath: "Private.md" });
});

test("Obsidian comments never enter visible or searchable text", () => {
	const [note] = buildVaultManifest([
		{
			path: "comments.md",
			body: "Visible before. %% private editorial note %% Visible after.",
			data: {},
		},
	]).notes;
	expect(note.text).toContain("Visible before");
	expect(note.text).toContain("Visible after");
	expect(note.text).not.toContain("private editorial note");
});

test("folder selection can close over linked notes but never explicit private notes", () => {
	const manifest = buildVaultManifest(
		[
			{
				path: "blog/post.md",
				body: "[reference](../reference.md) [secret](../secret.md)",
				data: {},
			},
			{ path: "reference.md", body: "Reference", data: {} },
			{ path: "secret.md", body: "Secret", data: { publish: false } },
			{ path: "unlinked.md", body: "Unlinked", data: {} },
		],
		{
			publication: {
				includeFolders: ["blog"],
				excludeFolders: [],
				linkedNotes: true,
			},
		},
	);
	expect(manifest.notes.map((note) => note.path)).toEqual([
		"blog/post.md",
		"reference.md",
	]);
	expect(
		manifest.allNotes.find((note) => note.path === "blog/post.md")?.links,
	).toEqual(
		expect.arrayContaining([
			expect.objectContaining({
				target: "../reference.md",
				status: "resolved",
			}),
			expect.objectContaining({
				target: "../secret.md",
				status: "unpublished",
			}),
		]),
	);
});

test("links inside headings are collected in document order", () => {
	const manifest = buildVaultManifest([
		{
			path: "Source.md",
			body: [
				"Intro linking [[Target]].",
				"",
				"## Heading about [[Target]]",
				"",
				"Body linking [[Target]] again.",
			].join("\n"),
			data: { publish: true },
		},
		{ path: "Target.md", body: "Target body.", data: { publish: true } },
	]);
	const note = manifest.notes.find((entry) => entry.path === "Source.md")!;

	expect(note.links.map((link) => link.sourceFragment)).toEqual([
		null,
		"heading-about-target",
		"heading-about-target",
	]);
	expect(note.links.every((link) => link.status === "resolved")).toBe(true);
	// Heading text stays out of the note's plain text/excerpt.
	expect(note.text).not.toContain("Heading about");
});
