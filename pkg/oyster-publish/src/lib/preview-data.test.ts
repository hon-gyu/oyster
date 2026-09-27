import { expect, test } from "vitest";
import { previewPayload, previewUrlForPath } from "./preview-data.ts";
import { buildVaultManifest } from "./vault-manifest.ts";

test("builds a fragment-aware static payload without the full note body", () => {
	const note = buildVaultManifest([
		{
			path: "Field Notes/Example.md",
			body: "# Example\n\nIntro text.\n\n## Details\n\n{#context}\nFragment context.",
			data: { title: "An example", publish: true },
		},
	]).notes[0];
	const payload = previewPayload(note);

	expect(payload).toMatchObject({
		title: "An example",
		breadcrumb: "Field Notes",
		href: "/field-notes/example",
	});
	expect(payload.anchors.details).toEqual({
		label: "Details",
		excerpt: "Fragment context.",
	});
	expect(payload.anchors.context.excerpt).toContain("Fragment context");
	expect(payload.excerpt.length).toBeLessThanOrEqual(240);
});

test("preview URLs are stable and path-sensitive", () => {
	expect(previewUrlForPath("a/Note.md")).toBe(previewUrlForPath("a/Note.md"));
	expect(previewUrlForPath("a/Note.md")).not.toBe(
		previewUrlForPath("b/Note.md"),
	);
	expect(previewUrlForPath("a/Note.md")).toMatch(
		/^\/oyster-previews\/[a-f0-9]{16}\.json$/,
	);
});
