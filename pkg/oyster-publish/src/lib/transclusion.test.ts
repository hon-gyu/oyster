import { expect, test } from "vitest";
import { parseToMdast } from "./oystermark/index.ts";
import { isolateEmbeddedContent, sliceContent } from "./transclusion.ts";

const tree: any = parseToMdast(
	[
		"# Intro",
		"",
		"Opening.",
		"",
		"## Repeated",
		"",
		"First section.",
		"",
		"## Repeated",
		"",
		"{#second-block}",
		"Second section.",
		"",
		"Anchored{#inline-anchor}",
		"",
		"# End",
	].join("\n"),
);

test("slices a whole note without sharing mutable nodes", () => {
	const slice = sliceContent(tree, null)!;
	expect(slice.nodes).toHaveLength(tree.children.length);
	const firstChild = slice.nodes[0].children?.[0];
	if (!firstChild)
		throw new Error("Expected the sliced heading to have a child");
	firstChild.value = "Changed";
	expect(tree.children[0].children[0].value).toBe("Intro");
});

test("slices a duplicate heading by its rendered slug through the next peer", () => {
	const slice = sliceContent(tree, "repeated-1")!;
	expect(slice.label).toBe("Repeated");
	expect(slice.nodes.map((node) => node.type)).toEqual([
		"heading",
		"paragraph",
		"paragraph",
	]);
	expect(slice.nodes[1].data?.hProperties?.id).toBe("second-block");
});

test("slices block and inline attribute anchors", () => {
	expect(sliceContent(tree, "second-block")?.nodes[0].type).toBe("paragraph");
	expect(sliceContent(tree, "inline-anchor")?.nodes[0].type).toBe("oyElement");
	expect(sliceContent(tree, "missing")).toBeNull();
});

test("isolates headings, explicit ids, links, and footnote identifiers", () => {
	const nodes: any[] = [
		{ type: "heading", depth: 1, children: [{ type: "text", value: "Title" }] },
		{ type: "paragraph", data: { hProperties: { id: "block" } }, children: [] },
		{ type: "link", url: "/source#title", children: [] },
		{ type: "footnoteReference", identifier: "one", label: "one" },
	];
	isolateEmbeddedContent(nodes, "embed-2", "/source");
	expect(nodes[0]).toMatchObject({
		depth: 2,
		data: { hProperties: { id: "embed-2-title" } },
	});
	expect(nodes[1].data.hProperties.id).toBe("embed-2-block");
	expect(nodes[2].url).toBe("#embed-2-title");
	expect(nodes[3]).toMatchObject({
		identifier: "embed-2-one",
		label: "embed-2-one",
	});
});
