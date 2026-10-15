// Unit tests for wikilink parsing and resolution. Run with `npm test` (vitest).
// The resolution cases below mirror Obsidian's own `getFirstLinkpathDest`
// behaviour, probed directly against the desktop app.

import { expect, test } from "vitest";
import { parseDestination, pathToId, pathToSlug } from "./wikilink.ts";

// Wikilink *parsing* now lives in the oymarkit parser (tested on the OCaml
// side); this file covers only markdown-link destination parsing + resolution.

// == parseDestination (markdown links/images) ================================

test("dest: scheme-less destination is internal", () => {
	expect(parseDestination("async")).toEqual({
		target: "async",
		fragment: null,
	});
	expect(parseDestination("rust/async.md")).toEqual({
		target: "rust/async.md",
		fragment: null,
	});
});

test("dest: a leading `?` is a URL query, not a vault link", () => {
	expect(parseDestination("?id==foo")).toBeNull();
});

test("dest: fragment split and percent-decode", () => {
	expect(parseDestination("async#Futures")).toEqual({
		target: "async",
		fragment: "Futures",
	});
	expect(parseDestination("rust%2Fasync")?.target).toBe("rust/async");
});

test("dest: same-page fragment has empty target", () => {
	expect(parseDestination("#Section")).toEqual({
		target: "",
		fragment: "Section",
	});
});

test("dest: any URI scheme is external (null)", () => {
	expect(parseDestination("https://example.com")).toBe(null);
	expect(parseDestination("mailto:a@b.com")).toBe(null);
	expect(parseDestination("obsidian://open")).toBe(null);
	expect(parseDestination("//cdn.example.com/x")).toBe(null);
});

// == pathToId / pathToSlug ===================================================

// `pathToId` is what Astro's glob loader keys notes by (we hand it over as
// `generateId`), so these ids *are* the site's routes. A vault written by a
// human names files for humans — capitals, spaces, punctuation, dots, non-Latin
// scripts — while a sample vault tends to be tidily slug-shaped already and so
// exercises none of it. Get this wrong and every link into a note points at a
// URL that doesn't exist.

test("id: a slug-shaped path is already its own id", () => {
	expect(pathToId("rust/async.md")).toBe("rust/async");
});

test("id: every segment is slugified, not just the filename", () => {
	expect(pathToId("Field Notes/Growing Tomatoes.md")).toBe(
		"field-notes/growing-tomatoes",
	);
});

test("id: punctuation is dropped, inner dots collapse", () => {
	expect(pathToId("Field Notes/Soil pH (basics).md")).toBe(
		"field-notes/soil-ph-basics",
	);
	expect(pathToId("Field Notes/Notes on chapter 3.2.md")).toBe(
		"field-notes/notes-on-chapter-32",
	);
	expect(pathToId("Field Notes/Baker's Dozen.md")).toBe(
		"field-notes/bakers-dozen",
	);
});

test("id: non-Latin filenames survive", () => {
	expect(pathToId("笔记/示例.md")).toBe("笔记/示例");
});

test("id: an index file takes its directory's id", () => {
	expect(pathToId("Field Notes/index.md")).toBe("field-notes");
	expect(pathToId("index.md")).toBe("index");
});

test("slug: the id served as an absolute URL, root included", () => {
	expect(pathToSlug("Field Notes/Growing Tomatoes.md")).toBe(
		"/field-notes/growing-tomatoes",
	);
	expect(pathToSlug("Field Notes/index.md")).toBe("/field-notes");
	expect(pathToSlug("index.md")).toBe("/");
});

// The bug these cases exist to prevent: a note's URL and the href that links to
// it are now derived from one function, so they cannot disagree.
test("slug: a note's href is the route its own id produces", () => {
	for (const path of [
		"index.md",
		"Field Notes/Growing Tomatoes.md",
		"笔记/示例.md",
		"Field Notes/index.md",
	]) {
		const id = pathToId(path);
		const route = id === "index" ? "/" : `/${id}`;
		expect(pathToSlug(path)).toBe(route);
	}
});
