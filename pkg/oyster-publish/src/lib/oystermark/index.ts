import { createRequire } from "node:module";
import path from "node:path";

type Mdast = { type: "root"; children: unknown[] };

export type OystermarkResolution =
	| { kind: "note" | "asset"; path: string }
	| { kind: "anchor"; path: string; anchor: unknown }
	| { kind: "missing-path"; reason?: string }
	| { kind: "missing-anchor"; path: string; reason: string }
	| OystermarkQueryFailure;

// A query link (`[[?id==foo]]`) that does not resolve. Unlike a missing note,
// which may be one yet to be written, this is an authoring error.
export type OystermarkQueryFailure =
	| { kind: "invalid-query"; reason: string }
	| { kind: "no-match"; reason: string }
	| { kind: "ambiguous"; paths: string[]; reason: string };

export function isQueryFailure(
	resolution: OystermarkResolution,
): resolution is OystermarkQueryFailure {
	return (
		resolution.kind === "invalid-query" ||
		resolution.kind === "no-match" ||
		resolution.kind === "ambiguous"
	);
}

export interface OystermarkLink {
	reference: {
		target: string | null;
		fragment:
			| { kind: "hash-path"; path: string[] }
			| { kind: "caret-id"; id: string }
			| null;
	};
	kind: "link" | "embed";
	location: { firstByte: number; lastByte: number };
	resolution: OystermarkResolution;
}

export interface OystermarkNote {
	path: string;
	mdast: Mdast;
	anchors: unknown[];
	links: OystermarkLink[];
}

export interface OystermarkIndex {
	notes: OystermarkNote[];
	assets: string[];
}

type Boundary = {
	parse: (markdown: string) => string;
	index: (request: string) => string;
};

let boundary: Boundary | undefined;
const parsedJson = new Map<string, string>();

function load(): Boundary {
	if (boundary) return boundary;
	const global = globalThis as {
		oystermarkParse?: (markdown: string) => string;
		oystermarkIndex?: (request: string) => string;
	};
	if (!global.oystermarkParse || !global.oystermarkIndex) {
		const bundle = path.resolve("src/lib/oystermark/oystermark.cjs");
		createRequire(import.meta.url)(bundle);
	}
	if (!global.oystermarkParse || !global.oystermarkIndex) {
		throw new Error(
			"Oystermark failed to load. Rebuild it with `npm run build:parser`.",
		);
	}
	boundary = { parse: global.oystermarkParse, index: global.oystermarkIndex };
	return boundary;
}

export function parseToMdast(markdown: string): Mdast {
	let json = parsedJson.get(markdown);
	if (json === undefined) {
		json = load().parse(markdown);
		parsedJson.set(markdown, json);
	}
	return JSON.parse(json) as Mdast;
}

export function buildOystermarkIndex(
	// `content` without its frontmatter, which goes in `frontmatter` as parsed
	// data: the bundle cannot parse YAML.
	markdownFiles: { path: string; content: string; frontmatter?: unknown }[],
	otherFiles: string[],
	vaultRoot = ".",
): OystermarkIndex {
	return JSON.parse(
		load().index(JSON.stringify({ vaultRoot, markdownFiles, otherFiles })),
	) as OystermarkIndex;
}

function fragmentText(link: OystermarkLink): string | null {
	const fragment = link.reference.fragment;
	if (!fragment) return null;
	return fragment.kind === "caret-id"
		? `^${fragment.id}`
		: fragment.path.join("#");
}

export function resolveReference(
	index: OystermarkIndex,
	source: string,
	target: string,
	fragment: string | null,
): OystermarkResolution {
	const note = index.notes.find((candidate) => candidate.path === source);
	const link = note?.links.find(
		(candidate) =>
			(candidate.reference.target ?? "") === target &&
			fragmentText(candidate) === fragment,
	);
	return link?.resolution ?? { kind: "missing-path" };
}
