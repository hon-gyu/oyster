// Rendering of links in an Oystermark-produced mdast tree.
//
// oymarkit now *parses* wikilinks: `[[Note#H|disp]]` arrives as a `link` node
// carrying `data.oyWikilink = { target, fragment, embed, aliased }` (plus a raw
// `url`).
// The Oystermark index supplies canonical note, anchor, and asset resolution;
// this plugin maps those targets to publisher routes and HTML-oriented mdast.
import fs from "node:fs";
import path from "node:path";
import { displayName } from "../lib/display-name.ts";
import { stripObsidianComments } from "../lib/obsidian-comments.ts";
import {
	buildOystermarkIndex,
	type OystermarkIndex,
	parseToMdast,
	resolveReference,
} from "../lib/oystermark/index.ts";
import { vaultDir } from "../lib/project-paths.ts";
import { buildPublicRoutes } from "../lib/public-routes.ts";
import { publishedOnly } from "../lib/publish-policy.ts";
import { isolateEmbeddedContent, sliceContent } from "../lib/transclusion.ts";
import {
	scanVaultAttachments,
	type VaultAttachment,
} from "../lib/vault-attachments.ts";
import {
	parseNoteSource,
	queryFrontmatter,
	readVaultNoteSources,
	stripFrontmatter,
} from "../lib/vault-note-source.ts";
import {
	type NoteFile,
	parseDestination,
	pathToSlug,
} from "../lib/wikilink.ts";

function scanVault(dir: string, prefix = ""): NoteFile[] {
	const inputs: {
		path: string;
		original: string;
		permalink?: string;
		aliases?: string[];
	}[] = [];
	const walk = (current: string, currentPrefix = "") => {
		for (const entry of fs.readdirSync(current, { withFileTypes: true })) {
			if (entry.name.startsWith(".")) continue;
			const rel = currentPrefix ? `${currentPrefix}/${entry.name}` : entry.name;
			if (entry.isDirectory()) walk(path.join(current, entry.name), rel);
			else if (/\.(md|mdx)$/.test(entry.name)) {
				const data = parseNoteSource(
					fs.readFileSync(path.join(current, entry.name), "utf8"),
				).data;
				inputs.push({
					path: rel,
					original: pathToSlug(rel),
					permalink:
						typeof data.permalink === "string" ? data.permalink : undefined,
					aliases:
						Array.isArray(data.aliases) &&
						data.aliases.every((value) => typeof value === "string")
							? data.aliases
							: undefined,
				});
			}
		}
	};
	walk(dir, prefix);
	return buildPublicRoutes(inputs).map((route) => ({
		path: route.path,
		slug: route.canonical,
	}));
}

// GitHub-style heading anchor, matching how rehype slugs headings.
function slugifyHeading(text: string): string {
	return text
		.toLowerCase()
		.replace(/[^\w\s-]/g, "")
		.trim()
		.replace(/\s+/g, "-");
}

type OyWikilink = {
	target: string;
	fragment:
		| { kind: "heading"; path: string[] }
		| { kind: "block"; id: string }
		| null;
	embed: boolean;
	aliased?: boolean;
};

type Point = { line: number; column?: number; offset?: number };
type PositionedNode = {
	position?: { start?: Point; end?: Point };
	data?: { hProperties?: Record<string, any>; [key: string]: any };
	children?: PositionedNode[];
	[key: string]: any;
};

type Anchor = { line: number; offset?: number };
type TransformContext = {
	files: NoteFile[];
	allFiles: NoteFile[];
	attachments: VaultAttachment[];
	sources: Map<string, string>;
	nextEmbedId: number;
	maxDepth: number;
	index: OystermarkIndex;
};

function resolveNote(
	context: TransformContext,
	target: string,
	fragment: string | null,
	fromPath: string,
	files = context.files,
): NoteFile | null {
	const resolution = resolveReference(
		context.index,
		fromPath,
		target,
		fragment,
	);
	if (resolution.kind !== "note" && resolution.kind !== "anchor") return null;
	return files.find((file) => file.path === resolution.path) ?? null;
}

function textContent(node: PositionedNode): string {
	if (node.type === "text") return node.value ?? "";
	return (node.children ?? []).map(textContent).join("");
}

// Build the in-page namespace before resolving links. Heading ids follow the
// same slug rule used below; explicit ids (block ids and inline/block
// attributes) come straight from the parser's hProperties.
function collectAnchors(tree: PositionedNode): Map<string, Anchor> {
	const anchors = new Map<string, Anchor>();
	const visit = (node: PositionedNode) => {
		const start = node.position?.start;
		if (start) {
			const explicit = node.data?.hProperties?.id;
			if (typeof explicit === "string") {
				anchors.set(explicit.toLowerCase(), {
					line: start.line,
					offset: start.offset,
				});
			}
			if (node.type === "heading") {
				const id = slugifyHeading(textContent(node));
				if (id && !anchors.has(id))
					anchors.set(id, { line: start.line, offset: start.offset });
			}
		}
		for (const child of node.children ?? []) visit(child);
	};
	visit(tree);
	return anchors;
}

function directionLabel(node: PositionedNode, target: Anchor): string | null {
	const end = node.position?.end;
	if (!end) return null;
	const delta = target.line - end.line;
	if (delta > 0) return `↓${delta}`;
	if (delta < 0) return `↑${-delta}`;
	if (
		target.offset == null ||
		end.offset == null ||
		node.position?.start?.offset == null
	)
		return null;
	if (target.offset < node.position.start.offset) return "←";
	if (target.offset > end.offset) return "→";
	return null;
}

function directionTitle(label: string): string {
	if (label === "←") return "Target is to the left";
	if (label === "→") return "Target is to the right";
	const distance = Number(label.slice(1));
	const unit = distance === 1 ? "line" : "lines";
	return `Target is ${label.startsWith("↑") ? "above" : "below"} by ${distance} ${unit}`;
}

function uniqueClasses(classes: string[]): string[] {
	return [...new Set(classes)];
}

function resolveAttachment(
	context: TransformContext,
	target: string,
	fromPath: string,
): VaultAttachment | null {
	const resolution = resolveReference(context.index, fromPath, target, null);
	return resolution.kind === "asset"
		? (context.attachments.find(
				(attachment) => attachment.path === resolution.path,
			) ?? null)
		: null;
}

function mediaDimensions(node: any): Record<string, number> {
	const label = textContent(node).trim();
	const match = /^(\d+)(?:x(\d+))?$/.exec(label);
	return match
		? {
				width: Number(match[1]),
				...(match[2] ? { height: Number(match[2]) } : {}),
			}
		: {};
}

function renderAttachment(node: any, attachment: VaultAttachment): void {
	const dimensions = mediaDimensions(node);
	const label = textContent(node).trim();
	const title = /^\d+(?:x\d+)?$/.test(label) ? attachment.basename : label;
	if (attachment.kind === "image") {
		node.type = "image";
		node.url = attachment.url;
		node.alt = title || attachment.basename;
		node.data = {
			hProperties: {
				className: ["vault-attachment", "vault-image"],
				...dimensions,
			},
		};
		delete node.children;
		return;
	}
	if (attachment.kind === "audio" || attachment.kind === "video") {
		node.type = "oyElement";
		node.data = {
			hName: attachment.kind,
			hProperties: {
				className: ["vault-attachment", `vault-${attachment.kind}`],
				src: attachment.url,
				controls: true,
			},
		};
		node.children = [];
		delete node.url;
		return;
	}
	if (attachment.kind === "pdf") {
		node.type = "oyElement";
		node.data = {
			hName: "iframe",
			hProperties: {
				className: ["vault-attachment", "vault-pdf"],
				src: attachment.url,
				title: title || attachment.basename,
				loading: "lazy",
			},
		};
		node.children = [];
		delete node.url;
		return;
	}
	node.url = attachment.url;
	node.data.hProperties = {
		...(node.data?.hProperties ?? {}),
		className: ["vault-attachment", "vault-file"],
	};
}

function addDirection(
	node: PositionedNode,
	anchor: string,
	anchors: Map<string, Anchor>,
): void {
	const target = anchors.get(anchor.replace(/^#/, "").toLowerCase());
	if (!target) return;
	const label = directionLabel(node, target);
	if (!label) return;
	const properties = node.data?.hProperties ?? {};
	const classes = Array.isArray(properties.className)
		? properties.className
		: [];
	node.data = {
		...node.data,
		hProperties: {
			...properties,
			className: uniqueClasses([...classes, "link-direction"]),
			dataLinkDirection: label,
			title: properties.title ?? directionTitle(label),
		},
	};
}

// The in-page anchor for a wikilink fragment, or "" for none. A heading
// fragment slugs to the rehype heading id; a block ref `#^id` targets the block
// id that oymarkit emits on the paragraph (see cmarkit_mdast block_id handling).
function fragmentAnchor(fragment: OyWikilink["fragment"]): string {
	if (fragment && fragment.kind === "heading" && fragment.path.length > 0) {
		return `#${slugifyHeading(fragment.path[fragment.path.length - 1])}`;
	}
	if (fragment && fragment.kind === "block") {
		return `#${fragment.id}`;
	}
	return "";
}

function fragmentSelector(fragment: OyWikilink["fragment"]): string | null {
	if (fragment?.kind === "block") return `^${fragment.id}`;
	if (fragment?.kind === "heading") return fragment.path.join("#");
	return null;
}

function fallbackTransclusion(node: any, message: string): void {
	node.type = "oyElement";
	node.data = {
		hName: "aside",
		hProperties: { className: ["transclusion", "transclusion-fallback"] },
	};
	node.children = [
		{ type: "paragraph", children: [{ type: "text", value: message }] },
	];
}

function expandTransclusion(
	paragraph: any,
	context: TransformContext,
	fromPath: string,
	stack: string[],
	depth: number,
): boolean {
	if (paragraph.type !== "paragraph" || paragraph.children?.length !== 1)
		return false;
	const link = paragraph.children[0];
	const wl = link?.data?.oyWikilink as OyWikilink | undefined;
	if (!wl?.embed) return false;

	const destination = wl.target
		? resolveNote(context, wl.target, fragmentSelector(wl.fragment), fromPath)
		: (context.files.find((file) => file.path === fromPath) ?? null);
	if (!destination) return false; // Attachment embeds are handled later.

	const sameNoteAtTop = depth === 0 && destination.path === fromPath;
	if (stack.includes(destination.path) && !sameNoteAtTop) {
		fallbackTransclusion(paragraph, `Transclusion cycle: ${destination.path}`);
		return true;
	}
	if (depth >= context.maxDepth) {
		fallbackTransclusion(
			paragraph,
			`Transclusion depth limit: ${destination.path}`,
		);
		return true;
	}
	const source = context.sources.get(destination.path);
	const slice = source
		? sliceContent(parseToMdast(source) as any, fragmentSelector(wl.fragment))
		: null;
	if (!slice) {
		fallbackTransclusion(
			paragraph,
			`Transclusion target not found: ${destination.path}`,
		);
		return true;
	}

	const embeddedRoot: any = { type: "root", children: slice.nodes };
	transform(
		embeddedRoot,
		context,
		destination.path,
		collectAnchors(embeddedRoot),
		[...stack, destination.path],
		depth + 1,
	);
	const prefix = `embed-${context.nextEmbedId++}`;
	isolateEmbeddedContent(embeddedRoot.children, prefix, destination.slug);
	paragraph.type = "oyElement";
	paragraph.data = {
		hName: "aside",
		hProperties: {
			className: ["transclusion"],
			dataTransclusionSource: destination.path,
		},
	};
	paragraph.children = [
		...embeddedRoot.children,
		{
			type: "paragraph",
			data: { hProperties: { className: ["transclusion-source"] } },
			children: [
				{ type: "text", value: "From " },
				{
					type: "link",
					url: destination.slug,
					children: [{ type: "text", value: displayName(destination.path) }],
				},
			],
		},
	];
	return true;
}

// Resolve a `data.oyWikilink` node in place: point its url at the resolved
// slug (+ heading anchor), or mark it unresolved so broken links stay visible.
function resolveWikilink(
	node: any,
	context: TransformContext,
	fromPath: string,
	anchors: Map<string, Anchor>,
): void {
	const wl = node.data.oyWikilink as OyWikilink;
	const fragment = fragmentSelector(wl.fragment);
	const dest = wl.target
		? resolveNote(context, wl.target, fragment, fromPath)
		: null;
	const unpublished =
		!dest && wl.target
			? resolveNote(context, wl.target, fragment, fromPath, context.allFiles)
			: null;
	const prev = node.data?.hProperties ?? {};
	const prevClasses = Array.isArray(prev.className) ? prev.className : [];
	const classes: string[] = [...prevClasses, "wikilink"];
	if (wl.embed) classes.push("embed");
	const anchor = fragmentAnchor(wl.fragment);
	let sameNoteAnchor = "";
	if (dest) {
		node.url = dest.slug + anchor;
		// A query link shows the note it found, not the query.
		if (wl.target.startsWith("?") && !wl.aliased)
			node.children = [{ type: "text", value: displayName(dest.path) }];
		if (dest.path.toLowerCase() === fromPath.toLowerCase() && anchor) {
			sameNoteAnchor = anchor;
		}
	} else if (wl.target === "") {
		// Same-note fragment link, e.g. [[#Heading]].
		node.url = anchor || "#";
		sameNoteAnchor = anchor;
	} else {
		const attachment = resolveAttachment(context, wl.target, fromPath);
		if (attachment) {
			if (wl.embed) {
				renderAttachment(node, attachment);
				return;
			}
			node.url = attachment.url;
			classes.push("vault-attachment-link");
		} else if (path.posix.extname(wl.target)) {
			classes.push("attachment-missing", "wikilink-unresolved");
		} else {
			classes.push("wikilink-unresolved");
		}
	}
	node.data.hProperties = {
		...(node.data.hProperties ?? {}),
		className: uniqueClasses(classes),
		...(classes.includes("wikilink-unresolved")
			? {
					title: unpublished
						? `Unpublished note: ${wl.target}`
						: `Unresolved ${classes.includes("attachment-missing") ? "attachment" : "note"}: ${wl.target}`,
				}
			: {}),
	};
	if (sameNoteAnchor) addDirection(node, sameNoteAnchor, anchors);
}

// Rewrite a plain markdown link `[x](dest)`: internal targets get a resolved
// href, same-page `#fragment` links become in-page anchors, external links are
// left alone.
function rewriteLink(
	node: any,
	context: TransformContext,
	fromPath: string,
	anchors: Map<string, Anchor>,
): void {
	const dest = parseDestination(node.url);
	if (!dest) return; // external URL
	if (dest.target === "") {
		if (dest.fragment) {
			node.url = `#${slugifyHeading(dest.fragment)}`;
			addDirection(node, node.url, anchors);
		}
		return;
	}
	const resolved = resolveNote(context, dest.target, dest.fragment, fromPath);
	const unpublished = !resolved
		? resolveNote(
				context,
				dest.target,
				dest.fragment,
				fromPath,
				context.allFiles,
			)
		: null;
	const classes = resolved ? ["wikilink"] : ["wikilink", "wikilink-unresolved"];
	let sameNoteAnchor = "";
	if (resolved) {
		node.url =
			resolved.slug +
			(dest.fragment ? `#${slugifyHeading(dest.fragment)}` : "");
		if (
			resolved.path.toLowerCase() === fromPath.toLowerCase() &&
			dest.fragment
		) {
			sameNoteAnchor = `#${slugifyHeading(dest.fragment)}`;
		}
	}
	// Merge, don't clobber: the link may already carry hProperties from a djot
	// inline attribute, e.g. `[text](/u){.hl}`.
	const prev = node.data?.hProperties ?? {};
	const prevClasses = Array.isArray(prev.className) ? prev.className : [];
	node.data = {
		...node.data,
		hProperties: {
			...prev,
			className: uniqueClasses([...prevClasses, ...classes]),
			...(!resolved
				? {
						title: unpublished
							? `Unpublished note: ${dest.target}`
							: `Unresolved note: ${dest.target}`,
					}
				: {}),
		},
	};
	if (sameNoteAnchor) addDirection(node, sameNoteAnchor, anchors);
}

function rewriteImage(
	node: any,
	context: TransformContext,
	fromPath: string,
): void {
	let external: URL | null = null;
	try {
		external = new URL(node.url);
	} catch {
		/* Internal asset path. */
	}
	const allowedEmbed =
		external &&
		((external.hostname === "www.youtube-nocookie.com" &&
			external.pathname.startsWith("/embed/")) ||
			(external.hostname === "player.vimeo.com" &&
				external.pathname.startsWith("/video/")));
	if (allowedEmbed) {
		const title = node.alt || "Embedded video";
		node.type = "oyElement";
		node.data = {
			hName: "figure",
			hProperties: { className: ["external-embed"] },
		};
		node.children = [
			{
				type: "oyElement",
				data: {
					hName: "iframe",
					hProperties: {
						src: external?.href,
						title,
						loading: "lazy",
						sandbox: "allow-scripts allow-same-origin allow-presentation",
						allow: "fullscreen; picture-in-picture",
						referrerPolicy: "strict-origin-when-cross-origin",
					},
				},
				children: [],
			},
			{
				type: "paragraph",
				children: [
					{
						type: "link",
						url: external?.href,
						children: [{ type: "text", value: `Open ${title}` }],
					},
				],
			},
		];
		delete node.url;
		delete node.alt;
		return;
	}
	const destination = parseDestination(node.url);
	if (!destination?.target) return;
	const attachment = resolveAttachment(context, destination.target, fromPath);
	if (attachment) {
		node.url = attachment.url;
		node.data = {
			...(node.data ?? {}),
			hProperties: {
				...(node.data?.hProperties ?? {}),
				className: ["vault-attachment", "vault-image"],
			},
		};
	} else {
		node.data = {
			...(node.data ?? {}),
			hProperties: {
				...(node.data?.hProperties ?? {}),
				className: ["attachment-missing"],
			},
		};
	}
}

function transform(
	node: any,
	context: TransformContext,
	fromPath: string,
	anchors: Map<string, Anchor>,
	stack: string[],
	depth: number,
): void {
	if (!node.children) return;
	for (const child of node.children) {
		if (expandTransclusion(child, context, fromPath, stack, depth)) continue;
		if (child.type === "link" && child.data?.oyWikilink) {
			resolveWikilink(child, context, fromPath, anchors);
		} else if (child.type === "link") {
			rewriteLink(child, context, fromPath, anchors);
		} else if (child.type === "image") {
			rewriteImage(child, context, fromPath);
		} else if (child.type !== "code" && child.type !== "inlineCode") {
			transform(child, context, fromPath, anchors, stack, depth);
		}
	}
}

/** Resolve and annotate an already-parsed note. Exported for focused tests. */
export function transformOyWikilinks(
	tree: any,
	files: NoteFile[],
	fromPath: string,
	attachments: VaultAttachment[] = [],
	sources: Map<string, string> = new Map(),
	allFiles: NoteFile[] = files,
	index: OystermarkIndex,
): void {
	stripObsidianComments(tree);
	const context = {
		files,
		allFiles,
		attachments,
		sources,
		nextEmbedId: 1,
		maxDepth: 5,
		index,
	};
	transform(tree, context, fromPath, collectAnchors(tree), [fromPath], 0);
}

export default function remarkOyWikilink() {
	const allFiles = scanVault(vaultDir);
	const attachments = scanVaultAttachments(vaultDir);
	const sources = readVaultNoteSources(vaultDir, allFiles, { publishedOnly });
	const allSources = new Map(
		allFiles.map((file) => [
			file.path,
			fs.readFileSync(path.join(vaultDir, file.path), "utf8"),
		]),
	);
	const index = buildOystermarkIndex(
		[...allSources].map(([filePath, source]) => ({
			path: filePath,
			content: stripFrontmatter(source),
			frontmatter: queryFrontmatter(source),
		})),
		attachments.map((attachment) => attachment.path),
		vaultDir,
	);
	// The renderer must not create links or embeds to notes without routes in a
	// published-only build. The same source set drives both behaviors.
	const files = allFiles.filter((file) => sources.has(file.path));
	return (tree: any, file: any) => {
		const fromPath = file?.path ? path.relative(vaultDir, file.path) : "";
		transformOyWikilinks(
			tree,
			files,
			fromPath,
			attachments,
			sources,
			allFiles,
			index,
		);
	};
}
