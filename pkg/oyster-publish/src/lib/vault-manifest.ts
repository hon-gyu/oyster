import GithubSlugger from "github-slugger";
import { toString as mdToString } from "mdast-util-to-string";
import { SKIP, visit } from "unist-util-visit";
import { displayName } from "./display-name.ts";
import { stripObsidianComments } from "./obsidian-comments.ts";
import {
	buildOystermarkIndex,
	isQueryFailure,
	type OystermarkLink,
	type OystermarkNote,
	parseToMdast,
} from "./oystermark/index.ts";
import { buildPublicRoutes } from "./public-routes.ts";
import {
	defaultPublicationRules,
	initiallyPublished,
	type PublicationRules,
} from "./publication-selection.ts";
import type { SearchDoc, SearchHeading } from "./search-index.ts";
import type { AttachmentKind, VaultAttachment } from "./vault-attachments.ts";
import { queryFrontmatter } from "./vault-note-source.ts";
import {
	type LinkGraph,
	type NoteFile,
	parseDestination,
	pathToSlug,
} from "./wikilink.ts";

export interface NoteMetadata {
	title?: string;
	tags?: string[];
	publish?: boolean;
	date?: Date;
	updated?: Date;
	author?: string;
	status?: string;
	permalink?: string;
	aliases?: string[];
	navOrder?: number;
	navHidden?: boolean;
}

export interface ManifestInput {
	path: string;
	body: string;
	// The file as written, frontmatter included. Query links (`[[?id==foo]]`)
	// are matched against its frontmatter; `data` has only the keys the schema
	// knows.
	source?: string;
	data: NoteMetadata;
}

export interface NoteAnchor {
	id: string;
	kind: "heading" | "block" | "attribute";
	text: string;
	depth?: number;
	excerpt: string;
}

export interface NoteLink {
	target: string;
	fragment: string | null;
	embed: boolean;
	syntax: "wikilink" | "markdown";
	status?: "resolved" | "unpublished" | "missing" | "same-note";
	destinationPath?: string;
	destinationKind?: "note" | "attachment";
	mediaKind?: AttachmentKind;
	width?: number;
	height?: number;
	context: string;
	sourceFragment: string | null;
	resolution?: OystermarkLink["resolution"];
}

export interface NoteRecord extends NoteFile {
	originalSlug: string;
	title: string;
	tags: string[];
	headings: SearchHeading[];
	anchors: NoteAnchor[];
	excerpt: string;
	text: string;
	links: NoteLink[];
	date?: string;
	updated?: string;
	author?: string;
	status?: string;
	permalink?: string;
	aliases: string[];
	navOrder?: number;
	navHidden: boolean;
	published: boolean;
	routed: boolean;
}

export interface VaultManifest {
	// Every note in the source vault, including unpublished targets.
	allNotes: NoteRecord[];
	// Notes that have routes in this build and feed reader-facing indexes.
	notes: NoteRecord[];
	files: NoteFile[];
	graph: LinkGraph;
	slugToPath: Map<string, string>;
	titleByPath: Map<string, string>;
	tagsByPath: Map<string, string[]>;
	searchDocs: SearchDoc[];
	attachments: VaultAttachment[];
	reachableAttachments: VaultAttachment[];
}

function breadcrumbOf(path: string): string {
	const slash = path.lastIndexOf("/");
	return slash === -1 ? "" : path.slice(0, slash).split("/").join(" / ");
}

function isoDate(value: Date | undefined): string | undefined {
	return value?.toISOString();
}

function wikilinkFragment(fragment: any): string | null {
	if (fragment?.kind === "block") return `^${fragment.id}`;
	if (fragment?.kind === "heading") return fragment.path.join("#");
	return null;
}

function embedDimensions(node: any): { width?: number; height?: number } {
	const label = mdToString(node).trim();
	const match = /^(\d+)(?:x(\d+))?$/.exec(label);
	if (!match) return {};
	return {
		width: Number(match[1]),
		...(match[2] ? { height: Number(match[2]) } : {}),
	};
}

function parseNote(input: ManifestInput, indexed?: OystermarkNote): NoteRecord {
	const tree = (indexed?.mdast ?? parseToMdast(input.body)) as any;
	stripObsidianComments(tree);
	const slugger = new GithubSlugger();
	const headings: SearchHeading[] = [];
	const anchors: NoteAnchor[] = [];
	const links: NoteLink[] = [];
	const textParts: string[] = [];
	const anchorNodes = new Map<NoteAnchor, any>();
	let sourceFragment: string | null = null;

	// Records a link node. Shared with the heading pass below: heading children
	// are skipped by the main walk, but links inside a heading still count and
	// must land in document order to line up with the Oystermark index.
	const collectLink = (node: any, parent: any): void => {
		if (node.type === "link" && node.data?.oyWikilink) {
			const link = node.data.oyWikilink;
			links.push({
				target: link.target,
				fragment: wikilinkFragment(link.fragment),
				embed: Boolean(link.embed),
				syntax: "wikilink",
				context: mdToString(parent ?? node)
					.replace(/\s+/g, " ")
					.trim()
					.slice(0, 240),
				sourceFragment,
				...(link.embed ? embedDimensions(node) : {}),
			});
		} else if (node.type === "link" || node.type === "image") {
			const destination = parseDestination(node.url);
			if (destination) {
				links.push({
					...destination,
					embed: node.type === "image",
					syntax: "markdown",
					context: mdToString(parent ?? node)
						.replace(/\s+/g, " ")
						.trim()
						.slice(0, 240),
					sourceFragment,
				});
			}
		}
	};

	visit(tree, (node: any, _index, parent: any) => {
		if (node.type === "heading") {
			const text = mdToString(node);
			const id = slugger.slug(text);
			headings.push({ text, slug: id });
			sourceFragment = id;
			const anchor = {
				id,
				kind: "heading" as const,
				text,
				depth: node.depth,
				excerpt: "",
			};
			anchors.push(anchor);
			anchorNodes.set(anchor, node);
			visit(node, (child: any, _childIndex, childParent: any) => {
				if (child !== node) collectLink(child, childParent ?? node);
			});
			return SKIP;
		}

		const explicitId = node.data?.hProperties?.id;
		if (typeof explicitId === "string") {
			const classes = node.data?.hProperties?.className;
			const anchor = {
				id: explicitId,
				kind:
					Array.isArray(classes) && classes.includes("has-block-id")
						? "block"
						: "attribute",
				text: mdToString(node),
				excerpt: "",
			} as NoteAnchor;
			anchors.push(anchor);
			anchorNodes.set(anchor, node);
		}

		if (node.type === "text" || node.type === "inlineCode") {
			textParts.push(node.value);
		} else {
			collectLink(node, parent);
		}
	});

	const text = textParts.join(" ").replace(/\s+/g, " ").trim();
	for (const anchor of anchors) {
		const node = anchorNodes.get(anchor);
		if (anchor.kind === "heading") {
			const index = tree.children.indexOf(node);
			const parts: string[] = [];
			for (let cursor = index + 1; cursor < tree.children.length; cursor += 1) {
				const candidate = tree.children[cursor];
				if (candidate.type === "heading" && candidate.depth <= node.depth)
					break;
				const candidateText = mdToString(candidate).replace(/\s+/g, " ").trim();
				if (candidateText) parts.push(candidateText);
			}
			anchor.excerpt = parts.join(" ").slice(0, 240) || anchor.text;
		} else {
			anchor.excerpt = mdToString(node)
				.replace(/\s+/g, " ")
				.trim()
				.slice(0, 240);
		}
	}
	const data = input.data;
	if (indexed) {
		if (links.length !== indexed.links.length) {
			throw new Error(
				`Oystermark link count mismatch for ${input.path}: mdast=${links.length}, index=${indexed.links.length}`,
			);
		}
		links.forEach((link, index) => {
			link.resolution = indexed.links[index].resolution;
		});
	}
	return {
		path: input.path,
		slug: pathToSlug(input.path),
		originalSlug: pathToSlug(input.path),
		title: displayName(input.path, data.title),
		tags: [...(data.tags ?? [])],
		headings,
		anchors,
		excerpt: text.slice(0, 240),
		text,
		links,
		date: isoDate(data.date),
		updated: isoDate(data.updated),
		author: data.author,
		status: data.status,
		permalink: data.permalink,
		aliases: [...(data.aliases ?? [])],
		navOrder: data.navOrder,
		navHidden: data.navHidden ?? false,
		published: data.publish ?? false,
		routed: false,
	};
}

/** Build the deterministic, framework-free manifest from already loaded notes. */
export function buildVaultManifest(
	inputs: ManifestInput[],
	options: {
		publishedOnly?: boolean;
		attachments?: VaultAttachment[];
		publication?: PublicationRules;
	} = {},
): VaultManifest {
	const oystermark = buildOystermarkIndex(
		inputs.map((input) => ({
			path: input.path,
			content: input.body,
			frontmatter:
				input.source === undefined ? undefined : queryFrontmatter(input.source),
		})),
		(options.attachments ?? []).map((attachment) => attachment.path),
	);
	const indexedByPath = new Map(
		oystermark.notes.map((note) => [note.path, note]),
	);
	const allNotes = inputs
		.map((input) => parseNote(input, indexedByPath.get(input.path)))
		.sort((a, b) => a.path.localeCompare(b.path, "en"));
	const navOrders = new Map<string, string>();
	for (const note of allNotes) {
		if (note.navOrder === undefined) continue;
		const parent =
			note.originalSlug.slice(0, note.originalSlug.lastIndexOf("/")) || "/";
		const key = `${parent}\0${note.navOrder}`;
		const other = navOrders.get(key);
		if (other)
			throw new Error(
				`Duplicate navOrder ${note.navOrder} under ${parent}: ${other} and ${note.path}`,
			);
		navOrders.set(key, note.path);
	}
	const publicRoutes = buildPublicRoutes(
		allNotes.map((note) => ({
			path: note.path,
			original: note.slug,
			permalink: note.permalink,
			aliases: note.aliases,
		})),
	);
	const publicRouteByPath = new Map(
		publicRoutes.map((route) => [route.path, route]),
	);
	for (const note of allNotes) {
		note.originalSlug = note.slug;
		const route = publicRouteByPath.get(note.path);
		if (!route) throw new Error(`Missing public route for ${note.path}`);
		note.slug = route.canonical;
	}
	const inputByPath = new Map(inputs.map((input) => [input.path, input]));
	const rules = options.publication ?? defaultPublicationRules;
	const routedPaths = new Set(
		allNotes
			.filter((note) =>
				initiallyPublished(
					note.path,
					inputByPath.get(note.path)?.data.publish,
					rules,
					options.publishedOnly ?? false,
				),
			)
			.map((note) => note.path),
	);
	if (rules.linkedNotes && !options.publishedOnly) {
		let changed = true;
		while (changed) {
			changed = false;
			for (const note of allNotes) {
				if (!routedPaths.has(note.path)) continue;
				for (const link of note.links) {
					const resolution = link.resolution;
					const destination =
						resolution &&
						(resolution.kind === "note" || resolution.kind === "anchor")
							? { path: resolution.path }
							: null;
					const target =
						destination &&
						allNotes.find((candidate) => candidate.path === destination.path);
					if (
						target &&
						!routedPaths.has(target.path) &&
						inputByPath.get(target.path)?.data.publish !== false
					) {
						routedPaths.add(target.path);
						changed = true;
					}
				}
			}
		}
	}
	for (const note of allNotes) note.routed = routedPaths.has(note.path);
	const attachments = [...(options.attachments ?? [])].sort((a, b) =>
		a.path.localeCompare(b.path, "en"),
	);
	const attachmentByPath = new Map(
		attachments.map((attachment) => [attachment.path, attachment]),
	);
	const notes = allNotes.filter((note) => note.routed);
	const files = notes.map(({ path, slug }) => ({ path, slug }));

	for (const note of allNotes) {
		for (const link of note.links) {
			if (!link.target) {
				link.status = "same-note";
				link.destinationPath = note.path;
				continue;
			}
			const resolution = link.resolution;
			if (resolution?.kind === "note" || resolution?.kind === "anchor") {
				// A numeric wikilink alias is ordinary link text for notes; Obsidian's
				// width syntax applies only when the resolved target is media.
				delete link.width;
				delete link.height;
				link.destinationPath = resolution.path;
				link.destinationKind = "note";
				link.status = routedPaths.has(resolution.path)
					? "resolved"
					: "unpublished";
				continue;
			}
			if (resolution?.kind === "asset") {
				link.status = "resolved";
				link.destinationPath = resolution.path;
				link.destinationKind = "attachment";
				link.mediaKind = attachmentByPath.get(resolution.path)?.kind;
			} else if (
				resolution?.kind === "missing-anchor" &&
				indexedByPath.has(resolution.path)
			) {
				delete link.width;
				delete link.height;
				link.status = "missing";
				link.destinationPath = resolution.path;
				link.destinationKind = "note";
			} else {
				link.status = "missing";
			}
		}
	}
	const queryFailures = notes.flatMap((note) =>
		note.links.flatMap((link) =>
			link.resolution && isQueryFailure(link.resolution)
				? [
						`${note.path}: [[${link.target}${link.fragment ? `#${link.fragment}` : ""}]]: ${link.resolution.reason}`,
					]
				: [],
		),
	);
	if (queryFailures.length > 0)
		throw new Error(
			`Query links that do not resolve:\n  ${queryFailures.join("\n  ")}`,
		);
	for (const note of allNotes) {
		for (const link of note.links) delete link.resolution;
	}

	const outgoing = new Map(
		notes.map((note) => [
			note.path,
			[
				...new Set(
					note.links
						.filter(
							(link) =>
								link.status === "resolved" &&
								link.destinationKind === "note" &&
								link.destinationPath !== note.path,
						)
						.flatMap((link) =>
							link.destinationPath ? [link.destinationPath] : [],
						),
				),
			],
		]),
	);
	const backlinks = new Map(notes.map((note) => [note.path, [] as string[]]));
	for (const [source, targets] of outgoing) {
		for (const target of targets) backlinks.get(target)?.push(source);
	}
	const reachablePaths = new Set(
		notes
			.flatMap((note) => note.links)
			.filter((link) => link.destinationKind === "attachment")
			.flatMap((link) => (link.destinationPath ? [link.destinationPath] : [])),
	);

	return {
		allNotes,
		notes,
		files,
		graph: { forward: outgoing, backlinks },
		slugToPath: new Map(notes.map((note) => [note.slug, note.path])),
		titleByPath: new Map(notes.map((note) => [note.path, note.title])),
		tagsByPath: new Map(notes.map((note) => [note.path, note.tags])),
		searchDocs: notes.map((note) => ({
			title: note.title,
			href: note.slug,
			breadcrumb: breadcrumbOf(note.path),
			text: note.text,
			headings: note.headings,
			tags: note.tags,
		})),
		attachments,
		reachableAttachments: attachments.filter((attachment) =>
			reachablePaths.has(attachment.path),
		),
	};
}
