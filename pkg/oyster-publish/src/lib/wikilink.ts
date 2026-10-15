// Publisher-owned path/route types and Markdown destination classification.
// Parsing and vault resolution belong to the Oystermark JavaScript boundary.

// The same slugifier Astro's content layer uses, so ids we generate look like
// the ones it would have generated itself.
import { slug as githubSlug } from "github-slugger";

// Types
// ====================

// One indexed note. `path` is the vault-relative file path (e.g. "rust/async.md");
// `slug` is its final URL path (e.g. "/rust/async"). Kept separate because
// resolution works on `path` but rendering needs `slug`.
export interface NoteFile {
	path: string;
	slug: string;
}

export interface LinkGraph {
	// note path -> paths of notes it links out to (resolved, deduped)
	forward: Map<string, string[]>;
	// note path -> paths of notes that link *to* it
	backlinks: Map<string, string[]>;
}

// Parsing
// ====================

// A regular markdown link `[x](dest)` or image `![x](dest)` is an *internal*
// link in Obsidian unless its destination is an external URL. Verified against
// the app: any URI scheme (http:, mailto:, obsidian:, …) or a protocol-relative
// `//host` is external; everything else resolves like a wikilink target.
//
// Returns null for external destinations. Otherwise gives the target (empty for
// a same-page `#fragment`) and its fragment, both percent-decoded.
export function parseDestination(
	dest: string,
): { target: string; fragment: string | null } | null {
	let decoded: string;
	try {
		decoded = decodeURIComponent(dest);
	} catch {
		decoded = dest; // malformed escape — use as-is
	}
	// A leading `?` is a URL query on the current page, not a vault link; only a
	// wikilink can be a query link.
	if (
		/^[a-z][a-z0-9+.-]*:/i.test(decoded) ||
		decoded.startsWith("//") ||
		decoded.startsWith("?")
	)
		return null;

	const hash = decoded.indexOf("#");
	const target = (hash === -1 ? decoded : decoded.slice(0, hash)).trim();
	const fragment = hash === -1 ? null : decoded.slice(hash + 1).trim();
	return { target, fragment: fragment || null };
}

// Resolution
// ====================
//
// Obsidian's link resolution, verified against its own
// `metadataCache.getFirstLinkpathDest`:
//   - Case-insensitive.
//   - A file extension is assumed to be `.md` when the link omits one.
//   - An exact full path from the vault root wins outright.
//   - Otherwise the link's path components must be a *suffix* of a file's path
//     components (a bare name matches by basename; a partial folder must really
//     be a trailing segment — arbitrary/interior matches do not count).
//   - Ties break toward the file closest to the linking note, then the shortest
//     path, then alphabetically.

// A vault-relative file path to the collection id Astro keys the note by. Every
// path segment is slugified, so a vault's real filenames ("Corner/Lambda
// Calculus.md") survive as URL-safe ids ("corner/lambda-calculus"). An `index`
// file stands in for its directory: "rust/index.md" -> "rust". The vault root's
// "index.md" keeps the id "index"; `idToSlug` maps that to "/".
//
// This is the *only* place a note's identity is derived. `published-glob.ts`
// hands it to Astro's glob loader as `generateId`, so the routes Astro serves
// and the hrefs we link with cannot drift apart. Anything that links into a
// note — wikilinks, graph nodes, search hits, backlinks — goes through here.
export function pathToId(relPath: string): string {
	return relPath
		.replace(/\.(md|mdx)$/, "")
		.split("/")
		.map((segment) => githubSlug(segment))
		.join("/")
		.replace(/\/index$/, "");
}

// The URL a note is served at: its id as an absolute path. "index.md" -> "/".
export function pathToSlug(relPath: string): string {
	const id = pathToId(relPath);
	return id === "index" ? "/" : `/${id}`;
}
