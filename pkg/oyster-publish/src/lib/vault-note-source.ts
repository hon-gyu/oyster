import fs from "node:fs";
import path from "node:path";
import { CORE_SCHEMA, load as parseYaml, type Schema } from "js-yaml";
import type { NoteFile } from "./wikilink.ts";

export interface ParsedNoteSource {
	body: string;
	data: Record<string, unknown>;
}

export function parseNoteSource(
	source: string,
	schema?: Schema,
): ParsedNoteSource {
	const lines = source.split(/(?<=\n)/);
	if (lines[0]?.replace(/\r?\n$/, "") !== "---") {
		return { body: source, data: {} };
	}
	for (let index = 1; index < lines.length; index += 1) {
		if (lines[index].replace(/\r?\n$/, "") === "---") {
			const parsed = parseYaml(lines.slice(1, index).join(""), { schema });
			return {
				body: lines.slice(index + 1).join(""),
				data:
					parsed && typeof parsed === "object"
						? (parsed as Record<string, unknown>)
						: {},
			};
		}
	}
	return { body: source, data: {} };
}

// The frontmatter as Oystermark matches query links (`[[?id==foo]]`) against
// it. YAML's core schema leaves a date the string it was written as, the way
// libyaml reads it for the other Oystermark clients.
export function queryFrontmatter(source: string): Record<string, unknown> {
	return parseNoteSource(source, CORE_SCHEMA).data;
}

export function stripFrontmatter(source: string): string {
	return parseNoteSource(source).body;
}

export function readVaultNoteSources(
	vaultDir: string,
	files: NoteFile[],
	options: { publishedOnly?: boolean } = {},
): Map<string, string> {
	const sources = new Map<string, string>();
	for (const file of files) {
		const parsed = parseNoteSource(
			fs.readFileSync(path.join(vaultDir, file.path), "utf8"),
		);
		if (parsed.data.publish === false) continue;
		if (options.publishedOnly && parsed.data.publish !== true) continue;
		sources.set(file.path, parsed.body);
	}
	return sources;
}
