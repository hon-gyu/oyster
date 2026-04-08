/** Selector matching logic — imported by widget.ts and tested directly. */

export type Selector =
  | "all"
  | "none"
  | { include: string[] }
  | { exclude: string[] };

/** Test [label] against a [Selector]. */
export function selectorMatches(sel: Selector, label: string): boolean {
  if (sel === "all") return true;
  if (sel === "none") return false;
  if ("include" in sel) return sel.include.includes(label);
  if ("exclude" in sel) return !sel.exclude.includes(label);
  return true;
}
