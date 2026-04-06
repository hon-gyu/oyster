/**
 * Wire format for the graph-view config.
 *
 * Mirrors OCaml [Config.Home_graph_view.t]. The OCaml side serializes
 * via [yojson_of_t] and inlines the result as `window.__graphConfig`.
 *
 * THIS FILE IS THE CONTRACT between OCaml and the browser widget.
 * If you change [Config.Home_graph_view.t] or [Config.Selector.t], the
 * `Home_graph_view wire format` expect-test in OCaml will fail — update
 * BOTH this file and the expect-test in the same commit.
 *
 * No `export` / `import` here so this stays an ambient declaration file
 * compatible with `module: "none"` + `outFile`.
 */

/** Mirrors [Config.Selector.t]. */
type Selector =
  | "all"
  | "none"
  | { include: string[] }
  | { exclude: string[] };

interface HomeGraphViewConfig {
  /** Which folder clusters appear in the panel. */
  dir: Selector;
  /** Which tag clusters appear in the panel. */
  tag: Selector;
  /** Of the visible folder clusters, which are ticked on initial load. */
  default_dir: Selector;
  /** Of the visible tag clusters, which are ticked on initial load. */
  default_tag: Selector;
}

interface GraphNode {
  id: string;
  title: string;
  tags: string[];
  folder: string;
  // Mutated by d3.forceSimulation
  x?: number;
  y?: number;
  vx?: number;
  vy?: number;
  fx?: number | null;
  fy?: number | null;
}

interface GraphEdge {
  source: string | GraphNode;
  target: string | GraphNode;
}

interface GraphData {
  nodes: GraphNode[];
  edges: GraphEdge[];
}

interface Window {
  __graphData: GraphData;
  __graphConfig?: HomeGraphViewConfig;
}

// d3 is loaded via a separate <script> tag in the embed; treat as ambient.
declare const d3: any;
