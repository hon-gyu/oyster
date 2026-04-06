/**
 * Graph view using D3.js.
 *
 * Preconditions:
 *  - graph JSON data is in `window.__graphData`
 *  - optional `window.__graphConfig` of type [HomeGraphViewConfig]
 *    (see config.d.ts — that file is the wire-format contract with OCaml)
 */

import { selectorMatches } from "./selector";

// Home_graph_view config
// ====================
// Each of dir/tag/default_dir/default_tag is a [Selector]:
//   "all"                   -> include everything
//   "none"                  -> exclude everything
//   { include: [..labels] } -> only these
//   { exclude: [..labels] } -> everything except these
//
// [dir]/[tag] decide which folder/tag clusters appear in the panel at
// all. [default_dir]/[default_tag] decide which of the visible ones are
// ticked on initial load.
const graphConfig: HomeGraphViewConfig = window.__graphConfig ?? {
	dir: "all",
	tag: "all",
	default_dir: { include: ["*"] },
	default_tag: { include: ["*"] },
};

// Global config
// ====================
// Tunables for layout tightness. Lower link distance + stronger (less
// negative) charge + smaller seed radius all push nodes closer together,
// which means fit-to-screen zooms in further and labels stay legible.
const config = {
	// Visual
	nodeRadius: 14, // circle radius in px
	labelFontSize: 16, // px
	// Force simulation
	linkDistance: 35, // target edge length; smaller = tighter graph
	chargeStrength: -80, // node-node repulsion; less negative = tighter
	collisionPadding: 6, // collision radius = nodeRadius + this
	// Initial seed positions (folder-based pre-layout)
	seedRadiusBase: 120, // minimum radius of the folder ring
	seedRadiusPerFolder: 30, // extra radius per folder so big vaults spread out
	seedJitter: 60, // px of random offset within each folder cluster
	// Cluster attractive force
	clusterStrength: 0.08, // initial pull strength when a cluster is active
};

const data: GraphData = window.__graphData;
console.log(
	"[graph-view] nodes:",
	data.nodes.length,
	"edges:",
	data.edges.length,
);
console.log("[graph-view] sample edge:", data.edges[0]);
const container = document.getElementById("graph-view")!;
// When embedded, the container may have no explicit height yet.
// Fall back to a reasonable default so the widget is visible.
const width =
	container.clientWidth || container.parentElement!.clientWidth || 800;
const height = container.clientHeight || Math.min(width * 0.6, 600);

// Visible debug counter
const debugEl = document.createElement("div");
debugEl.className = "debug-info";
debugEl.textContent = `nodes: ${data.nodes.length}  edges: ${data.edges.length}`;
container.appendChild(debugEl);

// Adjacency for hover highlighting
const adj = new Map<string, Set<string>>();
data.nodes.forEach((n) => {
	adj.set(n.id, new Set());
});
data.edges.forEach((e) => {
	const sid = typeof e.source === "string" ? e.source : e.source.id;
	const tid = typeof e.target === "string" ? e.target : e.target.id;
	adj.get(sid)!.add(tid);
	adj.get(tid)!.add(sid);
});

// Color by folder
const folders: string[] = [...new Set(data.nodes.map((n) => n.folder))];
const palette = [
	"#4e79a7",
	"#f28e2b",
	"#e15759",
	"#76b7b2",
	"#59a14f",
	"#edc948",
	"#b07aa1",
	"#ff9da7",
	"#9c755f",
	"#bab0ac",
];
const folderColor = new Map<string, string>(
	folders.map((f, i) => [f, palette[i % palette.length]] as const),
);

/**
 * Pre-position nodes by folder so the initial layout already reflects
 * cluster structure. Toggling cluster force later becomes a refinement
 * rather than a full relayout.
 *
 * Folders are used (rather than tags) because they form a partition —
 * every node has exactly one folder, so the seed is unambiguous. Tag
 * clusters still pull nodes together when their force is enabled.
 *
 * Mutates [data.nodes] by setting [n.x] and [n.y]. Must run BEFORE
 * [d3.forceSimulation] is called, since d3 only randomizes nodes that
 * lack initial positions.
 */
function seedNodePositions(): void {
	const seedRadius = Math.max(
		config.seedRadiusBase,
		folders.length * config.seedRadiusPerFolder,
	);
	const folderHome = new Map<string, [number, number]>();
	folders.forEach((f, i) => {
		const angle = (i / Math.max(folders.length, 1)) * 2 * Math.PI;
		folderHome.set(f, [
			seedRadius * Math.cos(angle),
			seedRadius * Math.sin(angle),
		]);
	});
	data.nodes.forEach((n) => {
		const [hx, hy] = folderHome.get(n.folder)!;
		n.x = hx + (Math.random() - 0.5) * config.seedJitter;
		n.y = hy + (Math.random() - 0.5) * config.seedJitter;
	});
}
seedNodePositions();

// SVG setup
const svg = d3
	.select("#graph-view")
	.append("svg")
	.attr("width", width)
	.attr("height", height);

// Defs go on the SVG itself (not inside the zoomed group)
svg
	.append("defs")
	.append("marker")
	.attr("id", "arrow")
	.attr("viewBox", "0 -3 6 6")
	.attr("refX", 12)
	.attr("refY", 0)
	.attr("markerWidth", 6)
	.attr("markerHeight", 6)
	.attr("orient", "auto")
	.append("path")
	.attr("d", "M0,-3L6,0L0,3")
	.attr("fill", "#e0e0e0");

// Root group — all content lives inside this so we can pan/zoom it
const root = svg.append("g").attr("class", "zoom-root");

// Force simulation
const simulation = d3
	.forceSimulation(data.nodes)
	.force(
		"link",
		d3
			.forceLink(data.edges)
			.id((d: GraphNode) => d.id)
			.distance(config.linkDistance),
	)
	.force("charge", d3.forceManyBody().strength(config.chargeStrength))
	.force("center", d3.forceCenter(0, 0))
	.force(
		"collision",
		d3.forceCollide().radius(config.nodeRadius + config.collisionPadding),
	)
	.force("cluster", clusterForce());

/**
 * Custom d3 force: for every active cluster, pull its members toward
 * the cluster centroid.
 *
 * Multi-membership composes naturally — a node in N active clusters has
 * the centroid pull from each cluster added into its [vx]/[vy] in the
 * same tick, so it settles between them.
 *
 * The force is a no-op when no clusters are active or when
 * [clusterStrength] is zero, so it costs nothing in the default state.
 */
function clusterForce() {
	function force(alpha: number): void {
		if (clusterStrength === 0) return;
		for (const c of activeClusters()) {
			let cx = 0;
			let cy = 0;
			for (const n of c.nodes) {
				cx += n.x!;
				cy += n.y!;
			}
			cx /= c.nodes.length;
			cy /= c.nodes.length;
			const k = clusterStrength * alpha;
			for (const n of c.nodes) {
				n.vx! += (cx - n.x!) * k;
				n.vy! += (cy - n.y!) * k;
			}
		}
	}
	// d3 calls force.initialize(nodes) when the force is added; we don't
	// need per-node state, but the function must exist.
	(force as any).initialize = () => {};
	return force;
}

// Clusters
// ====================
// A node may belong to multiple clusters (one per folder + one per tag).
// Hulls are rendered under links so nodes/edges remain readable.

interface Cluster {
	key: string;
	kind: "folder" | "tag";
	label: string;
	color: string;
	nodes: GraphNode[];
}

const tagPalette = [
	"#8dd3c7",
	"#ffffb3",
	"#bebada",
	"#fb8072",
	"#80b1d3",
	"#fdb462",
	"#b3de69",
	"#fccde5",
	"#d9d9d9",
	"#bc80bd",
];
const allTags: string[] = [...new Set(data.nodes.flatMap((n) => n.tags || []))];
const tagColor = new Map<string, string>(
	allTags.map((t, i) => [t, tagPalette[i % tagPalette.length]] as const),
);

const folderClusters: Cluster[] = [...new Set(data.nodes.map((n) => n.folder))]
	.filter((folder) => selectorMatches(graphConfig.dir, folder))
	.map((folder) => ({
		key: `folder:${folder}`,
		kind: "folder",
		label: folder,
		color: folderColor.get(folder)!,
		nodes: data.nodes.filter((n) => n.folder === folder),
	}));
const tagClusters: Cluster[] = allTags
	.filter((tag) => selectorMatches(graphConfig.tag, tag))
	.map((tag) => ({
		key: `tag:${tag}`,
		kind: "tag",
		label: `#${tag}`,
		color: tagColor.get(tag)!,
		nodes: data.nodes.filter((n) => (n.tags || []).includes(tag)),
	}));

// Visibility state — set of cluster keys currently shown.
// Seeded from default_dir / default_tag, restricted to clusters that
// actually passed the dir/tag filter above.
const visibleKeys = new Set<string>();
for (const c of folderClusters) {
	if (selectorMatches(graphConfig.default_dir, c.label)) visibleKeys.add(c.key);
}
for (const c of tagClusters) {
	// c.label is "#tag"; default_tag is matched against the bare tag name
	const bare = c.label.replace(/^#/, "");
	if (selectorMatches(graphConfig.default_tag, bare)) visibleKeys.add(c.key);
}
// Cluster attractive-force strength (0 = off). Mutable at runtime via
// the strength slider; initial value comes from config.
let clusterStrength = config.clusterStrength;

// Hull layer — under links
const hullLayer = root.insert("g", ":first-child").attr("class", "hulls");

const HULL_PAD = 24;
const hullPath = d3.line().curve(d3.curveCatmullRomClosed.alpha(1));

const allClusters: Cluster[] = [...folderClusters, ...tagClusters];

function activeClusters(): Cluster[] {
	return allClusters.filter(
		(c) => visibleKeys.has(c.key) && c.nodes.length >= 2,
	);
}

function renderHulls(): void {
	const sel = hullLayer
		.selectAll("path")
		.data(activeClusters(), (c: Cluster) => c.key);
	sel.exit().remove();
	const entered = sel
		.enter()
		.append("path")
		.attr("fill-opacity", 0.12)
		.attr("stroke-opacity", 0.55)
		.attr("stroke-width", 2)
		.attr("stroke-linejoin", "round");
	entered
		.merge(sel)
		.attr("fill", (c: Cluster) => c.color)
		.attr("stroke", (c: Cluster) => c.color);
	updateHullGeometry();
}

function updateHullGeometry(): void {
	hullLayer.selectAll("path").attr("d", (c: Cluster) => {
		// Expand each node into a small ring of points so the hull has padding
		const pts = c.nodes.flatMap((n) => [
			[n.x! + HULL_PAD, n.y!],
			[n.x! - HULL_PAD, n.y!],
			[n.x!, n.y! + HULL_PAD],
			[n.x!, n.y! - HULL_PAD],
		]);
		const hull = d3.polygonHull(pts);
		return hull ? `${hullPath(hull)}Z` : null;
	});
}

// Edges — drawn first so nodes render on top
const link = root
	.append("g")
	.attr("class", "links")
	.attr("stroke", "#e0e0e0")
	.attr("stroke-opacity", 0.8)
	.selectAll("line")
	.data(data.edges)
	.join("line")
	.attr("stroke-width", 1.5)
	.attr("marker-end", "url(#arrow)");

// Nodes
const node = root
	.append("g")
	.attr("class", "nodes")
	.selectAll("circle")
	.data(data.nodes)
	.join("circle")
	.attr("r", config.nodeRadius)
	.attr("fill", (d: GraphNode) => folderColor.get(d.folder)!)
	.attr("stroke", "#fff")
	.attr("stroke-width", 1.5)
	.style("cursor", "pointer")
	.call(drag(simulation));

// Labels
const label = root
	.append("g")
	.attr("class", "labels")
	.selectAll("text")
	.data(data.nodes)
	.join("text")
	.text((d: GraphNode) => d.title)
	.attr("font-size", config.labelFontSize)
	.attr("font-family", "sans-serif")
	.attr("dx", config.nodeRadius + 4)
	.attr("dy", 4);

// Hover: dim unrelated, highlight connected edges
node
	.on("mouseenter", (_event: Event, d: GraphNode) => {
		const neighbors = adj.get(d.id)!;
		node.attr("opacity", (n: GraphNode) =>
			n.id === d.id || neighbors.has(n.id) ? 1 : 0.15,
		);
		label.attr("opacity", (n: GraphNode) =>
			n.id === d.id || neighbors.has(n.id) ? 1 : 0.15,
		);
		link
			.attr("stroke", (e: GraphEdge) => {
				const sid = typeof e.source === "string" ? e.source : e.source.id;
				const tid = typeof e.target === "string" ? e.target : e.target.id;
				return sid === d.id || tid === d.id ? "#ffcc00" : "#e0e0e0";
			})
			.attr("stroke-opacity", (e: GraphEdge) => {
				const sid = typeof e.source === "string" ? e.source : e.source.id;
				const tid = typeof e.target === "string" ? e.target : e.target.id;
				return sid === d.id || tid === d.id ? 1 : 0.15;
			})
			.attr("stroke-width", (e: GraphEdge) => {
				const sid = typeof e.source === "string" ? e.source : e.source.id;
				const tid = typeof e.target === "string" ? e.target : e.target.id;
				return sid === d.id || tid === d.id ? 2.5 : 1.5;
			});
	})
	.on("mouseleave", () => {
		node.attr("opacity", 1);
		label.attr("opacity", 1);
		link
			.attr("stroke", "#e0e0e0")
			.attr("stroke-opacity", 0.8)
			.attr("stroke-width", 1.5);
	});

// Click: navigate
node.on("click", (_event: Event, d: GraphNode) => {
	const url = d.id.replace(/\.md$/, ".html");
	window.location.href = url;
});

// Tick
simulation.on("tick", () => {
	link
		.attr("x1", (d: GraphEdge) =>
			typeof d.source === "string" ? 0 : d.source.x,
		)
		.attr("y1", (d: GraphEdge) =>
			typeof d.source === "string" ? 0 : d.source.y,
		)
		.attr("x2", (d: GraphEdge) =>
			typeof d.target === "string" ? 0 : d.target.x,
		)
		.attr("y2", (d: GraphEdge) =>
			typeof d.target === "string" ? 0 : d.target.y,
		);
	node.attr("cx", (d: GraphNode) => d.x!).attr("cy", (d: GraphNode) => d.y!);
	label.attr("x", (d: GraphNode) => d.x!).attr("y", (d: GraphNode) => d.y!);
	updateHullGeometry();
});

// Zoom + pan
const zoom = d3
	.zoom()
	.scaleExtent([0.1, 8])
	.on("zoom", (event: any) => {
		root.attr("transform", event.transform);
	});
svg.call(zoom);

// Fit-to-screen: run simulation ahead, compute bounds, set transform
function fitToScreen(): void {
	// Tick until the layout is roughly settled
	for (let i = 0; i < 300; i++) simulation.tick();
	simulation.alpha(0.1).restart();

	const curW = container.clientWidth || width;
	const curH = container.clientHeight || height;

	let minX = Infinity,
		minY = Infinity,
		maxX = -Infinity,
		maxY = -Infinity;
	data.nodes.forEach((n) => {
		if (n.x! < minX) minX = n.x!;
		if (n.y! < minY) minY = n.y!;
		if (n.x! > maxX) maxX = n.x!;
		if (n.y! > maxY) maxY = n.y!;
	});
	const pad = 60;
	const dx = maxX - minX + pad * 2;
	const dy = maxY - minY + pad * 2;
	const cx = (minX + maxX) / 2;
	const cy = (minY + maxY) / 2;
	const scale = Math.min(curW / dx, curH / dy, 2);
	const tx = curW / 2 - scale * cx;
	const ty = curH / 2 - scale * cy;
	svg.call(zoom.transform, d3.zoomIdentity.translate(tx, ty).scale(scale));
}
fitToScreen();

// Zoom controls
const controls = d3
	.select("#graph-view")
	.append("div")
	.attr("class", "zoom-controls");
controls
	.append("button")
	.text("+")
	.on("click", () => {
		svg.transition().duration(200).call(zoom.scaleBy, 1.4);
	});
controls
	.append("button")
	.text("−")
	.on("click", () => {
		svg
			.transition()
			.duration(200)
			.call(zoom.scaleBy, 1 / 1.4);
	});
controls
	.append("button")
	.text("⤢")
	.attr("title", "Fit")
	.on("click", () => {
		fitToScreen();
	});
// Backdrop for dimming the page behind the floating graph
const backdrop = d3
	.select("body")
	.append("div")
	.attr("id", "graph-view-backdrop")
	.on("click", () => toggleMaximize());

function toggleMaximize(): void {
	const isMax = container.classList.toggle("maximized");
	backdrop.classed("visible", isMax);
	d3.select(".maximize-btn").text(isMax ? "\u2B8C" : "\u26F6");
	const w = container.clientWidth;
	const h = container.clientHeight;
	svg.attr("width", w).attr("height", h);
	fitToScreen();
}
controls
	.append("button")
	.text("\u26F6")
	.attr("title", "Expand")
	.attr("class", "maximize-btn")
	.on("click", toggleMaximize);
d3.select("#graph-view")
	.append("button")
	.attr("class", "maximize-close")
	.attr("title", "Close")
	.text("\u2715")
	.on("click", toggleMaximize);
document.addEventListener("keydown", (e: KeyboardEvent) => {
	if (e.key === "Escape" && container.classList.contains("maximized")) {
		toggleMaximize();
	}
});

// Cluster filter panel
// ====================
const panel = d3
	.select("#graph-view")
	.append("div")
	.attr("class", "cluster-panel collapsed");

// Panel toggle — a small tab visible when the panel is minimized
const toggle = panel.append("div").attr("class", "panel-toggle");
toggle.append("span").attr("class", "panel-toggle-label").text("Clusters");
toggle.append("span").attr("class", "panel-toggle-chevron").text("\u25B2");
toggle.on("click", () => {
	const collapsed = panel.classed("collapsed");
	panel.classed("collapsed", !collapsed);
	panel.select(".panel-toggle-chevron").text(collapsed ? "\u25BC" : "\u25B2");
});

// Strength slider
const strengthRow = panel.append("div").attr("class", "panel-row");
strengthRow.append("label").text("Cluster pull");
const strengthInput = strengthRow
	.append("input")
	.attr("type", "range")
	.attr("min", 0)
	.attr("max", 0.3)
	.attr("step", 0.01)
	.attr("value", clusterStrength)
	.on("input", function (this: HTMLInputElement) {
		clusterStrength = +this.value;
		simulation.alpha(0.5).restart();
	});
strengthRow.append("span").attr("class", "strength-val").text(clusterStrength);
strengthInput.on("input.label", function (this: HTMLInputElement) {
	strengthRow.select(".strength-val").text((+this.value).toFixed(2));
});

function setVisible(keys: string[], on: boolean): void {
	for (const k of keys) {
		if (on) visibleKeys.add(k);
		else visibleKeys.delete(k);
	}
	panel
		.selectAll("input.cluster-cb")
		.property("checked", function (this: HTMLInputElement) {
			return visibleKeys.has(this.dataset.key!);
		});
	renderHulls();
	simulation.alpha(0.5).restart();
}

function buildSection(title: string, clusters: Cluster[]): void {
	const section = panel.append("div").attr("class", "panel-section");
	const header = section.append("div").attr("class", "panel-header");
	header.append("span").text(title);
	const actions = header.append("span").attr("class", "panel-actions");
	actions
		.append("button")
		.text("all")
		.on("click", () =>
			setVisible(
				clusters.map((c) => c.key),
				true,
			),
		);
	actions
		.append("button")
		.text("none")
		.on("click", () =>
			setVisible(
				clusters.map((c) => c.key),
				false,
			),
		);

	const list = section.append("div").attr("class", "panel-list");
	const items = list
		.selectAll("label")
		.data(clusters)
		.join("label")
		.attr("class", "cluster-item")
		.attr("title", (c: Cluster) => `${c.label} (${c.nodes.length} nodes)`);
	items
		.append("input")
		.attr("type", "checkbox")
		.attr("class", "cluster-cb")
		.property("checked", (c: Cluster) => visibleKeys.has(c.key))
		.each(function (this: HTMLInputElement, c: Cluster) {
			this.dataset.key = c.key;
		})
		.on("change", function (this: HTMLInputElement, _event: Event, c: Cluster) {
			if (this.checked) visibleKeys.add(c.key);
			else visibleKeys.delete(c.key);
			renderHulls();
			simulation.alpha(0.5).restart();
		});
	items
		.append("span")
		.attr("class", "swatch")
		.style("background", (c: Cluster) => c.color);
	items
		.append("span")
		.attr("class", "cluster-label")
		.text((c: Cluster) => `${c.label} (${c.nodes.length})`);
}

buildSection("Folders", folderClusters);
buildSection("Tags", tagClusters);
// Render hulls for any default-selected clusters
renderHulls();

// Drag behavior — coords already in root's local space thanks to d3.zoom
function drag(simulation: any) {
	return d3
		.drag()
		.on("start", (event: any, d: GraphNode) => {
			if (!event.active) simulation.alphaTarget(0.3).restart();
			d.fx = d.x;
			d.fy = d.y;
		})
		.on("drag", (event: any, d: GraphNode) => {
			d.fx = event.x;
			d.fy = event.y;
		})
		.on("end", (event: any, d: GraphNode) => {
			if (!event.active) simulation.alphaTarget(0);
			d.fx = null;
			d.fy = null;
		});
}
