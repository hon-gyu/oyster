/**
 * Graph view using D3.js.
 *
 * Preconditions:
 *  - graph JSON data is in [window.__graphData]
 *  - optional [window.__graphConfig] of type [HomeGraphViewConfig]
 *    (see config.d.ts — that file is the wire-format contract with OCaml)
 *
 * File layout:
 *   1. Constants + tunable params (every knob lives here)
 *   2. Graph config + data + derivations
 *   3. Seed / reset
 *   4. Forces + simulation
 *   5. SVG scaffold + rendering
 *   6. Interaction (hover, tick, zoom, fit, maximize, drag)
 *   7. Settings panel (sliders + cluster toggles)
 *
 * Spec (what must hold, not how it's done):
 *  - Sliders live in one collapsible panel. No persistence.
 *  - Every slider shows its current numeric value.
 *  - Containment (radius and strength) is slider-adjustable, not hard-coded.
 *  - Sliders reheat the simulation so changes take effect without
 *    dragging a node.
 *  - Reset button re-seeds layout using current slider settings.
 *  - Parent folder clusters include all recursive descendants.
 */

import {
	activeClusters as computeActiveClusters,
	assignColors,
	buildAdjacency,
	buildFolderClusters,
	buildTagClusters,
	type Cluster,
	initialVisibleKeys,
} from "./graph_model";

// ==========================================================================
// 1. Constants + tunable params
// ==========================================================================

/** Non-adjustable constants. Grouped so there's exactly one place to
 *  change a magic number. */
const C = {
	// Seed layout (folder ring)
	seedRadiusBase: 120,
	seedRadiusPerFolder: 30,
	seedJitter: 60,

	// Hull padding around each node, in svg units
	hullPad: 24,

	// Zoom
	zoomScaleExtent: [0.1, 8] as [number, number],
	zoomButtonFactor: 1.4,
	zoomTransitionMs: 200,

	// Fit-to-screen
	fitPad: 60,
	fitMaxScale: 2,
	fitSettleTicks: 300,

	// Simulation reheat alphas
	alphaSlider: 0.1, // gentle reheat on a slider nudge
	alphaReset: 0.5, // stronger reheat on reset / initial load
	alphaDragTarget: 0.3, // alphaTarget while a node is being dragged

	// Link appearance
	linkColorBase: "#e0e0e0",
	linkColorHover: "#ffcc00",
	linkOpacityBase: 0.8,
	linkOpacityDim: 0.15,
	linkWidthBase: 1.5,
	linkWidthHover: 2.5,

	// Node hover dimming
	nodeDimOpacity: 0.15,

	// Palettes
	folderPalette: [
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
	],
	tagPalette: [
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
	],
} as const;

// Tunable parameters (slider-exposed)
// --------------------
//
// Every slider is described here: label, default, range, step, format.
// Add a slider by adding a record and calling [makeSlider] in the panel
// section below. Forces read [P.<name>.value] via callback so live edits
// take effect on the next tick without re-installing forces.

interface Param {
	label: string;
	value: number;
	min: number;
	max: number;
	step: number;
	decimals: number;
}

const P = {
	nodeRadius: {
		label: "Node radius",
		value: 14,
		min: 5,
		max: 25,
		step: 1,
		decimals: 0,
	},
	labelFontSize: {
		label: "Label size",
		value: 16,
		min: 10,
		max: 24,
		step: 1,
		decimals: 0,
	},
	linkDistance: {
		label: "Link distance",
		value: 70,
		min: 10,
		max: 100,
		step: 1,
		decimals: 0,
	},
	chargeStrength: {
		label: "Charge strength",
		value: -80,
		min: -200,
		max: -20,
		step: 5,
		decimals: 0,
	},
	collisionPadding: {
		label: "Collision padding",
		value: 6,
		min: 0,
		max: 20,
		step: 1,
		decimals: 0,
	},
	clusterStrength: {
		label: "Cluster pull",
		value: 0.04,
		min: 0,
		max: 0.2,
		step: 0.005,
		decimals: 2,
	},
	containmentRadius: {
		label: "Containment",
		value: 600,
		min: 0,
		max: 1200,
		step: 50,
		decimals: 0,
	},
	containmentStrength: {
		label: "Contain strength",
		value: 0.05,
		min: 0,
		max: 0.5,
		step: 0.0025,
		decimals: 2,
	},
} satisfies Record<string, Param>;

// ==========================================================================
// 2. Graph config + data + derivations
// ==========================================================================

// Each of dir/tag/default_dir/default_tag is a [Selector]:
//   "all"                   -> include everything
//   "none"                  -> exclude everything
//   { include: [..labels] } -> only these
//   { exclude: [..labels] } -> everything except these
//
// [dir]/[tag] decide which clusters appear in the panel at all.
// [default_dir]/[default_tag] decide which of the visible ones are
// ticked on initial load.
const graphConfig: HomeGraphViewConfig = window.__graphConfig ?? {
	dir: "all",
	tag: "all",
	default_dir: { include: ["*"] },
	default_tag: { include: ["*"] },
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

// Derivations
// --------------------
const adj = buildAdjacency(data.nodes, data.edges);
const folders: string[] = [...new Set(data.nodes.map((n) => n.folder))];
const folderColor = assignColors(folders, [...C.folderPalette]);
const allTags: string[] = [...new Set(data.nodes.flatMap((n) => n.tags || []))];
const tagColor = assignColors(allTags, [...C.tagPalette]);

const folderClusters: Cluster[] = buildFolderClusters(
	data.nodes,
	graphConfig.dir,
	folderColor,
);
const tagClusters: Cluster[] = buildTagClusters(
	data.nodes,
	graphConfig.tag,
	tagColor,
);
const allClusters: Cluster[] = [...folderClusters, ...tagClusters];
const visibleKeys = initialVisibleKeys(
	folderClusters,
	tagClusters,
	graphConfig.default_dir,
	graphConfig.default_tag,
);

function activeClusters(): Cluster[] {
	return computeActiveClusters(allClusters, visibleKeys);
}

// ==========================================================================
// 3. Seed / reset
// ==========================================================================

/**
 * Pre-position nodes on a folder ring so the initial layout already
 * reflects cluster structure. Must run BEFORE [d3.forceSimulation] —
 * d3 randomizes only nodes that lack initial positions.
 *
 * Folders are used (not tags) because they partition nodes
 * unambiguously; tag clusters refine later via cluster force.
 */
function seedNodePositions(): void {
	const seedRadius = Math.max(
		C.seedRadiusBase,
		folders.length * C.seedRadiusPerFolder,
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
		n.x = hx + (Math.random() - 0.5) * C.seedJitter;
		n.y = hy + (Math.random() - 0.5) * C.seedJitter;
	});
}
seedNodePositions();

function resetLayout(): void {
	seedNodePositions();
	data.nodes.forEach((n) => {
		n.vx = 0;
		n.vy = 0;
	});
	simulation.alpha(C.alphaReset).restart();
}

// ==========================================================================
// 4. Forces + simulation
// ==========================================================================

const linkForce = d3
	.forceLink(data.edges)
	.id((d: GraphNode) => d.id)
	.distance(() => P.linkDistance.value);

const chargeForce = d3
	.forceManyBody()
	.strength(() => P.chargeStrength.value);

const collisionForce = d3
	.forceCollide()
	.radius(() => P.nodeRadius.value + P.collisionPadding.value);

/** Soft containment: nodes outside [containmentRadius] are nudged
 *  back toward the origin. No-op when radius or strength is zero. */
function containmentForce() {
	function force(): void {
		const R = P.containmentRadius.value;
		const k = P.containmentStrength.value;
		if (R <= 0 || k <= 0) return;
		for (const n of data.nodes) {
			const r = Math.sqrt(n.x! * n.x! + n.y! * n.y!);
			if (r > R) {
				const ratio = R / r;
				n.x! += (n.x! * ratio - n.x!) * k;
				n.y! += (n.y! * ratio - n.y!) * k;
				n.vx! *= 1 - k;
				n.vy! *= 1 - k;
			}
		}
	}
	// d3 calls force.initialize(nodes) when installed; we keep no state.
	(force as any).initialize = () => {};
	return force;
}

/** Custom force: pull members of every active cluster toward its
 *  centroid. A node in N clusters gets N pulls accumulated in the
 *  same tick, so it settles between them. */
function clusterForce() {
	function force(alpha: number): void {
		const base = P.clusterStrength.value;
		if (base === 0) return;
		for (const c of activeClusters()) {
			let cx = 0;
			let cy = 0;
			for (const n of c.nodes) {
				cx += n.x!;
				cy += n.y!;
			}
			cx /= c.nodes.length;
			cy /= c.nodes.length;
			const k = base * alpha;
			for (const n of c.nodes) {
				n.vx! += (cx - n.x!) * k;
				n.vy! += (cy - n.y!) * k;
			}
		}
	}
	(force as any).initialize = () => {};
	return force;
}

const simulation = d3
	.forceSimulation(data.nodes)
	.force("link", linkForce)
	.force("charge", chargeForce)
	.force("center", d3.forceCenter(0, 0))
	.force("collision", collisionForce)
	.force("cluster", clusterForce())
	.force("containment", containmentForce())
	// Starting at the default alpha=1 lets charge/link forces fling
	// nodes outward hard enough to hit the containment edge before
	// anything settles, where the position-clamp pins them there.
	.alpha(C.alphaReset);

// ==========================================================================
// 5. SVG scaffold + rendering
// ==========================================================================

const svg = d3
	.select("#graph-view")
	.append("svg")
	.attr("width", width)
	.attr("height", height);

// Arrow marker lives on the svg itself, not inside the zoomed group.
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
	.attr("fill", C.linkColorBase);

// Root group — all content lives inside this so we can pan/zoom it.
const root = svg.append("g").attr("class", "zoom-root");

// Hull layer — inserted first so it renders under links.
const hullLayer = root.insert("g", ":first-child").attr("class", "hulls");
const hullPath = d3.line().curve(d3.curveCatmullRomClosed.alpha(1));

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
		// Expand each node into a small ring of points so the hull has padding.
		const pts = c.nodes.flatMap((n) => [
			[n.x! + C.hullPad, n.y!],
			[n.x! - C.hullPad, n.y!],
			[n.x!, n.y! + C.hullPad],
			[n.x!, n.y! - C.hullPad],
		]);
		const hull = d3.polygonHull(pts);
		return hull ? `${hullPath(hull)}Z` : null;
	});
}

// Drag behavior — declared as a function so it's hoisted for the node
// builder below. Coords are already in the root's local space thanks
// to d3.zoom.
function drag(sim: any) {
	return d3
		.drag()
		.on("start", (event: any, d: GraphNode) => {
			if (!event.active) sim.alphaTarget(C.alphaDragTarget).restart();
			d.fx = d.x;
			d.fy = d.y;
		})
		.on("drag", (event: any, d: GraphNode) => {
			d.fx = event.x;
			d.fy = event.y;
		})
		.on("end", (event: any, d: GraphNode) => {
			if (!event.active) sim.alphaTarget(0);
			d.fx = null;
			d.fy = null;
		});
}

// Edges — drawn first so nodes render on top.
const link = root
	.append("g")
	.attr("class", "links")
	.attr("stroke", C.linkColorBase)
	.attr("stroke-opacity", C.linkOpacityBase)
	.selectAll("line")
	.data(data.edges)
	.join("line")
	.attr("stroke-width", C.linkWidthBase)
	.attr("marker-end", "url(#arrow)");

const node = root
	.append("g")
	.attr("class", "nodes")
	.selectAll("circle")
	.data(data.nodes)
	.join("circle")
	.attr("r", () => P.nodeRadius.value)
	.attr("fill", (d: GraphNode) => folderColor.get(d.folder)!)
	.attr("stroke", "#fff")
	.attr("stroke-width", 1.5)
	.style("cursor", "pointer")
	.call(drag(simulation));

const label = root
	.append("g")
	.attr("class", "labels")
	.selectAll("text")
	.data(data.nodes)
	.join("text")
	.text((d: GraphNode) => d.title)
	.attr("font-size", () => P.labelFontSize.value)
	.attr("font-family", "sans-serif")
	.attr("dx", () => P.nodeRadius.value + 4)
	.attr("dy", 4);

// ==========================================================================
// 6. Interaction
// ==========================================================================

// Hover: dim unrelated nodes, highlight connected edges.
node
	.on("mouseenter", (_event: Event, d: GraphNode) => {
		const neighbors = adj.get(d.id)!;
		const visible = (n: GraphNode) =>
			n.id === d.id || neighbors.has(n.id) ? 1 : C.nodeDimOpacity;
		const touchesD = (e: GraphEdge) => {
			const sid = typeof e.source === "string" ? e.source : e.source.id;
			const tid = typeof e.target === "string" ? e.target : e.target.id;
			return sid === d.id || tid === d.id;
		};
		node.attr("opacity", visible);
		label.attr("opacity", visible);
		link
			.attr("stroke", (e: GraphEdge) =>
				touchesD(e) ? C.linkColorHover : C.linkColorBase,
			)
			.attr("stroke-opacity", (e: GraphEdge) =>
				touchesD(e) ? 1 : C.linkOpacityDim,
			)
			.attr("stroke-width", (e: GraphEdge) =>
				touchesD(e) ? C.linkWidthHover : C.linkWidthBase,
			);
	})
	.on("mouseleave", () => {
		node.attr("opacity", 1);
		label.attr("opacity", 1);
		link
			.attr("stroke", C.linkColorBase)
			.attr("stroke-opacity", C.linkOpacityBase)
			.attr("stroke-width", C.linkWidthBase);
	});

node.on("click", (_event: Event, d: GraphNode) => {
	window.location.href = d.href;
});

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
	.scaleExtent(C.zoomScaleExtent)
	.on("zoom", (event: any) => {
		root.attr("transform", event.transform);
	});
svg.call(zoom);

// Fit-to-screen: run simulation ahead, compute bounds, set transform.
function fitToScreen(): void {
	for (let i = 0; i < C.fitSettleTicks; i++) simulation.tick();
	simulation.alpha(C.alphaSlider).restart();

	const curW = container.clientWidth || width;
	const curH = container.clientHeight || height;

	let minX = Infinity;
	let minY = Infinity;
	let maxX = -Infinity;
	let maxY = -Infinity;
	data.nodes.forEach((n) => {
		if (n.x! < minX) minX = n.x!;
		if (n.y! < minY) minY = n.y!;
		if (n.x! > maxX) maxX = n.x!;
		if (n.y! > maxY) maxY = n.y!;
	});
	const dx = maxX - minX + C.fitPad * 2;
	const dy = maxY - minY + C.fitPad * 2;
	const cx = (minX + maxX) / 2;
	const cy = (minY + maxY) / 2;
	const scale = Math.min(curW / dx, curH / dy, C.fitMaxScale);
	const tx = curW / 2 - scale * cx;
	const ty = curH / 2 - scale * cy;
	svg.call(zoom.transform, d3.zoomIdentity.translate(tx, ty).scale(scale));
}
fitToScreen();

// Maximize + controls
// --------------------

const backdrop = d3
	.select("body")
	.append("div")
	.attr("id", "graph-view-backdrop")
	.on("click", () => toggleMaximize());

function toggleMaximize(): void {
	const isMax = container.classList.toggle("maximized");
	backdrop.classed("visible", isMax);
	d3.select(".maximize-btn").text(isMax ? "−" : "\u26F6");
	const w = container.clientWidth;
	const h = container.clientHeight;
	svg.attr("width", w).attr("height", h);
	fitToScreen();
}

const controls = d3
	.select("#graph-view")
	.append("div")
	.attr("class", "zoom-controls");
controls
	.append("button")
	.text("+")
	.on("click", () => {
		svg
			.transition()
			.duration(C.zoomTransitionMs)
			.call(zoom.scaleBy, C.zoomButtonFactor);
	});
controls
	.append("button")
	.text("−")
	.on("click", () => {
		svg
			.transition()
			.duration(C.zoomTransitionMs)
			.call(zoom.scaleBy, 1 / C.zoomButtonFactor);
	});
controls.append("button").text("⤢").attr("title", "Fit").on("click", fitToScreen);
controls.append("button").text("↻").attr("title", "Reset layout").on("click", resetLayout);
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

// ==========================================================================
// 7. Settings panel
// ==========================================================================

const panel = d3
	.select("#graph-view")
	.append("div")
	.attr("class", "settings-panel collapsed");

// Panel toggle tab
const toggleBar = panel.append("div").attr("class", "panel-toggle");
toggleBar.append("span").attr("class", "panel-toggle-label").text("Settings");
toggleBar.append("span").attr("class", "panel-toggle-chevron").text("\u25B2");
toggleBar.on("click", () => {
	const collapsed = panel.classed("collapsed");
	panel.classed("collapsed", !collapsed);
	panel.select(".panel-toggle-chevron").text(collapsed ? "\u25BC" : "\u25B2");
	// Clear inline dimensions left by CSS resize so collapse/expand works.
	const el = panel.node() as HTMLElement;
	el.style.width = "";
	el.style.height = "";
});

// Slider factory
// --------------------

/** Build one slider row backed by a [Param] record.
 *  [onChange] runs on every input event before the simulation is reheated. */
function makeSlider(p: Param, onChange?: (v: number) => void): void {
	const row = panel.append("div").attr("class", "panel-row");
	row.append("label").text(p.label);
	const input = row
		.append("input")
		.attr("type", "range")
		.attr("min", p.min)
		.attr("max", p.max)
		.attr("step", p.step)
		.attr("value", p.value);
	const valSpan = row
		.append("span")
		.attr("class", "slider-val")
		.text(p.value.toFixed(p.decimals));
	input.on("input", function (this: HTMLInputElement) {
		p.value = +this.value;
		valSpan.text(p.value.toFixed(p.decimals));
		onChange?.(p.value);
		simulation.alpha(C.alphaSlider).restart();
	});
}

makeSlider(P.nodeRadius, (v) => {
	node.attr("r", () => v);
	label.attr("dx", () => v + 4);
	// Re-install collision so its cached radius picks up the new value.
	simulation.force(
		"collision",
		d3
			.forceCollide()
			.radius(() => P.nodeRadius.value + P.collisionPadding.value),
	);
});
makeSlider(P.labelFontSize, (v) => {
	label.attr("font-size", () => v);
});
makeSlider(P.linkDistance);
makeSlider(P.chargeStrength);
makeSlider(P.collisionPadding);
makeSlider(P.clusterStrength);
makeSlider(P.containmentRadius);
makeSlider(P.containmentStrength);

// Cluster toggles
// --------------------

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
	simulation.alpha(C.alphaReset).restart();
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
			simulation.alpha(C.alphaSlider).restart();
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
// Render hulls for any default-selected clusters.
renderHulls();
