"use strict";
(() => {
  // selector.ts
  function selectorMatches(sel, label2) {
    if (sel === "all") return true;
    if (sel === "none") return false;
    if ("include" in sel) return sel.include.includes(label2);
    if ("exclude" in sel) return !sel.exclude.includes(label2);
    return true;
  }

  // graph_model.ts
  function buildAdjacency(nodes, edges) {
    const adj2 = /* @__PURE__ */ new Map();
    for (const n of nodes) adj2.set(n.id, /* @__PURE__ */ new Set());
    for (const e of edges) {
      const sid = typeof e.source === "string" ? e.source : e.source.id;
      const tid = typeof e.target === "string" ? e.target : e.target.id;
      adj2.get(sid).add(tid);
      adj2.get(tid).add(sid);
    }
    return adj2;
  }
  function assignColors(labels, palette) {
    return new Map(labels.map((l, i) => [l, palette[i % palette.length]]));
  }
  function buildFolderClusters(nodes, dirSelector, folderColor2) {
    const folders2 = [...new Set(nodes.map((n) => n.folder))];
    return folders2.filter((folder) => selectorMatches(dirSelector, folder)).map((folder) => ({
      key: `folder:${folder}`,
      kind: "folder",
      label: folder,
      color: folderColor2.get(folder),
      nodes: nodes.filter(
        (n) => n.folder === folder || n.folder.startsWith(`${folder}/`)
      )
    }));
  }
  function buildTagClusters(nodes, tagSelector, tagColor2) {
    const allTags2 = [...new Set(nodes.flatMap((n) => n.tags || []))];
    return allTags2.filter((tag) => selectorMatches(tagSelector, tag)).map((tag) => ({
      key: `tag:${tag}`,
      kind: "tag",
      label: `#${tag}`,
      color: tagColor2.get(tag),
      nodes: nodes.filter((n) => (n.tags || []).includes(tag))
    }));
  }
  function initialVisibleKeys(folderClusters2, tagClusters2, defaultDir, defaultTag) {
    const keys = /* @__PURE__ */ new Set();
    for (const c of folderClusters2) {
      if (selectorMatches(defaultDir, c.label)) keys.add(c.key);
    }
    for (const c of tagClusters2) {
      const bare = c.label.replace(/^#/, "");
      if (selectorMatches(defaultTag, bare)) keys.add(c.key);
    }
    return keys;
  }
  function activeClusters(allClusters2, visibleKeys2) {
    return allClusters2.filter(
      (c) => visibleKeys2.has(c.key) && c.nodes.length >= 2
    );
  }

  // widget.ts
  var C = {
    // Seed layout (folder ring)
    seedRadiusBase: 120,
    seedRadiusPerFolder: 30,
    seedJitter: 60,
    // Hull padding around each node, in svg units
    hullPad: 24,
    // Zoom
    zoomScaleExtent: [0.1, 8],
    zoomButtonFactor: 1.4,
    zoomTransitionMs: 200,
    // Fit-to-screen
    fitPad: 60,
    fitMaxScale: 2,
    fitSettleTicks: 300,
    // Simulation reheat alphas
    alphaSlider: 0.1,
    // gentle reheat on a slider nudge
    alphaReset: 0.5,
    // stronger reheat on reset / initial load
    alphaDragTarget: 0.3,
    // alphaTarget while a node is being dragged
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
      "#bab0ac"
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
      "#bc80bd"
    ]
  };
  var P = {
    nodeRadius: {
      label: "Node radius",
      value: 14,
      min: 5,
      max: 25,
      step: 1,
      decimals: 0
    },
    labelFontSize: {
      label: "Label size",
      value: 16,
      min: 10,
      max: 24,
      step: 1,
      decimals: 0
    },
    linkDistance: {
      label: "Link distance",
      value: 70,
      min: 10,
      max: 100,
      step: 1,
      decimals: 0
    },
    chargeStrength: {
      label: "Charge strength",
      value: -80,
      min: -200,
      max: -20,
      step: 5,
      decimals: 0
    },
    collisionPadding: {
      label: "Collision padding",
      value: 6,
      min: 0,
      max: 20,
      step: 1,
      decimals: 0
    },
    clusterStrength: {
      label: "Cluster pull",
      value: 0.04,
      min: 0,
      max: 0.2,
      step: 5e-3,
      decimals: 2
    },
    containmentRadius: {
      label: "Containment",
      value: 600,
      min: 0,
      max: 1200,
      step: 50,
      decimals: 0
    },
    containmentStrength: {
      label: "Contain strength",
      value: 0.05,
      min: 0,
      max: 0.5,
      step: 25e-4,
      decimals: 2
    }
  };
  var graphConfig = window.__graphConfig ?? {
    dir: "all",
    tag: "all",
    default_dir: { include: ["*"] },
    default_tag: { include: ["*"] }
  };
  var data = window.__graphData;
  console.log(
    "[graph-view] nodes:",
    data.nodes.length,
    "edges:",
    data.edges.length
  );
  console.log("[graph-view] sample edge:", data.edges[0]);
  var container = document.getElementById("graph-view");
  var width = container.clientWidth || container.parentElement.clientWidth || 800;
  var height = container.clientHeight || Math.min(width * 0.6, 600);
  var debugEl = document.createElement("div");
  debugEl.className = "debug-info";
  debugEl.textContent = `nodes: ${data.nodes.length}  edges: ${data.edges.length}`;
  container.appendChild(debugEl);
  var adj = buildAdjacency(data.nodes, data.edges);
  var folders = [...new Set(data.nodes.map((n) => n.folder))];
  var folderColor = assignColors(folders, [...C.folderPalette]);
  var allTags = [...new Set(data.nodes.flatMap((n) => n.tags || []))];
  var tagColor = assignColors(allTags, [...C.tagPalette]);
  var folderClusters = buildFolderClusters(
    data.nodes,
    graphConfig.dir,
    folderColor
  );
  var tagClusters = buildTagClusters(
    data.nodes,
    graphConfig.tag,
    tagColor
  );
  var allClusters = [...folderClusters, ...tagClusters];
  var visibleKeys = initialVisibleKeys(
    folderClusters,
    tagClusters,
    graphConfig.default_dir,
    graphConfig.default_tag
  );
  function activeClusters2() {
    return activeClusters(allClusters, visibleKeys);
  }
  function seedNodePositions() {
    const seedRadius = Math.max(
      C.seedRadiusBase,
      folders.length * C.seedRadiusPerFolder
    );
    const folderHome = /* @__PURE__ */ new Map();
    folders.forEach((f, i) => {
      const angle = i / Math.max(folders.length, 1) * 2 * Math.PI;
      folderHome.set(f, [
        seedRadius * Math.cos(angle),
        seedRadius * Math.sin(angle)
      ]);
    });
    data.nodes.forEach((n) => {
      const [hx, hy] = folderHome.get(n.folder);
      n.x = hx + (Math.random() - 0.5) * C.seedJitter;
      n.y = hy + (Math.random() - 0.5) * C.seedJitter;
    });
  }
  seedNodePositions();
  function resetLayout() {
    seedNodePositions();
    data.nodes.forEach((n) => {
      n.vx = 0;
      n.vy = 0;
    });
    simulation.alpha(C.alphaReset).restart();
  }
  var linkForce = d3.forceLink(data.edges).id((d) => d.id).distance(() => P.linkDistance.value);
  var chargeForce = d3.forceManyBody().strength(() => P.chargeStrength.value);
  var collisionForce = d3.forceCollide().radius(() => P.nodeRadius.value + P.collisionPadding.value);
  function containmentForce() {
    function force() {
      const R = P.containmentRadius.value;
      const k = P.containmentStrength.value;
      if (R <= 0 || k <= 0) return;
      for (const n of data.nodes) {
        const r = Math.sqrt(n.x * n.x + n.y * n.y);
        if (r > R) {
          const ratio = R / r;
          n.x += (n.x * ratio - n.x) * k;
          n.y += (n.y * ratio - n.y) * k;
          n.vx *= 1 - k;
          n.vy *= 1 - k;
        }
      }
    }
    force.initialize = () => {
    };
    return force;
  }
  function clusterForce() {
    function force(alpha) {
      const base = P.clusterStrength.value;
      if (base === 0) return;
      for (const c of activeClusters2()) {
        let cx = 0;
        let cy = 0;
        for (const n of c.nodes) {
          cx += n.x;
          cy += n.y;
        }
        cx /= c.nodes.length;
        cy /= c.nodes.length;
        const k = base * alpha;
        for (const n of c.nodes) {
          n.vx += (cx - n.x) * k;
          n.vy += (cy - n.y) * k;
        }
      }
    }
    force.initialize = () => {
    };
    return force;
  }
  var simulation = d3.forceSimulation(data.nodes).force("link", linkForce).force("charge", chargeForce).force("center", d3.forceCenter(0, 0)).force("collision", collisionForce).force("cluster", clusterForce()).force("containment", containmentForce()).alpha(C.alphaReset);
  var svg = d3.select("#graph-view").append("svg").attr("width", width).attr("height", height);
  svg.append("defs").append("marker").attr("id", "arrow").attr("viewBox", "0 -3 6 6").attr("refX", 12).attr("refY", 0).attr("markerWidth", 6).attr("markerHeight", 6).attr("orient", "auto").append("path").attr("d", "M0,-3L6,0L0,3").attr("fill", C.linkColorBase);
  var root = svg.append("g").attr("class", "zoom-root");
  var hullLayer = root.insert("g", ":first-child").attr("class", "hulls");
  var hullPath = d3.line().curve(d3.curveCatmullRomClosed.alpha(1));
  function renderHulls() {
    const sel = hullLayer.selectAll("path").data(activeClusters2(), (c) => c.key);
    sel.exit().remove();
    const entered = sel.enter().append("path").attr("fill-opacity", 0.12).attr("stroke-opacity", 0.55).attr("stroke-width", 2).attr("stroke-linejoin", "round");
    entered.merge(sel).attr("fill", (c) => c.color).attr("stroke", (c) => c.color);
    updateHullGeometry();
  }
  function updateHullGeometry() {
    hullLayer.selectAll("path").attr("d", (c) => {
      const pts = c.nodes.flatMap((n) => [
        [n.x + C.hullPad, n.y],
        [n.x - C.hullPad, n.y],
        [n.x, n.y + C.hullPad],
        [n.x, n.y - C.hullPad]
      ]);
      const hull = d3.polygonHull(pts);
      return hull ? `${hullPath(hull)}Z` : null;
    });
  }
  function drag(sim) {
    return d3.drag().on("start", (event, d) => {
      if (!event.active) sim.alphaTarget(C.alphaDragTarget).restart();
      d.fx = d.x;
      d.fy = d.y;
    }).on("drag", (event, d) => {
      d.fx = event.x;
      d.fy = event.y;
    }).on("end", (event, d) => {
      if (!event.active) sim.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    });
  }
  var link = root.append("g").attr("class", "links").attr("stroke", C.linkColorBase).attr("stroke-opacity", C.linkOpacityBase).selectAll("line").data(data.edges).join("line").attr("stroke-width", C.linkWidthBase).attr("marker-end", "url(#arrow)");
  var node = root.append("g").attr("class", "nodes").selectAll("circle").data(data.nodes).join("circle").attr("r", () => P.nodeRadius.value).attr("fill", (d) => folderColor.get(d.folder)).attr("stroke", "#fff").attr("stroke-width", 1.5).style("cursor", "pointer").call(drag(simulation));
  var label = root.append("g").attr("class", "labels").selectAll("text").data(data.nodes).join("text").text((d) => d.title).attr("font-size", () => P.labelFontSize.value).attr("font-family", "sans-serif").attr("dx", () => P.nodeRadius.value + 4).attr("dy", 4);
  node.on("mouseenter", (_event, d) => {
    const neighbors = adj.get(d.id);
    const visible = (n) => n.id === d.id || neighbors.has(n.id) ? 1 : C.nodeDimOpacity;
    const touchesD = (e) => {
      const sid = typeof e.source === "string" ? e.source : e.source.id;
      const tid = typeof e.target === "string" ? e.target : e.target.id;
      return sid === d.id || tid === d.id;
    };
    node.attr("opacity", visible);
    label.attr("opacity", visible);
    link.attr(
      "stroke",
      (e) => touchesD(e) ? C.linkColorHover : C.linkColorBase
    ).attr(
      "stroke-opacity",
      (e) => touchesD(e) ? 1 : C.linkOpacityDim
    ).attr(
      "stroke-width",
      (e) => touchesD(e) ? C.linkWidthHover : C.linkWidthBase
    );
  }).on("mouseleave", () => {
    node.attr("opacity", 1);
    label.attr("opacity", 1);
    link.attr("stroke", C.linkColorBase).attr("stroke-opacity", C.linkOpacityBase).attr("stroke-width", C.linkWidthBase);
  });
  node.on("click", (_event, d) => {
    window.location.href = d.href;
  });
  simulation.on("tick", () => {
    link.attr(
      "x1",
      (d) => typeof d.source === "string" ? 0 : d.source.x
    ).attr(
      "y1",
      (d) => typeof d.source === "string" ? 0 : d.source.y
    ).attr(
      "x2",
      (d) => typeof d.target === "string" ? 0 : d.target.x
    ).attr(
      "y2",
      (d) => typeof d.target === "string" ? 0 : d.target.y
    );
    node.attr("cx", (d) => d.x).attr("cy", (d) => d.y);
    label.attr("x", (d) => d.x).attr("y", (d) => d.y);
    updateHullGeometry();
  });
  var zoom = d3.zoom().scaleExtent(C.zoomScaleExtent).on("zoom", (event) => {
    root.attr("transform", event.transform);
  });
  svg.call(zoom);
  function fitToScreen() {
    for (let i = 0; i < C.fitSettleTicks; i++) simulation.tick();
    simulation.alpha(C.alphaSlider).restart();
    const curW = container.clientWidth || width;
    const curH = container.clientHeight || height;
    let minX = Infinity;
    let minY = Infinity;
    let maxX = -Infinity;
    let maxY = -Infinity;
    data.nodes.forEach((n) => {
      if (n.x < minX) minX = n.x;
      if (n.y < minY) minY = n.y;
      if (n.x > maxX) maxX = n.x;
      if (n.y > maxY) maxY = n.y;
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
  var backdrop = d3.select("body").append("div").attr("id", "graph-view-backdrop").on("click", () => toggleMaximize());
  function toggleMaximize() {
    const isMax = container.classList.toggle("maximized");
    backdrop.classed("visible", isMax);
    d3.select(".maximize-btn").text(isMax ? "\u2212" : "\u26F6");
    const w = container.clientWidth;
    const h = container.clientHeight;
    svg.attr("width", w).attr("height", h);
    fitToScreen();
  }
  var controls = d3.select("#graph-view").append("div").attr("class", "zoom-controls");
  controls.append("button").text("+").on("click", () => {
    svg.transition().duration(C.zoomTransitionMs).call(zoom.scaleBy, C.zoomButtonFactor);
  });
  controls.append("button").text("\u2212").on("click", () => {
    svg.transition().duration(C.zoomTransitionMs).call(zoom.scaleBy, 1 / C.zoomButtonFactor);
  });
  controls.append("button").text("\u2922").attr("title", "Fit").on("click", fitToScreen);
  controls.append("button").text("\u21BB").attr("title", "Reset layout").on("click", resetLayout);
  controls.append("button").text("\u26F6").attr("title", "Expand").attr("class", "maximize-btn").on("click", toggleMaximize);
  d3.select("#graph-view").append("button").attr("class", "maximize-close").attr("title", "Close").text("\u2715").on("click", toggleMaximize);
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && container.classList.contains("maximized")) {
      toggleMaximize();
    }
  });
  var panel = d3.select("#graph-view").append("div").attr("class", "settings-panel collapsed");
  var toggleBar = panel.append("div").attr("class", "panel-toggle");
  toggleBar.append("span").attr("class", "panel-toggle-label").text("Settings");
  toggleBar.append("span").attr("class", "panel-toggle-chevron").text("\u25B2");
  toggleBar.on("click", () => {
    const collapsed = panel.classed("collapsed");
    panel.classed("collapsed", !collapsed);
    panel.select(".panel-toggle-chevron").text(collapsed ? "\u25BC" : "\u25B2");
    const el = panel.node();
    el.style.width = "";
    el.style.height = "";
  });
  function makeCollapsibleSection(title, { collapsed: startCollapsed = false } = {}) {
    const section = panel.append("div").attr("class", "panel-section");
    const header = section.append("div").attr("class", "panel-header panel-header-toggle");
    header.append("span").text(title);
    const chevron = header.append("span").attr("class", "panel-toggle-chevron").text(startCollapsed ? "\u25B6" : "\u25BC");
    const body = section.append("div").attr("class", "panel-section-body").style("display", startCollapsed ? "none" : null);
    header.on("click", () => {
      const isHidden = body.style("display") === "none";
      body.style("display", isHidden ? null : "none");
      chevron.text(isHidden ? "\u25BC" : "\u25B6");
    });
    return body;
  }
  var sliderBody = makeCollapsibleSection("Sliders", { collapsed: true });
  function makeSlider(p, onChange) {
    const row = sliderBody.append("div").attr("class", "panel-row");
    row.append("label").text(p.label);
    const input = row.append("input").attr("type", "range").attr("min", p.min).attr("max", p.max).attr("step", p.step).attr("value", p.value);
    const valSpan = row.append("span").attr("class", "slider-val").text(p.value.toFixed(p.decimals));
    input.on("input", function() {
      p.value = +this.value;
      valSpan.text(p.value.toFixed(p.decimals));
      onChange?.(p.value);
      simulation.alpha(C.alphaSlider).restart();
    });
  }
  makeSlider(P.nodeRadius, (v) => {
    node.attr("r", () => v);
    label.attr("dx", () => v + 4);
    simulation.force(
      "collision",
      d3.forceCollide().radius(() => P.nodeRadius.value + P.collisionPadding.value)
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
  function setVisible(keys, on) {
    for (const k of keys) {
      if (on) visibleKeys.add(k);
      else visibleKeys.delete(k);
    }
    panel.selectAll("input.cluster-cb").property("checked", function() {
      return visibleKeys.has(this.dataset.key);
    });
    renderHulls();
    simulation.alpha(C.alphaReset).restart();
  }
  function buildSection(title, clusters) {
    const section = panel.append("div").attr("class", "panel-section");
    const header = section.append("div").attr("class", "panel-header panel-header-toggle");
    header.append("span").text(title);
    const rightGroup = header.append("span").style("display", "flex").style("align-items", "center").style("gap", "6px");
    const actions = rightGroup.append("span").attr("class", "panel-actions");
    actions.append("button").text("all").on("click", (event) => {
      event.stopPropagation();
      setVisible(
        clusters.map((c) => c.key),
        true
      );
    });
    actions.append("button").text("none").on("click", (event) => {
      event.stopPropagation();
      setVisible(
        clusters.map((c) => c.key),
        false
      );
    });
    const chevron = rightGroup.append("span").attr("class", "panel-toggle-chevron").text("\u25BC");
    const body = section.append("div").attr("class", "panel-section-body");
    header.on("click", () => {
      const isHidden = body.style("display") === "none";
      body.style("display", isHidden ? null : "none");
      chevron.text(isHidden ? "\u25BC" : "\u25B6");
    });
    const list = body.append("div").attr("class", "panel-list");
    const items = list.selectAll("label").data(clusters).join("label").attr("class", "cluster-item").attr("title", (c) => `${c.label} (${c.nodes.length} nodes)`);
    items.append("input").attr("type", "checkbox").attr("class", "cluster-cb").property("checked", (c) => visibleKeys.has(c.key)).each(function(c) {
      this.dataset.key = c.key;
    }).on("change", function(_event, c) {
      if (this.checked) visibleKeys.add(c.key);
      else visibleKeys.delete(c.key);
      renderHulls();
      simulation.alpha(C.alphaSlider).restart();
    });
    items.append("span").attr("class", "swatch").style("background", (c) => c.color);
    items.append("span").attr("class", "cluster-label").text((c) => `${c.label} (${c.nodes.length})`);
  }
  buildSection("Folders", folderClusters);
  buildSection("Tags", tagClusters);
  renderHulls();
})();
