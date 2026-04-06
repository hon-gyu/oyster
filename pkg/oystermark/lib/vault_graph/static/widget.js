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

  // widget.ts
  var graphConfig = window.__graphConfig ?? {
    dir: "all",
    tag: "all",
    default_dir: { include: ["*"] },
    default_tag: { include: ["*"] }
  };
  var config = {
    // Visual
    nodeRadius: 14,
    // circle radius in px
    labelFontSize: 16,
    // px
    // Force simulation
    linkDistance: 35,
    // target edge length; smaller = tighter graph
    chargeStrength: -80,
    // node-node repulsion; less negative = tighter
    collisionPadding: 6,
    // collision radius = nodeRadius + this
    // Initial seed positions (folder-based pre-layout)
    seedRadiusBase: 120,
    // minimum radius of the folder ring
    seedRadiusPerFolder: 30,
    // extra radius per folder so big vaults spread out
    seedJitter: 60,
    // px of random offset within each folder cluster
    // Cluster attractive force
    clusterStrength: 0.08
    // initial pull strength when a cluster is active
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
  var adj = /* @__PURE__ */ new Map();
  data.nodes.forEach((n) => {
    adj.set(n.id, /* @__PURE__ */ new Set());
  });
  data.edges.forEach((e) => {
    const sid = typeof e.source === "string" ? e.source : e.source.id;
    const tid = typeof e.target === "string" ? e.target : e.target.id;
    adj.get(sid).add(tid);
    adj.get(tid).add(sid);
  });
  var folders = [...new Set(data.nodes.map((n) => n.folder))];
  var palette = [
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
  ];
  var folderColor = new Map(
    folders.map((f, i) => [f, palette[i % palette.length]])
  );
  function seedNodePositions() {
    const seedRadius = Math.max(
      config.seedRadiusBase,
      folders.length * config.seedRadiusPerFolder
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
      n.x = hx + (Math.random() - 0.5) * config.seedJitter;
      n.y = hy + (Math.random() - 0.5) * config.seedJitter;
    });
  }
  seedNodePositions();
  var svg = d3.select("#graph-view").append("svg").attr("width", width).attr("height", height);
  svg.append("defs").append("marker").attr("id", "arrow").attr("viewBox", "0 -3 6 6").attr("refX", 12).attr("refY", 0).attr("markerWidth", 6).attr("markerHeight", 6).attr("orient", "auto").append("path").attr("d", "M0,-3L6,0L0,3").attr("fill", "#e0e0e0");
  var root = svg.append("g").attr("class", "zoom-root");
  var simulation = d3.forceSimulation(data.nodes).force(
    "link",
    d3.forceLink(data.edges).id((d) => d.id).distance(config.linkDistance)
  ).force("charge", d3.forceManyBody().strength(config.chargeStrength)).force("center", d3.forceCenter(0, 0)).force(
    "collision",
    d3.forceCollide().radius(config.nodeRadius + config.collisionPadding)
  ).force("cluster", clusterForce());
  function clusterForce() {
    function force(alpha) {
      if (clusterStrength === 0) return;
      for (const c of activeClusters()) {
        let cx = 0;
        let cy = 0;
        for (const n of c.nodes) {
          cx += n.x;
          cy += n.y;
        }
        cx /= c.nodes.length;
        cy /= c.nodes.length;
        const k = clusterStrength * alpha;
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
  var tagPalette = [
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
  ];
  var allTags = [
    ...new Set(data.nodes.flatMap((n) => n.tags || []))
  ];
  var tagColor = new Map(
    allTags.map((t, i) => [t, tagPalette[i % tagPalette.length]])
  );
  var folderClusters = [
    ...new Set(data.nodes.map((n) => n.folder))
  ].filter((folder) => selectorMatches(graphConfig.dir, folder)).map((folder) => ({
    key: `folder:${folder}`,
    kind: "folder",
    label: folder,
    color: folderColor.get(folder),
    nodes: data.nodes.filter((n) => n.folder === folder)
  }));
  var tagClusters = allTags.filter((tag) => selectorMatches(graphConfig.tag, tag)).map((tag) => ({
    key: `tag:${tag}`,
    kind: "tag",
    label: `#${tag}`,
    color: tagColor.get(tag),
    nodes: data.nodes.filter((n) => (n.tags || []).includes(tag))
  }));
  var visibleKeys = /* @__PURE__ */ new Set();
  for (const c of folderClusters) {
    if (selectorMatches(graphConfig.default_dir, c.label))
      visibleKeys.add(c.key);
  }
  for (const c of tagClusters) {
    const bare = c.label.replace(/^#/, "");
    if (selectorMatches(graphConfig.default_tag, bare)) visibleKeys.add(c.key);
  }
  var clusterStrength = config.clusterStrength;
  var hullLayer = root.insert("g", ":first-child").attr("class", "hulls");
  var HULL_PAD = 24;
  var hullPath = d3.line().curve(d3.curveCatmullRomClosed.alpha(1));
  var allClusters = [...folderClusters, ...tagClusters];
  function activeClusters() {
    return allClusters.filter(
      (c) => visibleKeys.has(c.key) && c.nodes.length >= 2
    );
  }
  function renderHulls() {
    const sel = hullLayer.selectAll("path").data(activeClusters(), (c) => c.key);
    sel.exit().remove();
    const entered = sel.enter().append("path").attr("fill-opacity", 0.12).attr("stroke-opacity", 0.55).attr("stroke-width", 2).attr("stroke-linejoin", "round");
    entered.merge(sel).attr("fill", (c) => c.color).attr("stroke", (c) => c.color);
    updateHullGeometry();
  }
  function updateHullGeometry() {
    hullLayer.selectAll("path").attr("d", (c) => {
      const pts = c.nodes.flatMap((n) => [
        [n.x + HULL_PAD, n.y],
        [n.x - HULL_PAD, n.y],
        [n.x, n.y + HULL_PAD],
        [n.x, n.y - HULL_PAD]
      ]);
      const hull = d3.polygonHull(pts);
      return hull ? `${hullPath(hull)}Z` : null;
    });
  }
  var link = root.append("g").attr("class", "links").attr("stroke", "#e0e0e0").attr("stroke-opacity", 0.8).selectAll("line").data(data.edges).join("line").attr("stroke-width", 1.5).attr("marker-end", "url(#arrow)");
  var node = root.append("g").attr("class", "nodes").selectAll("circle").data(data.nodes).join("circle").attr("r", config.nodeRadius).attr("fill", (d) => folderColor.get(d.folder)).attr("stroke", "#fff").attr("stroke-width", 1.5).style("cursor", "pointer").call(drag(simulation));
  var label = root.append("g").attr("class", "labels").selectAll("text").data(data.nodes).join("text").text((d) => d.title).attr("font-size", config.labelFontSize).attr("font-family", "sans-serif").attr("dx", config.nodeRadius + 4).attr("dy", 4);
  node.on("mouseenter", (_event, d) => {
    const neighbors = adj.get(d.id);
    node.attr(
      "opacity",
      (n) => n.id === d.id || neighbors.has(n.id) ? 1 : 0.15
    );
    label.attr(
      "opacity",
      (n) => n.id === d.id || neighbors.has(n.id) ? 1 : 0.15
    );
    link.attr("stroke", (e) => {
      const sid = typeof e.source === "string" ? e.source : e.source.id;
      const tid = typeof e.target === "string" ? e.target : e.target.id;
      return sid === d.id || tid === d.id ? "#ffcc00" : "#e0e0e0";
    }).attr("stroke-opacity", (e) => {
      const sid = typeof e.source === "string" ? e.source : e.source.id;
      const tid = typeof e.target === "string" ? e.target : e.target.id;
      return sid === d.id || tid === d.id ? 1 : 0.15;
    }).attr("stroke-width", (e) => {
      const sid = typeof e.source === "string" ? e.source : e.source.id;
      const tid = typeof e.target === "string" ? e.target : e.target.id;
      return sid === d.id || tid === d.id ? 2.5 : 1.5;
    });
  }).on("mouseleave", () => {
    node.attr("opacity", 1);
    label.attr("opacity", 1);
    link.attr("stroke", "#e0e0e0").attr("stroke-opacity", 0.8).attr("stroke-width", 1.5);
  });
  node.on("click", (_event, d) => {
    const url = d.id.replace(/\.md$/, ".html");
    window.location.href = url;
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
  var zoom = d3.zoom().scaleExtent([0.1, 8]).on("zoom", (event) => {
    root.attr("transform", event.transform);
  });
  svg.call(zoom);
  function fitToScreen() {
    for (let i = 0; i < 300; i++) simulation.tick();
    simulation.alpha(0.1).restart();
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
    data.nodes.forEach((n) => {
      if (n.x < minX) minX = n.x;
      if (n.y < minY) minY = n.y;
      if (n.x > maxX) maxX = n.x;
      if (n.y > maxY) maxY = n.y;
    });
    const pad = 60;
    const dx = maxX - minX + pad * 2;
    const dy = maxY - minY + pad * 2;
    const cx = (minX + maxX) / 2;
    const cy = (minY + maxY) / 2;
    const scale = Math.min(width / dx, height / dy, 2);
    const tx = width / 2 - scale * cx;
    const ty = height / 2 - scale * cy;
    svg.call(zoom.transform, d3.zoomIdentity.translate(tx, ty).scale(scale));
  }
  fitToScreen();
  var controls = d3.select("#graph-view").append("div").attr("class", "zoom-controls");
  controls.append("button").text("+").on("click", () => {
    svg.transition().duration(200).call(zoom.scaleBy, 1.4);
  });
  controls.append("button").text("\u2212").on("click", () => {
    svg.transition().duration(200).call(zoom.scaleBy, 1 / 1.4);
  });
  controls.append("button").text("\u2922").attr("title", "Fit").on("click", () => {
    fitToScreen();
  });
  var panel = d3.select("#graph-view").append("div").attr("class", "cluster-panel collapsed");
  var toggle = panel.append("div").attr("class", "panel-toggle");
  toggle.append("span").attr("class", "panel-toggle-label").text("Clusters");
  toggle.append("span").attr("class", "panel-toggle-chevron").text("\u25B2");
  toggle.on("click", () => {
    const collapsed = panel.classed("collapsed");
    panel.classed("collapsed", !collapsed);
    panel.select(".panel-toggle-chevron").text(collapsed ? "\u25BC" : "\u25B2");
  });
  var strengthRow = panel.append("div").attr("class", "panel-row");
  strengthRow.append("label").text("Cluster pull");
  var strengthInput = strengthRow.append("input").attr("type", "range").attr("min", 0).attr("max", 0.3).attr("step", 0.01).attr("value", clusterStrength).on("input", function() {
    clusterStrength = +this.value;
    simulation.alpha(0.5).restart();
  });
  strengthRow.append("span").attr("class", "strength-val").text(clusterStrength);
  strengthInput.on("input.label", function() {
    strengthRow.select(".strength-val").text((+this.value).toFixed(2));
  });
  function setVisible(keys, on) {
    for (const k of keys) {
      if (on) visibleKeys.add(k);
      else visibleKeys.delete(k);
    }
    panel.selectAll("input.cluster-cb").property("checked", function() {
      return visibleKeys.has(this.dataset.key);
    });
    renderHulls();
    simulation.alpha(0.5).restart();
  }
  function buildSection(title, clusters) {
    const section = panel.append("div").attr("class", "panel-section");
    const header = section.append("div").attr("class", "panel-header");
    header.append("span").text(title);
    const actions = header.append("span").attr("class", "panel-actions");
    actions.append("button").text("all").on(
      "click",
      () => setVisible(
        clusters.map((c) => c.key),
        true
      )
    );
    actions.append("button").text("none").on(
      "click",
      () => setVisible(
        clusters.map((c) => c.key),
        false
      )
    );
    const list = section.append("div").attr("class", "panel-list");
    const items = list.selectAll("label").data(clusters).join("label").attr("class", "cluster-item").attr("title", (c) => `${c.label} (${c.nodes.length} nodes)`);
    items.append("input").attr("type", "checkbox").attr("class", "cluster-cb").property("checked", (c) => visibleKeys.has(c.key)).each(function(c) {
      this.dataset.key = c.key;
    }).on(
      "change",
      function(_event, c) {
        if (this.checked) visibleKeys.add(c.key);
        else visibleKeys.delete(c.key);
        renderHulls();
        simulation.alpha(0.5).restart();
      }
    );
    items.append("span").attr("class", "swatch").style("background", (c) => c.color);
    items.append("span").attr("class", "cluster-label").text((c) => `${c.label} (${c.nodes.length})`);
  }
  buildSection("Folders", folderClusters);
  buildSection("Tags", tagClusters);
  renderHulls();
  function drag(simulation2) {
    return d3.drag().on("start", (event, d) => {
      if (!event.active) simulation2.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }).on("drag", (event, d) => {
      d.fx = event.x;
      d.fy = event.y;
    }).on("end", (event, d) => {
      if (!event.active) simulation2.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    });
  }
})();
