/**
 Graph view using D3.js.
 Precondition:
    - graph JSON data is in window.__graphData
 */
(() => {
  // Global config
  const config = {
    nodeRadius: 14,
    labelFontSize: 16,
  };

  const data = window.__graphData;
  console.log(
    "[graph-view] nodes:",
    data.nodes.length,
    "edges:",
    data.edges.length,
  );
  console.log("[graph-view] sample edge:", data.edges[0]);
  const container = document.getElementById("graph-view");
  const width = container.clientWidth;
  const height = container.clientHeight;

  // Visible debug counter
  const debugEl = document.createElement("div");
  debugEl.className = "debug-info";
  debugEl.textContent = `nodes: ${data.nodes.length}  edges: ${data.edges.length}`;
  container.appendChild(debugEl);

  // Adjacency for hover highlighting
  const adj = new Map();
  data.nodes.forEach((n) => {
    adj.set(n.id, new Set());
  });
  data.edges.forEach((e) => {
    adj.get(e.source).add(e.target);
    adj.get(e.target).add(e.source);
  });

  // Color by folder
  const folders = [...new Set(data.nodes.map((n) => n.folder))];
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
  const folderColor = new Map(
    folders.map((f, i) => [f, palette[i % palette.length]]),
  );

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
        .id((d) => d.id)
        .distance(80),
    )
    .force("charge", d3.forceManyBody().strength(-150))
    .force("center", d3.forceCenter(0, 0))
    .force("collision", d3.forceCollide().radius(18));

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
    .attr("fill", (d) => folderColor.get(d.folder))
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
    .text((d) => d.title)
    .attr("font-size", config.labelFontSize)
    .attr("font-family", "sans-serif")
    .attr("dx", config.nodeRadius + 4)
    .attr("dy", 4);

  // Hover: dim unrelated, highlight connected edges
  node
    .on("mouseenter", (_event, d) => {
      const neighbors = adj.get(d.id);
      node.attr("opacity", (n) =>
        n.id === d.id || neighbors.has(n.id) ? 1 : 0.15,
      );
      label.attr("opacity", (n) =>
        n.id === d.id || neighbors.has(n.id) ? 1 : 0.15,
      );
      link
        .attr("stroke", (e) => {
          const sid = e.source.id || e.source;
          const tid = e.target.id || e.target;
          return sid === d.id || tid === d.id ? "#ffcc00" : "#e0e0e0";
        })
        .attr("stroke-opacity", (e) => {
          const sid = e.source.id || e.source;
          const tid = e.target.id || e.target;
          return sid === d.id || tid === d.id ? 1 : 0.15;
        })
        .attr("stroke-width", (e) => {
          const sid = e.source.id || e.source;
          const tid = e.target.id || e.target;
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
  node.on("click", (_event, d) => {
    const url = d.id.replace(/\.md$/, ".html");
    window.location.href = url;
  });

  // Tick
  simulation.on("tick", () => {
    link
      .attr("x1", (d) => d.source.x)
      .attr("y1", (d) => d.source.y)
      .attr("x2", (d) => d.target.x)
      .attr("y2", (d) => d.target.y);
    node.attr("cx", (d) => d.x).attr("cy", (d) => d.y);
    label.attr("x", (d) => d.x).attr("y", (d) => d.y);
  });

  // Zoom + pan
  const zoom = d3
    .zoom()
    .scaleExtent([0.1, 8])
    .on("zoom", (event) => {
      root.attr("transform", event.transform);
    });
  svg.call(zoom);

  // Fit-to-screen: run simulation ahead, compute bounds, set transform
  function fitToScreen() {
    // Tick until the layout is roughly settled
    for (let i = 0; i < 300; i++) simulation.tick();
    simulation.alpha(0.1).restart();

    let minX = Infinity,
      minY = Infinity,
      maxX = -Infinity,
      maxY = -Infinity;
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

  // Drag behavior — coords already in root's local space thanks to d3.zoom
  function drag(simulation) {
    return d3
      .drag()
      .on("start", (event, d) => {
        if (!event.active) simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
      })
      .on("drag", (event, d) => {
        d.fx = event.x;
        d.fy = event.y;
      })
      .on("end", (event, d) => {
        if (!event.active) simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
      });
  }
})();
