(** Graph view: visual rendering of a {!Vault_graph.t}. *)

open Core
module J = Yojson.Basic

module String_pair = struct
  module T = struct
    type t = string * string [@@deriving sexp, compare]
  end

  include T
  include Comparator.Make (T)
end

(* JSON output
   ==================== *)

(** Collapse the graph to note-level: one node per path, deduplicated edges. *)
let to_json (t : Vault_graph.t) : string =
  (* Collect note-level nodes *)
  let nodes : J.t list =
    Vault_graph.G.fold_vertex
      (fun (v : Vault_graph.vertex) acc ->
        match v.kind with
        | Note ->
          let meta =
            Map.find t.meta v.path
            |> Option.value
                 ~default:Vault_graph.{ title = v.path; tags = []; folder = "." }
          in
          `Assoc
            [ "id", `String v.path
            ; "title", `String meta.title
            ; "tags", `List (List.map meta.tags ~f:(fun t -> `String t))
            ; "folder", `String meta.folder
            ]
          :: acc
        | _ -> acc)
      t.graph
      []
  in
  (* Collect note-level edges (deduplicated) *)
  let edge_set =
    Vault_graph.G.fold_edges_e
      (fun (src, _kind, tgt) acc ->
        if String.equal src.path tgt.path
        then acc
        else Set.add acc (src.path, tgt.path))
      t.graph
      (Set.empty (module String_pair))
  in
  let edges : J.t list =
    Set.to_list edge_set
    |> List.map ~f:(fun (src, tgt) ->
      `Assoc [ "source", `String src; "target", `String tgt ])
  in
  J.pretty_to_string (`Assoc [ "nodes", `List nodes; "edges", `List edges ])
;;

(* HTML output
   ==================== *)

let widget_js : string =
  {js|
(function() {
  const data = window.__graphData;
  const container = document.getElementById("graph-view");
  const width = container.clientWidth;
  const height = container.clientHeight;

  // Node index for edge lookups
  const nodeById = new Map(data.nodes.map(n => [n.id, n]));

  // Adjacency for hover highlighting
  const adj = new Map();
  data.nodes.forEach(n => adj.set(n.id, new Set()));
  data.edges.forEach(e => {
    adj.get(e.source).add(e.target);
    adj.get(e.target).add(e.source);
  });

  // Color by folder
  const folders = [...new Set(data.nodes.map(n => n.folder))];
  const palette = [
    "#4e79a7","#f28e2b","#e15759","#76b7b2","#59a14f",
    "#edc948","#b07aa1","#ff9da7","#9c755f","#bab0ac"
  ];
  const folderColor = new Map(folders.map((f, i) => [f, palette[i % palette.length]]));

  // SVG setup
  const svg = d3.select("#graph-view")
    .append("svg")
    .attr("width", width)
    .attr("height", height);

  // Arrow marker
  svg.append("defs").append("marker")
    .attr("id", "arrow")
    .attr("viewBox", "0 -3 6 6")
    .attr("refX", 16)
    .attr("refY", 0)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M0,-3L6,0L0,3")
    .attr("fill", "#999");

  // Force simulation
  const simulation = d3.forceSimulation(data.nodes)
    .force("link", d3.forceLink(data.edges).id(d => d.id).distance(100))
    .force("charge", d3.forceManyBody().strength(-200))
    .force("center", d3.forceCenter(width / 2, height / 2))
    .force("collision", d3.forceCollide().radius(20));

  // Edges
  const link = svg.append("g")
    .selectAll("line")
    .data(data.edges)
    .join("line")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.5)
    .attr("stroke-width", 1)
    .attr("marker-end", "url(#arrow)");

  // Nodes
  const node = svg.append("g")
    .selectAll("circle")
    .data(data.nodes)
    .join("circle")
    .attr("r", 6)
    .attr("fill", d => folderColor.get(d.folder))
    .attr("stroke", "#fff")
    .attr("stroke-width", 1.5)
    .style("cursor", "pointer")
    .call(drag(simulation));

  // Labels
  const label = svg.append("g")
    .selectAll("text")
    .data(data.nodes)
    .join("text")
    .text(d => d.title)
    .attr("font-size", 10)
    .attr("font-family", "sans-serif")
    .attr("dx", 10)
    .attr("dy", 3)
    .attr("fill", "#333");

  // Hover: highlight connected edges
  node.on("mouseenter", function(event, d) {
    const neighbors = adj.get(d.id);
    node.attr("opacity", n => n.id === d.id || neighbors.has(n.id) ? 1 : 0.15);
    label.attr("opacity", n => n.id === d.id || neighbors.has(n.id) ? 1 : 0.15);
    link.attr("stroke-opacity", e => {
      const sid = typeof e.source === "object" ? e.source.id : e.source;
      const tid = typeof e.target === "object" ? e.target.id : e.target;
      return sid === d.id || tid === d.id ? 1 : 0.05;
    }).attr("stroke-width", e => {
      const sid = typeof e.source === "object" ? e.source.id : e.source;
      const tid = typeof e.target === "object" ? e.target.id : e.target;
      return sid === d.id || tid === d.id ? 2 : 1;
    });
  }).on("mouseleave", function() {
    node.attr("opacity", 1);
    label.attr("opacity", 1);
    link.attr("stroke-opacity", 0.5).attr("stroke-width", 1);
  });

  // Click: navigate
  node.on("click", function(event, d) {
    const url = d.id.replace(/\.md$/, ".html");
    window.location.href = url;
  });

  // Tick
  simulation.on("tick", () => {
    link
      .attr("x1", d => d.source.x)
      .attr("y1", d => d.source.y)
      .attr("x2", d => d.target.x)
      .attr("y2", d => d.target.y);
    node.attr("cx", d => d.x).attr("cy", d => d.y);
    label.attr("x", d => d.x).attr("y", d => d.y);
  });

  // Drag behavior
  function drag(simulation) {
    return d3.drag()
      .on("start", (event, d) => {
        if (!event.active) simulation.alphaTarget(0.3).restart();
        d.fx = d.x; d.fy = d.y;
      })
      .on("drag", (event, d) => {
        d.fx = event.x; d.fy = event.y;
      })
      .on("end", (event, d) => {
        if (!event.active) simulation.alphaTarget(0);
        d.fx = null; d.fy = null;
      });
  }
})();
|js}
;;

let to_html (t : Vault_graph.t) : string =
  let json = to_json t in
  Printf.sprintf
    {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Graph View</title>
<style>
  * { margin: 0; padding: 0; box-sizing: border-box; }
  body { background: #1a1a2e; overflow: hidden; }
  #graph-view { width: 100vw; height: 100vh; }
  text { fill: #ccc; }
</style>
</head>
<body>
<div id="graph-view"></div>
<script src="https://d3js.org/d3.v7.min.js"></script>
<script>
window.__graphData = %s;
</script>
<script>
%s
</script>
</body>
</html>|}
    json
    widget_js
;;
