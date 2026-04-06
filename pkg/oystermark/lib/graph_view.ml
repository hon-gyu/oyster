(** Graph view: visual rendering (d3.js) of a {!Vault_graph.t}. *)

(* TODO: refactor graph_view
  graph_view/
    `- widget.js
    `- style.css
    `- test/
       `- data.json
  use blob for style css
*)

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

let widget_js : string = [%blob "static/graph_view.js"];;

let%expect_test "to_json with cross-note links" =
  let vault =
    Vault.of_inmem_files
      ~vault_root:"/tmp_vault"
      [ "a.md", "link to [[b]]"; "b.md", "link to [[a]]" ]
  in
  let g = Vault_graph.of_vault vault in
  print_endline (to_json g);
  [%expect
    {|
    {
      "nodes": [
        { "id": "b.md", "title": "b", "tags": [], "folder": "." },
        { "id": "a.md", "title": "a", "tags": [], "folder": "." }
      ],
      "edges": [
        { "source": "a.md", "target": "b.md" },
        { "source": "b.md", "target": "a.md" }
      ]
    }
    |}]
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
  body { background: #1a1a2e; overflow: hidden; font-family: sans-serif; }
  #graph-view { width: 100vw; height: 100vh; position: relative; }
  #graph-view svg { display: block; }
  text { fill: #ccc; pointer-events: none; user-select: none; }
  .zoom-controls {
    position: absolute;
    top: 16px;
    right: 16px;
    display: flex;
    flex-direction: column;
    gap: 4px;
    z-index: 10;
  }
  .zoom-controls button {
    width: 32px;
    height: 32px;
    border: 1px solid #555;
    background: #2a2a3e;
    color: #eee;
    font-size: 16px;
    cursor: pointer;
    border-radius: 4px;
  }
  .zoom-controls button:hover { background: #3a3a4e; }
  .debug-info {
    position: absolute;
    top: 16px;
    left: 16px;
    color: #eee;
    background: #2a2a3e;
    padding: 6px 10px;
    border-radius: 4px;
    font-size: 12px;
    font-family: monospace;
    z-index: 10;
  }
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
