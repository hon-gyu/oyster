(** Graph view: visual rendering (d3.js) of a {!Common.t}. *)

(* TODO: refactor graph_view
  graph_view/
    `- widget.js
    `- style.css
    `- test/
       `- data.json
  use blob for style css
*)

open Core
open Common
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
let to_json (t : t) : string =
  (* Collect note-level nodes *)
  let nodes : J.t list =
    G.fold_vertex
      (fun (v : vertex) acc ->
         match v.kind with
         | Note ->
           let meta =
             Map.find t.meta v.path
             |> Option.value ~default:{ title = v.path; tags = []; folder = "." }
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
    G.fold_edges_e
      (fun (src, _kind, tgt) acc ->
         if String.equal src.path tgt.path then acc else Set.add acc (src.path, tgt.path))
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

let widget_js : string = [%blob "../static/graph_view/widget.js"]
let widget_css : string = [%blob "../static/graph_view/style.css"]

let%expect_test "to_json with cross-note links" =
  let vault =
    Vault.of_inmem_files
      ~vault_root:"/tmp_vault"
      [ "a.md", "link to [[b]]"; "b.md", "link to [[a]]" ]
  in
  let g = of_vault vault in
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

(** Embeddable widget HTML fragment (style + container + scripts).
    Suitable for inlining into an existing page via an [=html] code block. *)
let to_widget_html (t : t) : string =
  let json = to_json t in
  Printf.sprintf
    {|<style>
%s
</style>
<div id="graph-view"></div>
<script src="https://d3js.org/d3.v7.min.js"></script>
<script>
window.__graphData = %s;
</script>
<script>
%s
</script>|}
    widget_css
    json
    widget_js
;;

let to_html (t : t) : string =
  let json = to_json t in
  Printf.sprintf
    {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<title>Graph View</title>
<style>
%s
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
    widget_css
    json
    widget_js
;;

(*
(** Pipeline step: append an interactive graph widget to [home.md]. *)
let home_graph : Pipeline.t =
  let on_vault (ctx : Vault.t) : Vault.t =
    let g = of_vault ctx in
    let html = to_widget_html g in
    let docs =
      List.map ctx.docs ~f:(fun (path, doc) ->
        if not (String.equal path "home.md")
        then path, doc
        else (
          let block_mapper = Pipeline.add_html_code_block `Append html in
          let mapper =
            Cmarkit.Mapper.make
              ~inline_ext_default:(fun _m i -> Some i)
              ~block_ext_default:(fun _m b -> Some b)
              ~block:block_mapper
              ()
          in
          path, Cmarkit.Mapper.map_doc mapper doc))
    in
    { ctx with docs }
  in
  Pipeline.make ~on_vault ()
;; *)
