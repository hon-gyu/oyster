(** Graph extracted from a resolved vault. *)

open Core

(* Edge kind
====================  *)

type edge_kind =
  | To_note
  | To_heading of
      { heading : string
      ; slug : string
      }
  | To_block of { block_id : string }
[@@deriving sexp, compare]

type vertex = string

(* Graph
==================== *)

module G =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
    (String)
    (struct
      type t = edge_kind [@@deriving compare]

      let default = To_note
    end)

type t = G.t

(* Edge extraction
==================== *)

(** Extract [target_path, edge_kind] from a resolved target.
    Returns [None] for self-links and unresolved. *)
let edge_of_resolved (target : Vault.Resolve.target) : (string * edge_kind) option =
  match target with
  | Vault.Resolve.Note { path } -> Some (path, To_note)
  | Vault.Resolve.File { path } -> Some (path, To_note)
  | Vault.Resolve.Heading { path; heading; slug; _ } ->
    Some (path, To_heading { heading; slug })
  | Vault.Resolve.Block { path; block_id } -> Some (path, To_block { block_id })
  | Vault.Resolve.Curr_file
  | Vault.Resolve.Curr_heading _
  | Vault.Resolve.Curr_block _
  | Vault.Resolve.Unresolved -> None
;;

(** Collect all [(target_path, edge_kind)] pairs from a single document. *)
let collect_edges_from_doc (doc : Cmarkit.Doc.t) : (string * edge_kind) list =
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun _f acc (i : Cmarkit.Inline.t) ->
        match i with
        | Cmarkit.Inline.Link (_, meta) | Cmarkit.Inline.Image (_, meta) ->
          (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
           | Some resolved ->
             (match edge_of_resolved resolved with
              | Some edge -> Cmarkit.Folder.ret (edge :: acc)
              | None -> Cmarkit.Folder.default)
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Parse.Wikilink.Ext_wikilink (_, meta) ->
          (match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
           | Some resolved ->
             (match edge_of_resolved resolved with
              | Some edge -> edge :: acc
              | None -> acc)
           | None -> acc)
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  List.rev (Cmarkit.Folder.fold_doc folder [] doc)
;;

(* Build graph
-------------------- *)

let of_vault (vault : Vault.t) : t =
  let g = G.empty in
  (* Add all note nodes *)
  let g =
    List.fold vault.docs ~init:g ~f:(fun g (rel_path, _doc) -> G.add_vertex g rel_path)
  in
  (* Add edges *)
  List.fold vault.docs ~init:g ~f:(fun g (src_path, doc) ->
    let edges = collect_edges_from_doc doc in
    List.fold edges ~init:g ~f:(fun g (dst_path, kind) ->
      if String.equal src_path dst_path
      then g (* skip self-loops *)
      else G.add_edge_e g (src_path, kind, dst_path)))
;;

(* DOT output
==================== *)

module Dot = Graph.Graphviz.Dot (struct
    type nonrec t = t

    module V = G.V
    module E = G.E

    let iter_vertex = G.iter_vertex
    let iter_edges_e = G.iter_edges_e
    let graph_attributes _g = [ `Rankdir `LeftToRight ]

    let default_vertex_attributes _g =
      [ `Shape `Box; `Style `Rounded; `Fontname "sans-serif"; `Fontsize 10 ]
    ;;

    let vertex_name v = Printf.sprintf "%S" v

    let vertex_attributes v =
      let label = String.chop_suffix_exn ~suffix:".md" v |> Filename.basename in
      [ `Label label ]
    ;;

    let get_subgraph _v = None

    let default_edge_attributes _g =
      [ `Arrowsize 0.5; `Color 0x666666; `Fontsize 8; `Fontname "sans-serif" ]
    ;;

    let edge_attributes (_src, kind, _dst) =
      match kind with
      | To_note -> []
      | To_heading { heading; _ } -> [ `Label heading ]
      | To_block { block_id } -> [ `Label ("^" ^ block_id) ]
    ;;
  end)

let to_dot (g : t) : string =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Dot.fprint_graph fmt g;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
;;

(* Tests
==================== *)

let%test_module "graph" =
  (module struct
    (** Build a minimal resolved vault from markdown strings. *)
    let build_vault (files : (string * string) list) : Vault.t =
      let docs =
        List.map files ~f:(fun (path, content) -> path, Parse.of_string content)
      in
      let index = Vault.build_index ~md_docs:docs ~other_files:[] ~dirs:[] in
      let resolved_docs = Vault.Resolve.resolve_docs docs index in
      { vault_root = "/test"
      ; index
      ; docs = resolved_docs
      ; vault_meta = Cmarkit.Meta.none
      }
    ;;

    let show_edges (g : t) =
      G.iter_edges_e
        (fun (src, kind, dst) ->
           let kind_s = Sexp.to_string (sexp_of_edge_kind kind) in
           printf "%s --%s--> %s\n" src kind_s dst)
        g
    ;;

    let%expect_test "simple wikilinks" =
      let vault =
        build_vault
          [ "a.md", "link to [[b]]"; "b.md", "link to [[a]]"; "c.md", "no links here" ]
      in
      let g = of_vault vault in
      show_edges g;
      [%expect
        {|
        a.md --To_note--> b.md
        b.md --To_note--> a.md
        |}]
    ;;

    let%expect_test "heading link" =
      let vault =
        build_vault [ "a.md", "see [[b#Section]]"; "b.md", "# Section\nsome content" ]
      in
      let g = of_vault vault in
      show_edges g;
      [%expect
        {| a.md --(To_heading(heading Section)(slug section))--> b.md |}]
    ;;

    let%expect_test "self-links skipped" =
      let vault = build_vault [ "a.md", "see [[a]] and [[#heading]]" ] in
      let g = of_vault vault in
      show_edges g;
      [%expect {| |}]
    ;;

    let%expect_test "dot output" =
      let vault =
        build_vault [ "notes/a.md", "link to [[b]]"; "b.md", "link to [[a]]" ]
      in
      let g = of_vault vault in
      print_string (to_dot g);
      [%expect
        {|
        digraph G {
          rankdir=LR;
          node [fontsize=10, fontname="sans-serif", shape=box, style="rounded", ];
          "b.md" [label="b", ];
          "notes/a.md" [label="a", ];


          edge [fontname="sans-serif", fontsize=8, color="#666666",
                arrowsize=0.500000, ];
          "b.md" -> "notes/a.md";
          "notes/a.md" -> "b.md";

          }
        |}]
    ;;
  end)
;;
