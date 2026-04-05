(** Graph extracted from a resolved vault. *)

open Core

(* Location
   ==================== *)

type loc =
  { first_byte : int
  ; last_byte : int
  ; first_line : int
  ; last_line : int
  }
[@@deriving sexp, compare]

let loc_of_textloc (tl : Cmarkit.Textloc.t) : loc option =
  if Cmarkit.Textloc.is_none tl
  then None
  else
    Some
      { first_byte = Cmarkit.Textloc.first_byte tl
      ; last_byte = Cmarkit.Textloc.last_byte tl
      ; first_line = fst (Cmarkit.Textloc.first_line tl)
      ; last_line = fst (Cmarkit.Textloc.last_line tl)
      }
;;

(* Vertex
   ==================== *)

type vertex_kind =
  | Src of loc
  | Tgt_note
  | Tgt_heading of
      { heading : string
      ; slug : string
      }
  | Tgt_block of { block_id : string }
[@@deriving sexp, compare]

type vertex =
  { path : string
  ; kind : vertex_kind
  }
[@@deriving sexp, compare]

(* Edge
   ==================== *)

type edge_kind = Link [@@deriving sexp, compare]

(* Graph
   ==================== *)

module G =
  Graph.Persistent.Digraph.ConcreteBidirectionalLabeled
    (struct
      type t = vertex [@@deriving compare]

      let hash (v : t) = String.hash v.path
      let equal a b = compare a b = 0
    end)
    (struct
      type t = edge_kind [@@deriving compare]

      let default = Link
    end)

type t = G.t

(* Edge extraction
   ==================== *)

(** Convert a resolved target to a target vertex. *)
let vertex_of_resolved (target : Vault.Resolve.target) (src_path : string) : vertex option
  =
  match target with
  | Vault.Resolve.Note { path } -> Some { path; kind = Tgt_note }
  | Vault.Resolve.File { path } -> Some { path; kind = Tgt_note }
  | Vault.Resolve.Heading { path; heading; slug; _ } ->
    Some { path; kind = Tgt_heading { heading; slug } }
  | Vault.Resolve.Block { path; block_id } -> Some { path; kind = Tgt_block { block_id } }
  | Vault.Resolve.Curr_file -> Some { path = src_path; kind = Tgt_note }
  | Vault.Resolve.Curr_heading { heading; slug; _ } ->
    Some { path = src_path; kind = Tgt_heading { heading; slug } }
  | Vault.Resolve.Curr_block { block_id } ->
    Some { path = src_path; kind = Tgt_block { block_id } }
  | Vault.Resolve.Unresolved -> None
;;

(** Collect [(src_vertex, tgt_vertex)] pairs from a single document. *)
let collect_edges_from_doc (src_path : string) (doc : Cmarkit.Doc.t)
  : (vertex * vertex) list
  =
  let extract_edge acc meta ~resolve_target =
    match Cmarkit.Meta.find Vault.Resolve.resolved_key meta with
    | None -> acc
    | Some resolved ->
      (match vertex_of_resolved resolved src_path with
       | None -> acc
       | Some tgt ->
         let src_loc = loc_of_textloc (Cmarkit.Meta.textloc meta) in
         (match src_loc with
          | None -> acc
          | Some loc ->
            let _ = resolve_target in
            let src = { path = src_path; kind = Src loc } in
            (src, tgt) :: acc))
  in
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun _f acc (i : Cmarkit.Inline.t) ->
        match i with
        | Cmarkit.Inline.Link (_, meta) | Cmarkit.Inline.Image (_, meta) ->
          Cmarkit.Folder.ret (extract_edge acc meta ~resolve_target:i)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Parse.Wikilink.Ext_wikilink (_, meta) -> extract_edge acc meta ~resolve_target:i
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
  (* Add all notes as Tgt_note vertices *)
  let g =
    List.fold vault.docs ~init:g ~f:(fun g (rel_path, _doc) ->
      G.add_vertex g { path = rel_path; kind = Tgt_note })
  in
  (* Add edges *)
  List.fold vault.docs ~init:g ~f:(fun g (src_path, doc) ->
    let edges = collect_edges_from_doc src_path doc in
    List.fold edges ~init:g ~f:(fun g (src, tgt) -> G.add_edge_e g (src, Link, tgt)))
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

    let vertex_name (v : vertex) =
      let kind_suffix =
        match v.kind with
        | Src loc -> Printf.sprintf ":src:%d-%d" loc.first_byte loc.last_byte
        | Tgt_note -> ":tgt:note"
        | Tgt_heading { slug; _ } -> ":tgt:h:" ^ slug
        | Tgt_block { block_id } -> ":tgt:b:" ^ block_id
      in
      Printf.sprintf "%S" (v.path ^ kind_suffix)
    ;;

    let vertex_attributes (v : vertex) =
      let base_label =
        match String.chop_suffix ~suffix:".md" v.path with
        | Some s -> Filename.basename s
        | None -> v.path
      in
      match v.kind with
      | Src loc ->
        [ `Label (Printf.sprintf "%s:%d" base_label loc.first_line)
        ; `Shape `Plaintext
        ; `Fontsize 8
        ]
      | Tgt_note -> [ `Label base_label ]
      | Tgt_heading { heading; _ } ->
        [ `Label (Printf.sprintf "%s § %s" base_label heading) ]
      | Tgt_block { block_id } -> [ `Label (Printf.sprintf "%s ^%s" base_label block_id) ]
    ;;

    let get_subgraph _v = None

    let default_edge_attributes _g =
      [ `Arrowsize 0.5; `Color 0x666666; `Fontsize 8; `Fontname "sans-serif" ]
    ;;

    let edge_attributes _e = []
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
    (** Build a minimal resolved vault from markdown strings.
        Uses [~locs:true] so that source locations are available. *)
    let build_vault (files : (string * string) list) : Vault.t =
      let docs =
        List.map files ~f:(fun (path, content) ->
          path, Parse.of_string ~locs:true content)
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
      let edges = G.fold_edges_e (fun e acc -> e :: acc) g [] in
      let edges =
        List.sort edges ~compare:(fun (s1, _, t1) (s2, _, t2) ->
          let c = compare_vertex s1 s2 in
          if c <> 0 then c else compare_vertex t1 t2)
      in
      List.iter edges ~f:(fun (src, _kind, tgt) ->
        let src_s = Sexp.to_string (sexp_of_vertex src) in
        let tgt_s = Sexp.to_string (sexp_of_vertex tgt) in
        printf "%s -> %s\n" src_s tgt_s)
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
        ((path a.md)(kind(Src((first_byte 8)(last_byte 12)(first_line 1)(last_line 1))))) -> ((path b.md)(kind Tgt_note))
        ((path b.md)(kind(Src((first_byte 8)(last_byte 12)(first_line 1)(last_line 1))))) -> ((path a.md)(kind Tgt_note))
        |}]
    ;;

    let%expect_test "heading link" =
      let vault =
        build_vault [ "a.md", "see [[b#Section]]"; "b.md", "# Section\nsome content" ]
      in
      let g = of_vault vault in
      show_edges g;
      [%expect
        {| ((path a.md)(kind(Src((first_byte 4)(last_byte 16)(first_line 1)(last_line 1))))) -> ((path b.md)(kind(Tgt_heading(heading Section)(slug section)))) |}]
    ;;

    let%expect_test "self-links" =
      let vault = build_vault [ "a.md", "# Top\nsee [[a]] and [[a#Top]]" ] in
      let g = of_vault vault in
      show_edges g;
      [%expect
        {|
        ((path a.md)(kind(Src((first_byte 10)(last_byte 14)(first_line 2)(last_line 2))))) -> ((path a.md)(kind Tgt_note))
        ((path a.md)(kind(Src((first_byte 20)(last_byte 28)(first_line 2)(last_line 2))))) -> ((path a.md)(kind(Tgt_heading(heading Top)(slug top))))
        |}]
    ;;
  end)
;;
