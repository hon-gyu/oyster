(** Graph extracted from a resolved vault. *)

open Core

(* Textloc
   ==================== *)

type textloc = Cmarkit.Textloc.t

let sexp_of_textloc = Parse.Textloc_conv.sexp_of_t
let textloc_of_sexp = Parse.Textloc_conv.t_of_sexp
let compare_textloc = Parse.Textloc_conv.compare

let textloc_of_meta (meta : Cmarkit.Meta.t) : textloc option =
  let tl = Cmarkit.Meta.textloc meta in
  if Cmarkit.Textloc.is_none tl then None else Some tl
;;

(* Graph
  ==================== *)

type vertex_kind =
  | Link of textloc
  | Note
  | Heading of
      { heading : string
      ; slug : string
      ; loc : textloc option
      }
  | Block of
      { block_id : string
      ; loc : textloc option
      }
[@@deriving sexp, compare]

type vertex =
  { path : string
  ; kind : vertex_kind
  }
[@@deriving sexp, compare]

type edge_kind = Link [@@deriving sexp, compare]

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

(* Node metadata
   ==================== *)

type node_meta =
  { title : string
  ; tags : string list
  ; folder : string
  ; href : string
  }
[@@deriving sexp, compare]

(** Extract metadata for a single document. *)
let meta_of_doc (rel_path : string) (doc : Cmarkit.Doc.t) : node_meta =
  let title_from_path (rel_path : string) : string =
    let base = Filename.basename rel_path in
    match String.chop_suffix base ~suffix:".md" with
    | Some s -> s
    | None -> base
  in
  let folder = Filename.dirname rel_path in
  let href = Component.Html.note_url_path rel_path in
  let default_title = title_from_path rel_path in
  match Parse.Frontmatter.of_doc doc with
  | None -> { title = default_title; tags = []; folder; href }
  | Some yaml ->
    let title =
      match yaml with
      | `O pairs ->
        List.find_map pairs ~f:(fun (k, v) ->
          if String.equal k "title"
          then (
            match v with
            | `String s -> Some s
            | _ -> None)
          else None)
        |> Option.value ~default:default_title
      | _ -> default_title
    in
    let tags =
      match yaml with
      | `O pairs ->
        List.find_map pairs ~f:(fun (k, v) ->
          if String.equal k "tags"
          then (
            match v with
            | `A items ->
              Some
                (List.filter_map items ~f:(fun item ->
                   match item with
                   | `String s -> Some s
                   | _ -> None))
            | _ -> None)
          else None)
        |> Option.value ~default:[]
      | _ -> []
    in
    { title; tags; folder; href }
;;

(* Graph type
   ==================== *)

type t =
  { graph : G.t
  ; meta : node_meta Map.M(String).t
  }

(* Edge extraction
   ==================== *)

(** Convert a resolved target to a target vertex. *)
let vertex_of_resolved_target (target : Vault.Resolve.target) (src_path : string)
  : vertex option
  =
  match target with
  | Vault.Resolve.Note { path } -> Some { path; kind = Note }
  | Vault.Resolve.File { path } -> Some { path; kind = Note }
  | Vault.Resolve.Heading { path; heading; slug; loc; _ } ->
    Some { path; kind = Heading { heading; slug; loc } }
  | Vault.Resolve.Block { path; block_id; loc } ->
    Some { path; kind = Block { block_id; loc } }
  | Vault.Resolve.Curr_file -> Some { path = src_path; kind = Note }
  | Vault.Resolve.Curr_heading { heading; slug; loc; _ } ->
    Some { path = src_path; kind = Heading { heading; slug; loc } }
  | Vault.Resolve.Curr_block { block_id; loc } ->
    Some { path = src_path; kind = Block { block_id; loc } }
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
      (match vertex_of_resolved_target resolved src_path with
       | None -> acc
       | Some tgt ->
         (match textloc_of_meta meta with
          | None -> acc
          | Some tl ->
            let _ = resolve_target in
            let src = { path = src_path; kind = Link tl } in
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
  (* Add all notes as Tgt_note vertices and collect metadata *)
  let g, meta =
    List.fold
      vault.docs
      ~init:(g, Map.empty (module String))
      ~f:(fun (g, meta) (rel_path, doc) ->
        let g = G.add_vertex g { path = rel_path; kind = Note } in
        let meta = Map.set meta ~key:rel_path ~data:(meta_of_doc rel_path doc) in
        g, meta)
  in
  (* Add edges *)
  let g =
    List.fold vault.docs ~init:g ~f:(fun g (src_path, doc) ->
      let edges = collect_edges_from_doc src_path doc in
      List.fold edges ~init:g ~f:(fun g (src, tgt) -> G.add_edge_e g (src, Link, tgt)))
  in
  { graph = g; meta }
;;

(* DOT output
   ==================== *)

module Dot = Graph.Graphviz.Dot (struct
    type t = G.t

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
        | Link tl ->
          Printf.sprintf
            ":src:%d-%d"
            (Cmarkit.Textloc.first_byte tl)
            (Cmarkit.Textloc.last_byte tl)
        | Note -> ":tgt:note"
        | Heading { slug; _ } -> ":tgt:h:" ^ slug
        | Block { block_id; _ } -> ":tgt:b:" ^ block_id
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
      | Link tl ->
        [ `Label (Printf.sprintf "%s:%d" base_label (fst (Cmarkit.Textloc.first_line tl)))
        ; `Shape `Plaintext
        ; `Fontsize 8
        ]
      | Note -> [ `Label base_label ]
      | Heading { heading; _ } -> [ `Label (Printf.sprintf "%s § %s" base_label heading) ]
      | Block { block_id } -> [ `Label (Printf.sprintf "%s ^%s" base_label block_id) ]
    ;;

    let get_subgraph _v = None

    let default_edge_attributes _g =
      [ `Arrowsize 0.5; `Color 0x666666; `Fontsize 8; `Fontname "sans-serif" ]
    ;;

    let edge_attributes _e = []
  end)

let to_dot (t : t) : string =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Dot.fprint_graph fmt t.graph;
  Format.pp_print_flush fmt ();
  Buffer.contents buf
;;

(* Tests
   ==================== *)

let%test_module "graph" =
  (module struct
    let build_vault = Vault.of_inmem_files ~vault_root:"/tmp_vault"

    let show_edges (t : t) =
      let edges = G.fold_edges_e (fun e acc -> e :: acc) t.graph [] in
      let edges =
        List.sort edges ~compare:(fun (s1, _, t1) (s2, _, t2) ->
          let c = compare_vertex s1 s2 in
          if c <> 0 then c else compare_vertex t1 t2)
      in
      List.iteri edges ~f:(fun i (src, _kind, tgt) ->
        printf "%d\n" i;
        print_s [%sexp ([ "Src", src; "Tgt", tgt ] : (string * vertex) list)];
        printf "\n")
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
        0
        ((Src
          ((path a.md)
           (kind
            (Link
             ((first_byte 8) (last_byte 12) (first_line (1 0)) (last_line (1 0)))))))
         (Tgt ((path b.md) (kind Note))))

        1
        ((Src
          ((path b.md)
           (kind
            (Link
             ((first_byte 8) (last_byte 12) (first_line (1 0)) (last_line (1 0)))))))
         (Tgt ((path a.md) (kind Note))))
        |}]
    ;;

    let%expect_test "heading link" =
      let vault =
        build_vault [ "a.md", "see [[b#Section]]"; "b.md", "# Section\nsome content" ]
      in
      let g = of_vault vault in
      show_edges g;
      [%expect
        {|
        0
        ((Src
          ((path a.md)
           (kind
            (Link
             ((first_byte 4) (last_byte 16) (first_line (1 0)) (last_line (1 0)))))))
         (Tgt
          ((path b.md)
           (kind
            (Heading (heading Section) (slug section)
             (loc
              (((first_byte 0) (last_byte 8) (first_line (1 0)) (last_line (1 0))))))))))
        |}]
    ;;

    let%expect_test "self-links" =
      let vault = build_vault [ "a.md", "# Top\nsee [[a]] and [[a#Top]]" ] in
      let g = of_vault vault in
      show_edges g;
      [%expect
        {|
        0
        ((Src
          ((path a.md)
           (kind
            (Link
             ((first_byte 10) (last_byte 14) (first_line (2 6)) (last_line (2 6)))))))
         (Tgt ((path a.md) (kind Note))))

        1
        ((Src
          ((path a.md)
           (kind
            (Link
             ((first_byte 20) (last_byte 28) (first_line (2 6)) (last_line (2 6)))))))
         (Tgt
          ((path a.md)
           (kind
            (Heading (heading Top) (slug top)
             (loc
              (((first_byte 0) (last_byte 4) (first_line (1 0)) (last_line (1 0))))))))))
        |}]
    ;;
  end)
;;
