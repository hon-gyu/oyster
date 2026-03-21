(** Pretty-print collected OTEL spans for testing.

    Three rendering styles, unified behind a single [format] function:

    - {b Flat}: chronological list, no indentation (like logfire's
      [SimpleConsoleSpanExporter]).
    - {b Indented}: depth-first tree walk, children indented under parents
      (like [IndentedConsoleSpanExporter]).
    - {b Show_parents}: like [Indented], but re-prints ancestor context
      (dimmed with ["~"]) when consecutive spans come from different
      subtrees (like [ShowParentsConsoleSpanExporter]).

    Both streaming and post-hoc usage are supported:
    {[
      (* post-hoc *)
      Trace_pp.format Indented spans

      (* streaming *)
      let pp = Trace_pp.create Indented in
      ...emit spans...
      Trace_pp.process pp span;
      ...later...
      Trace_pp.contents pp
    ]} *)

open Core
module OT = Opentelemetry_proto.Trace
module Column = Ascii_table_kernel.Column

type style =
  | Flat
  | Indented
  | Show_parents
[@@deriving sexp_of]

(* internal row representation
==================== *)

type row =
  { name : string (* includes indentation prefix *)
  ; duration : string
  ; attrs : string
  }

(* span helpers
==================== *)

let span_id_hex (b : bytes) : string =
  let len = Bytes.length b in
  if len = 0
  then ""
  else Bytes.to_string b |> String.concat_map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
;;

let format_duration (sp : OT.span) : string =
  let d = Int64.(sp.end_time_unix_nano - sp.start_time_unix_nano) in
  let us = Int64.to_float d /. 1_000.0 in
  if Float.(us >= 1_000_000.0)
  then sprintf "%.2fs" (us /. 1_000_000.0)
  else if Float.(us >= 1_000.0)
  then sprintf "%.1fms" (us /. 1_000.0)
  else sprintf "%.0fus" us
;;

let is_internal_attr key =
  String.is_prefix key ~prefix:"code." || String.is_prefix key ~prefix:"otel."
;;

let format_attrs (sp : OT.span) : string =
  List.filter_map sp.attributes ~f:(fun (kv : Opentelemetry_proto.Common.key_value) ->
    if is_internal_attr kv.key
    then None
    else (
      let v =
        match kv.value with
        | Some (String_value s) -> s
        | Some (Int_value i) -> Int64.to_string i
        | Some (Bool_value b) -> Bool.to_string b
        | Some (Double_value f) -> Float.to_string f
        | _ -> "?"
      in
      Some (sprintf "%s=%s" kv.key v)))
  |> String.concat ~sep:" "
;;

(* tree
==================== *)

type span_node =
  { span : OT.span
  ; children : span_node list
  }

let build_forest (spans : OT.span list) : span_node list =
  let by_parent = Hashtbl.create (module String) in
  List.iter spans ~f:(fun sp ->
    let pid = span_id_hex sp.parent_span_id in
    Hashtbl.add_multi by_parent ~key:pid ~data:sp);
  let span_ids =
    Set.of_list (module String) (List.map spans ~f:(fun sp -> span_id_hex sp.span_id))
  in
  let rec build (span : OT.span) =
    let children =
      Hashtbl.find_multi by_parent (span_id_hex span.span_id)
      |> List.sort ~compare:(fun a b ->
        Int64.compare a.OT.start_time_unix_nano b.start_time_unix_nano)
    in
    { span; children = List.map children ~f:build }
  in
  let roots =
    List.filter spans ~f:(fun sp ->
      let pid = span_id_hex sp.parent_span_id in
      String.is_empty pid || not (Set.mem span_ids pid))
    |> List.sort ~compare:(fun a b ->
      Int64.compare a.OT.start_time_unix_nano b.start_time_unix_nano)
  in
  List.map roots ~f:build
;;

(* row computation per style
==================== *)

let duration_nanos (sp : OT.span) =
  Int64.(sp.end_time_unix_nano - sp.start_time_unix_nano)
;;

(** Build a map from span_id to duration rank (1 = shortest). Ties share the same rank. *)
let build_duration_ranks (spans : OT.span list) : (string, int) Hashtbl.t =
  let sorted =
    List.map spans ~f:(fun sp -> span_id_hex sp.span_id, duration_nanos sp)
    |> List.sort ~compare:(fun (_, a) (_, b) -> Int64.compare a b)
  in
  let ranks = Hashtbl.create (module String) in
  let rank = ref 0 in
  let prev = ref Int64.min_value in
  List.iter sorted ~f:(fun (sid, dur) ->
    if Int64.( <> ) dur !prev
    then (
      Int.incr rank;
      prev := dur);
    Hashtbl.set ranks ~key:sid ~data:!rank);
  ranks
;;

let make_row ?(prefix = "") ?duration_ranks (sp : OT.span) : row =
  let duration =
    match duration_ranks with
    | Some ranks ->
      (match Hashtbl.find ranks (span_id_hex sp.span_id) with
       | Some r -> Int.to_string r
       | None -> format_duration sp)
    | None -> format_duration sp
  in
  { name = prefix ^ sp.name; duration; attrs = format_attrs sp }
;;

let flat_rows ?duration_ranks spans : row list =
  List.sort spans ~compare:(fun a b ->
    Int64.compare a.OT.start_time_unix_nano b.start_time_unix_nano)
  |> List.map ~f:(make_row ?duration_ranks ?prefix:None)
;;

let indented_rows ?duration_ranks spans : row list =
  let forest = build_forest spans in
  let acc = Queue.create () in
  let rec walk depth node =
    let prefix = String.make (depth * 2) ' ' in
    Queue.enqueue acc (make_row ?duration_ranks ~prefix node.span);
    List.iter node.children ~f:(walk (depth + 1))
  in
  List.iter forest ~f:(walk 0);
  Queue.to_list acc
;;

let show_parents_rows ?duration_ranks spans : row list =
  let forest = build_forest spans in
  (* Walk depth-first but track the "last path" so we can re-print
     ancestor context when we jump between subtrees. *)
  let acc = Queue.create () in
  (* last_path.(i) = span_id at depth i of the most recently printed span *)
  let last_path : string option array = Array.create ~len:64 None in
  let rec walk depth node =
    let sid = span_id_hex node.span.span_id in
    (* Check if parent context is already visible *)
    let parent_visible =
      depth = 0
      || Option.equal
           String.equal
           last_path.(depth - 1)
           (Some (span_id_hex node.span.parent_span_id))
    in
    (* If parent isn't the last thing at that depth, re-print ancestors *)
    if not parent_visible then re_print_ancestors depth node;
    let prefix = String.make (depth * 2) ' ' in
    Queue.enqueue acc (make_row ?duration_ranks ~prefix node.span);
    last_path.(depth) <- Some sid;
    (* Clear deeper levels *)
    for i = depth + 1 to Array.length last_path - 1 do
      last_path.(i) <- None
    done;
    List.iter node.children ~f:(walk (depth + 1))
  and re_print_ancestors depth node =
    (* Collect ancestor chain from the tree *)
    let ancestors = collect_ancestors depth node in
    List.iter ancestors ~f:(fun (d, sp) ->
      let prefix = String.make (d * 2) ' ' ^ "~ " in
      Queue.enqueue acc (make_row ?duration_ranks ~prefix sp);
      last_path.(d) <- Some (span_id_hex sp.span_id))
  and collect_ancestors _depth _node =
    (* Walk up using parent_span_id to find ancestors not in last_path *)
    (* For tree-based rendering, ancestors are implicit in the recursion,
       so we find them by looking at the forest. *)
    let id_to_node = Hashtbl.create (module String) in
    let parent_of = Hashtbl.create (module String) in
    let rec index ~parent_id node =
      let sid = span_id_hex node.span.span_id in
      Hashtbl.set id_to_node ~key:sid ~data:node;
      Hashtbl.set parent_of ~key:sid ~data:parent_id;
      List.iter node.children ~f:(index ~parent_id:sid)
    in
    List.iter (build_forest spans) ~f:(index ~parent_id:"");
    let _target_sid = span_id_hex _node.span.span_id in
    (* Walk up from target, collecting ancestors not in last_path *)
    let chain = ref [] in
    let rec go sid depth =
      if depth < 0 || String.is_empty sid
      then ()
      else (
        let visible = Option.equal String.equal last_path.(depth) (Some sid) in
        if not visible
        then (
          match Hashtbl.find id_to_node sid with
          | Some n -> chain := (depth, n.span) :: !chain
          | None -> ());
        match Hashtbl.find parent_of sid with
        | Some pid -> go pid (depth - 1)
        | None -> ())
    in
    (* Find parent and walk up *)
    let pid = span_id_hex _node.span.parent_span_id in
    go pid (_depth - 1);
    !chain
  in
  List.iter forest ~f:(walk 0);
  Queue.to_list acc
;;

(* rendering with Ascii_table
==================== *)

let render_rows (rows : row list) : string =
  let has_attrs = List.exists rows ~f:(fun r -> not (String.is_empty r.attrs)) in
  let columns =
    let open Column in
    [ create "span" ~align:Left (fun r -> r.name)
    ; create "duration" ~align:Right (fun r -> r.duration)
    ]
    @ if has_attrs then [ create "attrs" ~align:Left (fun r -> r.attrs) ] else []
  in
  Ascii_table_kernel.to_string_noattr
    ~display:Ascii_table_kernel.Display.column_titles
    ~bars:`Unicode
    columns
    rows
;;

(* public API
==================== *)

type t =
  { style : style
  ; normalize_duration : bool
  ; mutable spans : OT.span list
  }

let create ?(normalize_duration = false) style =
  { style; normalize_duration; spans = [] }
;;

let process t span = t.spans <- span :: t.spans

let contents t =
  let spans = List.rev t.spans in
  let duration_ranks =
    if t.normalize_duration then Some (build_duration_ranks spans) else None
  in
  let rows =
    match t.style with
    | Flat -> flat_rows ?duration_ranks spans
    | Indented -> indented_rows ?duration_ranks spans
    | Show_parents -> show_parents_rows ?duration_ranks spans
  in
  render_rows rows
;;

let format ?(normalize_duration = false) (style : style) (spans : OT.span list) : string
    =
  let t = create ~normalize_duration style in
  List.iter spans ~f:(process t);
  contents t
;;
