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

type style =
  | Flat
  | Indented
  | Show_parents

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

let duration_nanos (sp : OT.span) =
  Int64.(sp.end_time_unix_nano - sp.start_time_unix_nano)
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

(* duration ranks
==================== *)

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

(* line formatting
==================== *)

let format_line
      ?(duration_ranks : (string, int) Base.Hashtbl.t option)
      ?(prefix = "")
      (sp : OT.span)
  : string
  =
  let dur =
    match duration_ranks with
    | Some ranks ->
      (match Hashtbl.find ranks (span_id_hex sp.span_id) with
       | Some r -> Int.to_string r ^ "us"
       | None -> format_duration sp)
    | None -> format_duration sp
  in
  let attrs = format_attrs sp in
  let parts =
    [ prefix ^ sp.name; dur ] @ if String.is_empty attrs then [] else [ attrs ]
  in
  String.concat parts ~sep:" "
;;

(* row computation per style
==================== *)

let flat_lines
      ?(duration_ranks : (string, int) Base.Hashtbl.t option)
      (spans : OT.span list)
      (buf : Buffer.t)
  : unit
  =
  let sorted =
    List.sort spans ~compare:(fun a b ->
      Int64.compare a.OT.start_time_unix_nano b.start_time_unix_nano)
  in
  List.iter sorted ~f:(fun sp ->
    Buffer.add_string buf (format_line ?duration_ranks sp);
    Buffer.add_char buf '\n')
;;

let indented_lines
      ?(duration_ranks : (string, int) Base.Hashtbl.t option)
      (spans : OT.span list)
      (buf : Buffer.t)
  : unit
  =
  let forest = build_forest spans in
  let rec walk depth node =
    let prefix = String.make (depth * 2) ' ' in
    Buffer.add_string buf (format_line ?duration_ranks ~prefix node.span);
    Buffer.add_char buf '\n';
    List.iter node.children ~f:(walk (depth + 1))
  in
  List.iter forest ~f:(walk 0)
;;

let show_parents_lines
      ?(duration_ranks : (string, int) Base.Hashtbl.t option)
      (spans : OT.span list)
      (buf : Buffer.t)
  =
  let forest = build_forest spans in
  (* last_path.(i) = span_id at depth i of the most recently printed span *)
  let last_path : string option array = Array.create ~len:64 None in
  let rec walk depth node =
    let sid = span_id_hex node.span.span_id in
    let parent_visible =
      depth = 0
      || Option.equal
           String.equal
           last_path.(depth - 1)
           (Some (span_id_hex node.span.parent_span_id))
    in
    if not parent_visible then re_print_ancestors depth node;
    let prefix = String.make (depth * 2) ' ' in
    Buffer.add_string buf (format_line ?duration_ranks ~prefix node.span);
    Buffer.add_char buf '\n';
    last_path.(depth) <- Some sid;
    for i = depth + 1 to Array.length last_path - 1 do
      last_path.(i) <- None
    done;
    List.iter node.children ~f:(walk (depth + 1))
  and re_print_ancestors depth node =
    let ancestors = collect_ancestors depth node in
    List.iter ancestors ~f:(fun (d, sp) ->
      let prefix = String.make (d * 2) ' ' ^ "~ " in
      Buffer.add_string buf (format_line ?duration_ranks ~prefix sp);
      Buffer.add_char buf '\n';
      last_path.(d) <- Some (span_id_hex sp.span_id))
  and collect_ancestors _depth _node =
    let id_to_node = Hashtbl.create (module String) in
    let parent_of = Hashtbl.create (module String) in
    let rec index ~parent_id node =
      let sid = span_id_hex node.span.span_id in
      Hashtbl.set id_to_node ~key:sid ~data:node;
      Hashtbl.set parent_of ~key:sid ~data:parent_id;
      List.iter node.children ~f:(index ~parent_id:sid)
    in
    List.iter (build_forest spans) ~f:(index ~parent_id:"");
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
    let pid = span_id_hex _node.span.parent_span_id in
    go pid (_depth - 1);
    !chain
  in
  List.iter forest ~f:(walk 0)
;;

(* public API
==================== *)

type t =
  { style : style
  ; normalize_duration : bool
  ; mutable spans : OT.span list
  }

let create ?(normalize_duration = false) (style : style) : t =
  { style; normalize_duration; spans = [] }
;;

let process (t : t) (span : OT.span) : unit = t.spans <- span :: t.spans

let contents (t : t) : string =
  let spans = List.rev t.spans in
  let (duration_ranks : (string, int) Base.Hashtbl.t option) =
    if t.normalize_duration then Some (build_duration_ranks spans) else None
  in
  let buf = Buffer.create 256 in
  (match t.style with
   | Flat -> flat_lines ?duration_ranks spans buf
   | Indented -> indented_lines ?duration_ranks spans buf
   | Show_parents -> show_parents_lines ?duration_ranks spans buf);
  Buffer.contents buf
;;

let format ?(normalize_duration = false) (style : style) (spans : OT.span list) : string =
  let t = create ~normalize_duration style in
  List.iter spans ~f:(process t);
  contents t
;;
