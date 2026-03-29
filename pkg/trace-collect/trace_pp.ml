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

(** Tree-drawing characters for indented styles. *)
type tree_chars =
  | Null (** plain indentation only *)
  | Ascii (** [|-- ] and [`-- ] *)
  | Utf8 (** [├── ] and [└── ] *)

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

(* line formatter
==================== *)

let format_line ~prefix (sp : OT.span) : string =
  let dur = format_duration sp in
  let attrs = format_attrs sp in
  let parts =
    [ prefix ^ sp.name; dur ] @ if String.is_empty attrs then [] else [ attrs ]
  in
  String.concat parts ~sep:" "
;;

(* tree prefix helpers
==================== *)

let tree_prefix (tc : tree_chars) ~depth ~is_last =
  match tc with
  | Null -> String.make (depth * 2) ' '
  | Ascii ->
    let indent = if depth = 0 then "" else String.make ((depth - 1) * 4) ' ' in
    if depth = 0 then "" else indent ^ if is_last then "`-- " else "|-- "
  | Utf8 ->
    let indent = if depth = 0 then "" else String.make ((depth - 1) * 4) ' ' in
    if depth = 0 then "" else indent ^ if is_last then "└── " else "├── "
;;

(** Build the continuation prefix for children: spaces under [|] columns, blanks under [`]. *)
let tree_child_indent (tc : tree_chars) ~depth ~is_last =
  match tc with
  | Null -> ignore (depth, is_last)
  | Ascii | Utf8 -> ignore (depth, is_last)
;;

(* row computation per style
==================== *)

let flat_lines (spans : OT.span list) (buf : Buffer.t) : unit =
  let sorted =
    List.sort spans ~compare:(fun a b ->
      Int64.compare a.OT.start_time_unix_nano b.start_time_unix_nano)
  in
  List.iter sorted ~f:(fun sp ->
    Buffer.add_string buf (format_line ~prefix:"" sp);
    Buffer.add_char buf '\n')
;;

let indented_lines ~(tc : tree_chars) (spans : OT.span list) (buf : Buffer.t) : unit =
  let forest = build_forest spans in
  let rec walk ~depth ~parent_prefix node ~is_last =
    let prefix = parent_prefix ^ tree_prefix tc ~depth ~is_last in
    Buffer.add_string buf (format_line ~prefix node.span);
    Buffer.add_char buf '\n';
    let n_children = List.length node.children in
    List.iteri node.children ~f:(fun i child ->
      let child_is_last = i = n_children - 1 in
      let next_parent_prefix =
        match tc with
        | Null -> ""
        | Ascii ->
          parent_prefix ^ if depth = 0 then "" else if is_last then "    " else "|   "
        | Utf8 ->
          parent_prefix ^ if depth = 0 then "" else if is_last then "    " else "│   "
      in
      walk
        ~depth:(depth + 1)
        ~parent_prefix:next_parent_prefix
        child
        ~is_last:child_is_last)
  in
  let n_roots = List.length forest in
  List.iteri forest ~f:(fun i root ->
    walk ~depth:0 ~parent_prefix:"" root ~is_last:(i = n_roots - 1))
;;

let show_parents_lines ~(tc : tree_chars) (spans : OT.span list) (buf : Buffer.t) =
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
    let prefix = tree_prefix tc ~depth ~is_last:true in
    Buffer.add_string buf (format_line ~prefix node.span);
    Buffer.add_char buf '\n';
    last_path.(depth) <- Some sid;
    for i = depth + 1 to Array.length last_path - 1 do
      last_path.(i) <- None
    done;
    List.iter node.children ~f:(walk (depth + 1))
  and re_print_ancestors depth node =
    let ancestors = collect_ancestors depth node in
    List.iter ancestors ~f:(fun (d, sp) ->
      let prefix = tree_prefix tc ~depth:d ~is_last:true ^ "~ " in
      Buffer.add_string buf (format_line ~prefix sp);
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
  ; tree_chars : tree_chars
  ; mutable spans : OT.span list
  }

let create ?(tree_chars = Null) (style : style) : t = { style; tree_chars; spans = [] }
let process (t : t) (span : OT.span) : unit = t.spans <- span :: t.spans

let contents (t : t) : string =
  let spans = List.rev t.spans in
  let buf = Buffer.create 256 in
  (match t.style with
   | Flat -> flat_lines spans buf
   | Indented -> indented_lines ~tc:t.tree_chars spans buf
   | Show_parents -> show_parents_lines ~tc:t.tree_chars spans buf);
  Buffer.contents buf
;;

let format ?(tree_chars = Utf8) ?(style : style = Indented) (spans : OT.span list) : string =
  let t = create ~tree_chars style in
  List.iter spans ~f:(process t);
  contents t
;;
