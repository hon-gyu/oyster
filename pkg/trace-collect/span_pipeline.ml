(** Span transformers for sanitization and scrubbing. *)

open Core
module OT = Opentelemetry_proto.Trace

type kv = Opentelemetry_proto.Common.key_value
type value = Opentelemetry_proto.Common.any_value

let span_id_hex (b : bytes) : string =
  let len = Bytes.length b in
  if len = 0
  then ""
  else Bytes.to_string b |> String.concat_map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
;;

let duration_nanos (sp : OT.span) =
  Int64.(sp.end_time_unix_nano - sp.start_time_unix_nano)
;;

(** Replace each span's duration with [rank * 1000] nanoseconds (i.e. rank
    microseconds), where rank is determined by start time order (earliest
    start → lowest rank).  Spans are ranked by when they start, so the
    output is fully deterministic regardless of actual execution speed. *)
let normalize_duration (spans : OT.span list) : OT.span list =
  let indexed =
    List.mapi spans ~f:(fun i sp -> i, sp)
    |> List.sort ~compare:(fun (i, a) (j, b) ->
      let c = Int64.compare a.OT.start_time_unix_nano b.OT.start_time_unix_nano in
      if c <> 0 then c else Int.compare i j)
  in
  let ranks = Hashtbl.create (module String) in
  List.iteri indexed ~f:(fun rank (_, sp) ->
    Hashtbl.set ranks ~key:(span_id_hex sp.span_id) ~data:(rank + 1));
  List.map spans ~f:(fun sp ->
    let sp' = OT.copy_span sp in
    let r = Hashtbl.find_exn ranks (span_id_hex sp.span_id) in
    OT.span_set_end_time_unix_nano
      sp'
      Int64.(sp'.start_time_unix_nano + (of_int r * of_int 1000));
    sp')
;;

module Common = Opentelemetry_proto.Common

(** Recursively walk attributes, calling [f path value] at each leaf.
    [f] receives the key path (outermost first) and the current value.
    - [Some v] → keep/replace with [v]
    - [None]   → drop the attribute *)
let rec map_value
          (f : string list -> value option -> value option)
          ~(path : string list)
          (v : value)
  : value option
  =
  match v with
  | Kvlist_value kvl ->
    let kvs =
      List.filter_map kvl.values ~f:(fun (kv : kv) ->
        let child_path = path @ [ kv.key ] in
        match kv.value with
        | None ->
          f child_path None
          |> Option.map ~f:(fun v' -> Common.make_key_value ~key:kv.key ~value:v' ())
        | Some child_v ->
          (match map_value f ~path:child_path child_v with
           | None -> None
           | Some v' -> Some (Common.make_key_value ~key:kv.key ~value:v' ())))
    in
    if List.is_empty kvs
    then None
    else Some (Kvlist_value (Common.make_key_value_list ~values:kvs ()))
  | _ -> f path (Some v)
;;

(** Recursively walk attributes, calling [f path value] at each leaf.
    @param f receives the key path (outermost first) and the current value.
    - [Some v] → keep/replace with [v]
    - [None]   → drop the attribute *)
let filter_map_attributes
      (spans : OT.span list)
      ~(f : string list -> value option -> value option)
  : OT.span list
  =
  List.map spans ~f:(fun sp ->
    let sp' = OT.copy_span sp in
    let new_attrs =
      List.filter_map sp'.attributes ~f:(fun (kv : kv) ->
        let path = [ kv.key ] in
        match kv.value with
        | None ->
          f path None
          |> Option.map ~f:(fun v -> Common.make_key_value ~key:kv.key ~value:v ())
        | Some v ->
          (match map_value f ~path v with
           | None -> None
           | Some v' -> Some (Common.make_key_value ~key:kv.key ~value:v' ())))
    in
    OT.span_set_attributes sp' new_attrs;
    sp')
;;

(** Helper based on {!filter_map_attributes} that drops attributes whose
    key paths match any entry in [remove].  Each entry is a [string list]
    key path, e.g. [["http"; "request"; "method"]] to target a nested key. *)
let filter_attributes (spans : OT.span list) ~(remove : string list list) : OT.span list =
  let remove_set = Set.Poly.of_list remove in
  filter_map_attributes spans ~f:(fun path v ->
    if Set.mem remove_set path then None else v)
;;

(** Helper based on {!filter_map_attributes} that replaces attributes whose
    key paths match any entry in [scrub] with ["-"]. *)
let scrub_attributes (spans : OT.span list) ~(scrub : string list list) : OT.span list =
  let scrub_set = Set.Poly.of_list scrub in
  filter_map_attributes spans ~f:(fun path v ->
    if Set.mem scrub_set path then Some (String_value "-") else v)
;;
