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

(** Replace each span's duration with [rank * 1us], where rank is the
    1-based position of the span when sorted by
    [(end_time asc, depth desc, start_time asc)].

    End-time order matches completion order: in single-threaded sequential
    code, innermost spans close before their parents and earlier siblings
    close before later ones, so the innermost/earliest span gets [1us] and
    the outermost/latest gets the highest value.  The [depth desc] tiebreaker
    handles the case where nested spans collide at clock resolution (child
    must still rank before parent); [start_time asc] then orders true
    siblings deterministically.

    Commonly used in tests to make spans deterministic. *)
let normalize_duration (spans : OT.span list) : OT.span list =
  let by_id = Hashtbl.create (module String) in
  List.iter spans ~f:(fun (sp : OT.span) ->
    Hashtbl.set by_id ~key:(span_id_hex sp.span_id) ~data:sp);
  let depth_cache = Hashtbl.create (module String) in
  let rec depth_of (sp : OT.span) : int =
    let key = span_id_hex sp.span_id in
    match Hashtbl.find depth_cache key with
    | Some d -> d
    | None ->
      let parent_key = span_id_hex sp.parent_span_id in
      let d =
        match Hashtbl.find by_id parent_key with
        | None -> 0
        | Some parent -> 1 + depth_of parent
      in
      Hashtbl.set depth_cache ~key ~data:d;
      d
  in
  let sorted =
    List.sort spans ~compare:(fun (a : OT.span) (b : OT.span) ->
      match Int64.compare a.end_time_unix_nano b.end_time_unix_nano with
      | 0 ->
        (match Int.compare (depth_of b) (depth_of a) with
         | 0 -> Int64.compare a.start_time_unix_nano b.start_time_unix_nano
         | c -> c)
      | c -> c)
  in
  let ranks = Hashtbl.create (module String) in
  List.iteri sorted ~f:(fun i sp ->
    Hashtbl.set ranks ~key:(span_id_hex sp.span_id) ~data:(i + 1));
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
