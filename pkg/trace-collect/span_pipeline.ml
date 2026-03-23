(** Span transformers for sanitization and scrubbing. *)

open Core
module OT = Opentelemetry_proto.Trace

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
    microseconds), where rank 1 is the shortest.  Ties within
    [tie_tolerance_nano] share the same rank.  Start times are preserved. *)
let normalize_duration
      ?(tie_tolerance_nano : int64 = Int64.of_int 3)
      (spans : OT.span list)
  : OT.span list
  =
  let sorted =
    List.map spans ~f:(fun sp -> span_id_hex sp.span_id, duration_nanos sp)
    |> List.sort ~compare:(fun (_, a) (_, b) ->
      let diff = Int64.(abs (a - b)) in
      if Int64.(diff <= tie_tolerance_nano) then 0 else Int64.compare a b)
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
  List.map spans ~f:(fun sp ->
    let sp' = OT.copy_span sp in
    let r = Hashtbl.find_exn ranks (span_id_hex sp.span_id) in
    OT.span_set_end_time_unix_nano
      sp'
      Int64.(sp'.start_time_unix_nano + (of_int r * of_int 1000));
    sp')
;;
