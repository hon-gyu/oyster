(** In-memory OTEL trace collector for testing.

    Captures OpenTelemetry spans via a custom [Opentelemetry.Exporter.t],
    suitable for trace-based property testing. The captured data is
    [Opentelemetry_proto.Trace.span list] — the same type you get from
    parsing OTLP JSON, so the same pretty-printer works for both OCaml
    traces and external traces from any language.

    Usage:
    {[
      let t = Trace_collect.create () in
      Trace_collect.with_collect t (fun () ->
        (* call instrumented code *)
        ...);
      Trace_collect.spans t  (* returns Opentelemetry_proto.Trace.span list *)
    ]} *)

open Core
module OT = Opentelemetry_proto.Trace
module Trace_pp = Trace_pp
module Otlp_receiver = Otlp_receiver
module Span_pipeline = Span_pipeline

type t = { mutable collected_spans : OT.span list }

let create () = { collected_spans = [] }

(** Build an in-memory OTEL exporter that captures spans into [t]. *)
let exporter (t : t) : Opentelemetry.Exporter.t =
  let sw, _trigger = Opentelemetry.Aswitch.create () in
  { export =
      (fun signal ->
        match signal with
        | Opentelemetry.Any_signal_l.Spans spans ->
          t.collected_spans <- spans @ t.collected_spans
        | _ -> ())
  ; active = (fun () -> sw)
  ; shutdown = ignore
  ; self_metrics = (fun () -> [])
  }
;;

(** Run [f] with both the OTEL exporter and the ocaml-trace bridge installed.
    Spans emitted via [Trace_core.with_span] are captured into [t].

    Uses unbatched providers so spans arrive at the exporter immediately
    without needing a flush or tick. *)
let with_collect (t : t) (f : unit -> 'a) : 'a =
  Ambient_context.set_current_storage Ambient_context_tls.storage;
  let exp = exporter t in
  Opentelemetry.Sdk.set exp;
  Opentelemetry_trace.setup ();
  let result = f () in
  Opentelemetry.Sdk.remove ~on_done:ignore ();
  Trace_core.shutdown ();
  result
;;

(** Get collected spans in chronological order (earliest first). *)
let spans (t : t) : OT.span list = List.rev t.collected_spans

(** Get just the span names in order. *)
let span_names (t : t) : string list =
  List.map (spans t) ~f:(fun (sp : OT.span) -> sp.name)
;;

(** Find the first span with the given name. *)
let find_span (t : t) (name : string) : OT.span option =
  List.find (spans t) ~f:(fun (sp : OT.span) -> String.equal sp.name name)
;;

(** Extract attribute values from a span as an assoc list of [string * string]. *)
let span_attrs (sp : OT.span) : (string * string) list =
  List.map sp.attributes ~f:(fun (kv : Opentelemetry_proto.Common.key_value) ->
    let v =
      match kv.value with
      | Some (String_value s) -> s
      | Some (Int_value i) -> Int64.to_string i
      | Some (Bool_value b) -> Bool.to_string b
      | Some (Double_value f) -> Float.to_string f
      | _ -> "<unknown>"
    in
    kv.key, v)
;;

(** Find attribute value by key in a span. *)
let span_attr (sp : OT.span) (key : string) : string option =
  List.Assoc.find (span_attrs sp) ~equal:String.equal key
;;
