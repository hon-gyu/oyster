(** In-memory trace collector for testing.

    Captures spans and messages into a mutable list, suitable for
    trace-based property testing. Install with {!with_collector},
    then inspect {!event} values after the function under test returns. *)

open Core

let sexp_of_user_data : Trace_core.user_data -> Sexp.t = function
  | `Int i -> Sexp.List [ Atom "Int"; sexp_of_int i ]
  | `String s -> Sexp.List [ Atom "String"; sexp_of_string s ]
  | `Bool b -> Sexp.List [ Atom "Bool"; sexp_of_bool b ]
  | `Float f -> Sexp.List [ Atom "Float"; sexp_of_float f ]
  | `None -> Sexp.Atom "None"
;;

let sexp_of_datum (k, v) = Sexp.List [ sexp_of_string k; sexp_of_user_data v ]
let sexp_of_data data = sexp_of_list sexp_of_datum data

(** A single recorded trace event. *)
type event =
  | Enter of
      { name : string
      ; data : (string * Trace_core.user_data) list
      }
  | Exit of { name : string }
  | Message of
      { msg : string
      ; data : (string * Trace_core.user_data) list
      }

let sexp_of_event = function
  | Enter { name; data } ->
    Sexp.List [ Atom "Enter"; Sexp.List [ Atom name; sexp_of_data data ] ]
  | Exit { name } -> Sexp.List [ Atom "Exit"; Atom name ]
  | Message { msg; data } ->
    Sexp.List [ Atom "Message"; Sexp.List [ Atom msg; sexp_of_data data ] ]
;;

type span_info =
  { name : string
  ; mutable data : (string * Trace_core.user_data) list
  }

type Trace_core.span += Test_span of span_info

(** Collected events, most recent last. *)
type t = { mutable events : event list }

let create () = { events = [] }

let collector (t : t) : Trace_core.Collector.t =
  Trace_core.Collector.C_some
    ( t
    , Trace_core.Collector.Callbacks.make
        ~enter_span:
          (fun
            t
            ~__FUNCTION__:_
            ~__FILE__:_
            ~__LINE__:_
            ~level:_
            ~params:_
            ~data
            ~parent:_
            name ->
          let info = { name; data } in
          t.events <- Enter { name; data } :: t.events;
          Test_span info)
        ~exit_span:(fun t span ->
          match span with
          | Test_span info -> t.events <- Exit { name = info.name } :: t.events
          | _ -> ())
        ~add_data_to_span:(fun t span data ->
          match span with
          | Test_span info ->
            info.data <- info.data @ data;
            (* Update the most recent Enter event for this span *)
            t.events
            <- List.map t.events ~f:(fun ev ->
                 match ev with
                 | Enter e when String.equal e.name info.name ->
                   Enter { e with data = info.data }
                 | other -> other)
          | _ -> ())
        ~message:(fun t ~level:_ ~params:_ ~data ~span:_ msg ->
          t.events <- Message { msg; data } :: t.events)
        ~metric:(fun _ ~level:_ ~params:_ ~data:_ _ _ -> ())
        () )
;;

(** Run [f] with tracing collected into [t]. *)
let with_collector (t : t) (f : unit -> 'a) : 'a =
  Trace_core.with_setup_collector (collector t) f
;;

(** Get events in chronological order. *)
let events (t : t) : event list = List.rev t.events

(** Get only [Enter] events (completed spans with final data). *)
let spans (t : t) : (string * (string * Trace_core.user_data) list) list =
  List.filter_map (events t) ~f:(fun ev ->
    match ev with
    | Enter { name; data } -> Some (name, data)
    | _ -> None)
;;

(** Find span data by name. Returns the data of the first matching span. *)
let find_span (t : t) (name : string) : (string * Trace_core.user_data) list option =
  List.find_map (events t) ~f:(fun ev ->
    match ev with
    | Enter { name = n; data } when String.equal n name -> Some data
    | _ -> None)
;;

(** Get just the span names in order. *)
let span_names (t : t) : string list =
  List.filter_map (events t) ~f:(fun ev ->
    match ev with
    | Enter { name; _ } -> Some name
    | _ -> None)
;;
