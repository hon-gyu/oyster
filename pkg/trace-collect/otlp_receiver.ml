(** Minimal OTLP HTTP/protobuf receiver for capturing traces from external processes.

    Runs a single-threaded TCP server (in a background thread) that accepts
    OTLP HTTP/protobuf [ExportTraceServiceRequest] POSTs on [/v1/traces] and
    collects the decoded spans in memory.

    Usage:
    {[
      let r = Otlp_receiver.create () in
      Otlp_receiver.start r;
      (* run external process with OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:<port> *)
      ...
      Otlp_receiver.stop r;
      Otlp_receiver.spans r  (* collected spans *)
    ]} *)

open Core
module OT = Opentelemetry_proto.Trace
module OTS = Opentelemetry_proto.Trace_service

type t =
  { socket : Core_unix.File_descr.t
  ; port : int
  ; mutable spans : OT.span list
  ; mutable running : bool
  }

let create () =
  let socket = Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  Core_unix.setsockopt socket SO_REUSEADDR true;
  Core_unix.bind socket ~addr:(ADDR_INET (Core_unix.Inet_addr.localhost, 0));
  Core_unix.listen socket ~backlog:5;
  let port =
    match Core_unix.getsockname socket with
    | ADDR_INET (_, p) -> p
    | _ -> failwith "unexpected socket address"
  in
  { socket; port; spans = []; running = true }
;;

let port t = t.port
let endpoint t = sprintf "http://localhost:%d" t.port
let spans t = List.rev t.spans

(** Read exactly [n] bytes from [fd]. *)
let read_exactly (fd : Core_unix.File_descr.t) (n : int) : string =
  let buf = Bytes.create n in
  let read = ref 0 in
  while !read < n do
    let got = Core_unix.read fd ~buf ~pos:!read ~len:(n - !read) in
    if got = 0 then failwith "connection closed";
    read := !read + got
  done;
  Bytes.to_string buf
;;

(** Read HTTP headers (up to [\r\n\r\n]) and return them as a string. *)
let read_headers (fd : Core_unix.File_descr.t) : string =
  let buf = Buffer.create 512 in
  let b = Bytes.create 1 in
  let found = ref false in
  while not !found do
    let got = Core_unix.read fd ~buf:b ~pos:0 ~len:1 in
    if got = 0
    then found := true
    else (
      Buffer.add_char buf (Bytes.get b 0);
      let s = Buffer.contents buf in
      if String.is_suffix s ~suffix:"\r\n\r\n" then found := true)
  done;
  Buffer.contents buf
;;

(** Extract Content-Length from raw HTTP headers. *)
let content_length_of_headers (headers : string) : int =
  String.split_lines headers
  |> List.find_map ~f:(fun line ->
    let lower = String.lowercase line in
    if String.is_prefix lower ~prefix:"content-length:"
    then
      Some
        (String.chop_prefix_exn lower ~prefix:"content-length:"
         |> String.strip
         |> Int.of_string)
    else None)
  |> Option.value ~default:0
;;

let http_200 = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"

(** Accept one HTTP connection, decode OTLP protobuf body, collect spans. *)
let accept_one (t : t) : unit =
  let fd, _addr = Core_unix.accept t.socket in
  try
    let headers = read_headers fd in
    let len = content_length_of_headers headers in
    let body = if len > 0 then read_exactly fd len else "" in
    let _written =
      Core_unix.write_substring fd ~buf:http_200 ~pos:0 ~len:(String.length http_200)
    in
    Core_unix.close fd;
    if len > 0
    then (
      let decoder = Pbrt.Decoder.of_string body in
      let req = OTS.decode_pb_export_trace_service_request decoder in
      let new_spans =
        List.concat_map req.resource_spans ~f:(fun rs ->
          List.concat_map rs.scope_spans ~f:(fun ss -> ss.spans))
      in
      t.spans <- t.spans @ new_spans)
  with
  | exn ->
    (try Core_unix.close fd with
     | _ -> ());
    eprintf "otlp_receiver: %s\n" (Exn.to_string exn)
;;

(** Start the receiver loop in a background thread. *)
let start (t : t) : unit =
  let (_ : Core_thread.t) =
    Core_thread.create
      (fun () ->
         while t.running do
           (* Use select with timeout so we can check t.running periodically *)
           let sel =
             Core_unix.select
               ~read:[ t.socket ]
               ~write:[]
               ~except:[]
               ~timeout:(`After (Core.Time_ns.Span.of_sec 0.1))
               ()
           in
           if not (List.is_empty sel.read)
           then (
             try accept_one t with
             | _ -> ())
         done)
      ()
      ~on_uncaught_exn:`Kill_whole_process
  in
  ()
;;

(** Stop the receiver and close the listening socket. *)
let stop (t : t) : unit =
  t.running <- false;
  (* Give the thread time to notice *)
  Core_unix.nanosleep 0.15 |> (ignore : float -> unit);
  try Core_unix.close t.socket with
  | _ -> ()
;;
