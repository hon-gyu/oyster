(** Test harness for the oystermark LSP server.

    Provides JSON-RPC helpers for spawning the server as a subprocess,
    sending messages, and reading responses.  Used by {!Test_lsp}. *)

open Core

(* JSON-RPC framing
   ================= *)

let send_message (oc : Out_channel.t) (json : Yojson.Safe.t) : unit =
  let body = Yojson.Safe.to_string json in
  let len = String.length body in
  Out_channel.fprintf oc "Content-Length: %d\r\n\r\n%s" len body;
  Out_channel.flush oc
;;

(** Read one JSON-RPC message, consuming the Content-Length header. *)
let read_message (ic : In_channel.t) : Yojson.Safe.t =
  let content_length = ref 0 in
  let rec read_headers () =
    let line = In_channel.input_line_exn ic in
    let line = String.rstrip ~drop:(Char.equal '\r') line in
    if String.is_empty line
    then ()
    else (
      (match String.lsplit2 line ~on:':' with
       | Some (key, value) when String.equal (String.lowercase key) "content-length" ->
         content_length := Int.of_string (String.strip value)
       | _ -> ());
      read_headers ())
  in
  read_headers ();
  let buf = Bytes.create !content_length in
  In_channel.really_input_exn ic ~buf ~pos:0 ~len:!content_length;
  Yojson.Safe.from_string (Bytes.to_string buf)
;;

(** Read messages until we find one with the given request id.  Discards
    notifications (messages without id) and logs (window/logMessage). *)
let read_response (ic : In_channel.t) ~(id : int) : Yojson.Safe.t =
  let target_id = `Int id in
  let rec loop () =
    let msg = read_message ic in
    match Yojson.Safe.Util.member "id" msg with
    | id_val when Yojson.Safe.equal id_val target_id -> msg
    | _ -> loop ()
  in
  loop ()
;;

(** Read messages until we find a notification with the given method name.
    Discards responses and other notifications. *)
let read_notification (ic : In_channel.t) ~(method_ : string) : Yojson.Safe.t =
  let rec loop () =
    let msg = read_message ic in
    match Yojson.Safe.Util.member "method" msg with
    | `String m when String.equal m method_ -> msg
    | _ -> loop ()
  in
  loop ()
;;

(* Message constructors
   ===================== *)

let make_request ~(id : int) ~(method_ : string) (params : Yojson.Safe.t) : Yojson.Safe.t =
  `Assoc
    [ "jsonrpc", `String "2.0"
    ; "id", `Int id
    ; "method", `String method_
    ; "params", params
    ]
;;

let make_notification ~(method_ : string) (params : Yojson.Safe.t) : Yojson.Safe.t =
  `Assoc [ "jsonrpc", `String "2.0"; "method", `String method_; "params", params ]
;;

(* Session management
   =================== *)

type session =
  { ic : In_channel.t
  ; oc : Out_channel.t
  ; vault_root : string
  ; mutable next_id : int
  }

let fresh_id (s : session) : int =
  let id = s.next_id in
  s.next_id <- id + 1;
  id
;;

let start_server ~(vault_root : string) : session =
  let lsp_exe =
    match Sys.getenv "OYSTERMARK_LSP" with
    | Some path -> path
    | None -> "oystermark-lsp"
  in
  let ic, oc = Core_unix.open_process (sprintf "%s 2>/dev/null" lsp_exe) in
  { ic; oc; vault_root; next_id = 1 }
;;

let initialize (s : session) : unit =
  let id = fresh_id s in
  let uri = sprintf "file://%s" s.vault_root in
  let params =
    `Assoc
      [ "processId", `Int (Pid.to_int (Core_unix.getpid ()))
      ; "rootUri", `String uri
      ; ( "capabilities"
        , `Assoc
            [ ( "textDocument"
              , `Assoc [ "definition", `Assoc [ "dynamicRegistration", `Bool false ] ] )
            ] )
      ]
  in
  send_message s.oc (make_request ~id ~method_:"initialize" params);
  let _resp = read_response s.ic ~id in
  send_message s.oc (make_notification ~method_:"initialized" (`Assoc []))
;;

let did_open (s : session) ~(rel_path : string) : unit =
  let full_path = Filename.concat s.vault_root rel_path in
  let content = In_channel.read_all full_path in
  let uri = sprintf "file://%s" full_path in
  let params =
    `Assoc
      [ ( "textDocument"
        , `Assoc
            [ "uri", `String uri
            ; "languageId", `String "markdown"
            ; "version", `Int 1
            ; "text", `String content
            ] )
      ]
  in
  send_message s.oc (make_notification ~method_:"textDocument/didOpen" params)
;;

let did_change (s : session) ~(rel_path : string) ~(version : int) ~(text : string) : unit =
  let full_path = Filename.concat s.vault_root rel_path in
  let uri = sprintf "file://%s" full_path in
  let params =
    `Assoc
      [ ( "textDocument"
        , `Assoc [ "uri", `String uri; "version", `Int version ] )
      ; "contentChanges", `List [ `Assoc [ "text", `String text ] ]
      ]
  in
  send_message s.oc (make_notification ~method_:"textDocument/didChange" params)
;;

let shutdown (s : session) : unit =
  let id = fresh_id s in
  send_message s.oc (make_request ~id ~method_:"shutdown" `Null);
  let _resp = read_response s.ic ~id in
  send_message s.oc (make_notification ~method_:"exit" `Null);
  Core_unix.close_process (s.ic, s.oc) |> ignore
;;

(* LSP request helpers
   ==================== *)

(** Send a textDocument/definition request and return just the result. *)
let definition (s : session) ~(rel_path : string) ~(line : int) ~(character : int)
  : Yojson.Safe.t
  =
  let id = fresh_id s in
  let full_path = Filename.concat s.vault_root rel_path in
  let uri = sprintf "file://%s" full_path in
  let params =
    `Assoc
      [ "textDocument", `Assoc [ "uri", `String uri ]
      ; "position", `Assoc [ "line", `Int line; "character", `Int character ]
      ]
  in
  send_message s.oc (make_request ~id ~method_:"textDocument/definition" params);
  let resp = read_response s.ic ~id in
  Yojson.Safe.Util.member "result" resp
;;

(** Send a textDocument/hover request and return just the result. *)
let hover (s : session) ~(rel_path : string) ~(line : int) ~(character : int)
  : Yojson.Safe.t
  =
  let id = fresh_id s in
  let full_path = Filename.concat s.vault_root rel_path in
  let uri = sprintf "file://%s" full_path in
  let params =
    `Assoc
      [ "textDocument", `Assoc [ "uri", `String uri ]
      ; "position", `Assoc [ "line", `Int line; "character", `Int character ]
      ]
  in
  send_message s.oc (make_request ~id ~method_:"textDocument/hover" params);
  let resp = read_response s.ic ~id in
  Yojson.Safe.Util.member "result" resp
;;

(** Send a textDocument/references request and return just the result. *)
let references (s : session) ~(rel_path : string) ~(line : int) ~(character : int)
  : Yojson.Safe.t
  =
  let id = fresh_id s in
  let full_path = Filename.concat s.vault_root rel_path in
  let uri = sprintf "file://%s" full_path in
  let params =
    `Assoc
      [ "textDocument", `Assoc [ "uri", `String uri ]
      ; "position", `Assoc [ "line", `Int line; "character", `Int character ]
      ; "context", `Assoc [ "includeDeclaration", `Bool true ]
      ]
  in
  send_message s.oc (make_request ~id ~method_:"textDocument/references" params);
  let resp = read_response s.ic ~id in
  Yojson.Safe.Util.member "result" resp
;;

(** Send a textDocument/inlayHint request and return just the result. *)
let inlay_hint
      (s : session)
      ~(rel_path : string)
      ~(start_line : int)
      ~(end_line : int)
  : Yojson.Safe.t
  =
  let id = fresh_id s in
  let full_path = Filename.concat s.vault_root rel_path in
  let uri = sprintf "file://%s" full_path in
  let params =
    `Assoc
      [ "textDocument", `Assoc [ "uri", `String uri ]
      ; ( "range"
        , `Assoc
            [ ( "start"
              , `Assoc [ "line", `Int start_line; "character", `Int 0 ] )
            ; "end", `Assoc [ "line", `Int end_line; "character", `Int 0 ]
            ] )
      ]
  in
  send_message s.oc (make_request ~id ~method_:"textDocument/inlayHint" params);
  let resp = read_response s.ic ~id in
  Yojson.Safe.Util.member "result" resp
;;

(* Result parsers
   ================ *)

(** Parse a JSON-RPC definition result into a typed value.
    [vault_root] is stripped from the URI prefix to produce a relative path. *)
let parse_definition_result (vault_root : string) (result : Yojson.Safe.t)
  : Lsp_lib.Go_to_definition.definition_result option
  =
  match result with
  | `Null -> None
  | `List [ loc ] ->
    let uri = Yojson.Safe.Util.(member "uri" loc |> to_string) in
    let range = Yojson.Safe.Util.member "range" loc in
    let start = Yojson.Safe.Util.member "start" range in
    let line = Yojson.Safe.Util.(member "line" start |> to_int) in
    let path =
      let raw = String.chop_prefix_exn uri ~prefix:"file://" in
      match String.chop_prefix raw ~prefix:(vault_root ^ "/") with
      | Some rel -> rel
      | None -> raw
    in
    Some { Lsp_lib.Go_to_definition.path; line }
  | other -> failwithf "unexpected definition result: %s" (Yojson.Safe.to_string other) ()
;;

(** Parse a JSON-RPC hover result into the markdown content string. *)
let parse_hover_result (result : Yojson.Safe.t) : string option =
  match result with
  | `Null -> None
  | json ->
    let contents = Yojson.Safe.Util.member "contents" json in
    Some Yojson.Safe.Util.(member "value" contents |> to_string)
;;

(** Parse a JSON-RPC references result into a list of [(rel_path, start_line, start_char)] triples,
    sorted by path then position. *)
let parse_references_result (vault_root : string) (result : Yojson.Safe.t)
  : (string * int * int) list
  =
  match result with
  | `Null -> []
  | `List locs ->
    let parsed =
      List.map locs ~f:(fun loc ->
        let uri = Yojson.Safe.Util.(member "uri" loc |> to_string) in
        let range = Yojson.Safe.Util.member "range" loc in
        let start = Yojson.Safe.Util.member "start" range in
        let line = Yojson.Safe.Util.(member "line" start |> to_int) in
        let character = Yojson.Safe.Util.(member "character" start |> to_int) in
        let path =
          let raw = String.chop_prefix_exn uri ~prefix:"file://" in
          match String.chop_prefix raw ~prefix:(vault_root ^ "/") with
          | Some rel -> rel
          | None -> raw
        in
        path, line, character)
    in
    List.sort parsed ~compare:(fun (p1, l1, c1) (p2, l2, c2) ->
      let c = String.compare p1 p2 in
      if c <> 0
      then c
      else (
        let c = Int.compare l1 l2 in
        if c <> 0 then c else Int.compare c1 c2))
  | other -> failwithf "unexpected references result: %s" (Yojson.Safe.to_string other) ()
;;

(** Parse a JSON-RPC inlayHint result into a list of [(line, character, label)] triples,
    sorted by line then character. *)
let parse_inlay_hint_result (result : Yojson.Safe.t) : (int * int * string) list =
  match result with
  | `Null -> []
  | `List hints ->
    let parsed =
      List.map hints ~f:(fun hint ->
        let pos = Yojson.Safe.Util.member "position" hint in
        let line = Yojson.Safe.Util.(member "line" pos |> to_int) in
        let character = Yojson.Safe.Util.(member "character" pos |> to_int) in
        let label = Yojson.Safe.Util.(member "label" hint |> to_string) in
        line, character, label)
    in
    List.sort parsed ~compare:(fun (l1, c1, _) (l2, c2, _) ->
      let c = Int.compare l1 l2 in
      if c <> 0 then c else Int.compare c1 c2)
  | other -> failwithf "unexpected inlay hint result: %s" (Yojson.Safe.to_string other) ()
;;

(** Parse a publishDiagnostics notification into a list of [(message, line, character)] triples,
    sorted by line then character for stable output. *)
let parse_diagnostics_notification (notif : Yojson.Safe.t) : (string * int * int) list =
  let params = Yojson.Safe.Util.member "params" notif in
  let diags = Yojson.Safe.Util.(member "diagnostics" params |> to_list) in
  let parsed =
    List.map diags ~f:(fun d ->
      let message = Yojson.Safe.Util.(member "message" d |> to_string) in
      let range = Yojson.Safe.Util.member "range" d in
      let start = Yojson.Safe.Util.member "start" range in
      let line = Yojson.Safe.Util.(member "line" start |> to_int) in
      let character = Yojson.Safe.Util.(member "character" start |> to_int) in
      message, line, character)
  in
  List.sort parsed ~compare:(fun (_, l1, c1) (_, l2, c2) ->
    let cmp = Int.compare l1 l2 in
    if cmp <> 0 then cmp else Int.compare c1 c2)
;;
