(** Integration tests for the oystermark LSP server.

    Spawns the server as a subprocess, sends JSON-RPC messages over
    stdin/stdout, and asserts on the responses. *)

open Core

(** {1 JSON-RPC helpers} *)

let send_message (oc : Out_channel.t) (json : Yojson.Safe.t) : unit =
  let body = Yojson.Safe.to_string json in
  let len = String.length body in
  Out_channel.fprintf oc "Content-Length: %d\r\n\r\n%s" len body;
  Out_channel.flush oc
;;

(** Read one JSON-RPC message, consuming the Content-Length header. *)
let read_message (ic : In_channel.t) : Yojson.Safe.t =
  (* Read headers until blank line *)
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

(** Read messages until we find one with the given request id. Discards
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

(** {1 LSP message constructors} *)

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

(** {1 LSP session} *)

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
  (* Send initialized notification *)
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

let shutdown (s : session) : unit =
  let id = fresh_id s in
  send_message s.oc (make_request ~id ~method_:"shutdown" `Null);
  let _resp = read_response s.ic ~id in
  send_message s.oc (make_notification ~method_:"exit" `Null);
  Core_unix.close_process (s.ic, s.oc) |> ignore
;;

(** {1 Pretty-printing helpers} *)

(** Extract a readable summary from a definition result.
    Prints "rel/path.md:line" or "null". *)
let pp_definition_result (vault_root : string) (result : Yojson.Safe.t) : string =
  match result with
  | `Null -> "null"
  | `List [ loc ] ->
    let uri = Yojson.Safe.Util.(member "uri" loc |> to_string) in
    let range = Yojson.Safe.Util.member "range" loc in
    let start = Yojson.Safe.Util.member "start" range in
    let line = Yojson.Safe.Util.(member "line" start |> to_int) in
    (* Strip file:// prefix and vault root *)
    let path =
      let raw = String.chop_prefix_exn uri ~prefix:"file://" in
      match String.chop_prefix raw ~prefix:(vault_root ^ "/") with
      | Some rel -> rel
      | None -> raw
    in
    sprintf "%s:%d" path line
  | other -> Yojson.Safe.to_string other
;;

(** {1 Tests} *)

(** The test data dir is made available by the [(source_tree data)] dep
    in the dune file. Dune runs inline tests from the library's source
    directory inside the build sandbox, so "data" is a valid relative path. *)
let vault_root =
  let cwd = Core_unix.getcwd () in
  Filename.concat cwd "data"
;;

let%expect_test "go-to-definition: wikilink to note" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 2: "Link to [[note-a]] here." — cursor on "note-a" *)
  let result = definition s ~rel_path:"note-b.md" ~line:2 ~character:13 in
  print_endline (pp_definition_result vault_root result);
  shutdown s;
  [%expect {| note-a.md:0 |}]
;;

let%expect_test "go-to-definition: wikilink to heading" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 4: "See [[note-a#Section One]]." — cursor inside the link *)
  let result = definition s ~rel_path:"note-b.md" ~line:4 ~character:10 in
  print_endline (pp_definition_result vault_root result);
  shutdown s;
  [%expect {| note-a.md:2 |}]
;;

let%expect_test "go-to-definition: wikilink to block id" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 6: "Also [[note-a#^block1]]." — cursor inside the link *)
  let result = definition s ~rel_path:"note-b.md" ~line:6 ~character:10 in
  print_endline (pp_definition_result vault_root result);
  shutdown s;
  [%expect {| note-a.md:4 |}]
;;

let%expect_test "go-to-definition: markdown link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 8: "Markdown [link](note-a)." — cursor on "note-a" in URL *)
  let result = definition s ~rel_path:"note-b.md" ~line:8 ~character:18 in
  print_endline (pp_definition_result vault_root result);
  shutdown s;
  [%expect {| note-a.md:0 |}]
;;

let%expect_test "go-to-definition: unresolved link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 10: "Unresolved [[missing-note]]." — cursor inside *)
  let result = definition s ~rel_path:"note-b.md" ~line:10 ~character:16 in
  print_endline (pp_definition_result vault_root result);
  shutdown s;
  [%expect {| null |}]
;;

let%expect_test "go-to-definition: cursor not on link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"note-b.md";
  (* Line 0: "# Beta" — no link here *)
  let result = definition s ~rel_path:"note-b.md" ~line:0 ~character:2 in
  print_endline (pp_definition_result vault_root result);
  shutdown s;
  [%expect {| null |}]
;;

let%expect_test "go-to-definition: cross-directory link" =
  let s = start_server ~vault_root in
  initialize s;
  did_open s ~rel_path:"subdir/nested.md";
  (* Line 2: "Link to [[note-a]] from subdirectory." *)
  let result = definition s ~rel_path:"subdir/nested.md" ~line:2 ~character:13 in
  print_endline (pp_definition_result vault_root result);
  shutdown s;
  [%expect {| note-a.md:0 |}]
;;
