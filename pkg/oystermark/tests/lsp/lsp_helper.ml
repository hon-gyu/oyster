(** Test harness for the oystermark LSP server.

    Drives {!Lsp_lib.Server} in-process: a "session" {i is} a
    {!Lsp_lib.Server.t}, so tests call handlers directly
    ([Server.hover s ~rel_path ~line ~character]) instead of exchanging
    JSON-RPC with a subprocess.  This module only supplies the two things
    that are awkward to inline — session setup and result formatting.

    Everything below the protocol boundary is still exercised for real: the
    same vault build, the same byte-offset → [Position] conversion, and the
    same [WorkspaceEdit] construction that [main.ml] serializes. *)

open Core
open Linol_lsp.Lsp.Types
module Server = Lsp_lib.Server

(* Session
   ======== *)

(** Create a server and point it at [vault_root], as [initialize] would. *)
let start_server ~(vault_root : string) : Server.t =
  let server = Server.create () in
  Server.initialize server ~root:vault_root;
  server
;;

(** Open [rel_path] with its on-disk content, returning the diagnostics the
    server would publish in response. *)
let open_doc (s : Server.t) ~(rel_path : string) : Diagnostic.t list =
  let vault_root = Option.value_exn (Server.vault_root s) in
  let content = In_channel.read_all (Filename.concat vault_root rel_path) in
  Server.did_open s ~rel_path ~content
;;

(** {!open_doc}, discarding the diagnostics. *)
let did_open (s : Server.t) ~(rel_path : string) : unit =
  ignore (open_doc s ~rel_path : Diagnostic.t list)
;;

(** Replace the buffer contents of [rel_path], returning the republished
    diagnostics. *)
let did_change (s : Server.t) ~(rel_path : string) ~(text : string) : Diagnostic.t list =
  Server.did_change s ~rel_path ~content:text
;;

(** Build a vault in a fresh temporary directory, run [f] against it, and
    clean up.  For tests that need to mutate the vault on disk. *)
let with_tmp_vault ~(files : (string * string) list) (f : string -> unit) : unit =
  let dir = Core_unix.mkdtemp "/tmp/oystermark-lsp-test-" in
  List.iter files ~f:(fun (rel, content) ->
    let full = Filename.concat dir rel in
    Core_unix.mkdir_p (Filename.dirname full);
    Out_channel.write_all full ~data:content);
  Exn.protect
    ~f:(fun () -> f dir)
    ~finally:(fun () ->
      let (_ : Core_unix.Exit_or_signal.t) =
        Core_unix.system (sprintf "rm -rf %s" (Filename.quote dir))
      in
      ())
;;

(* Result formatting
   ================== *)

(** Collapse a single-location definition result back into the pure-layer
    record, so server-level expectations read the same as the unit ones. *)
let definition_result (s : Server.t) (result : Location.t list option)
  : Lsp_lib.Go_to_definition.definition_result option
  =
  match result with
  | None | Some [] -> None
  | Some [ loc ] ->
    let start = loc.range.start in
    Some
      { Lsp_lib.Go_to_definition.path = Server.rel_path_of_uri s loc.uri
      ; line = start.line
      ; character = start.character
      }
  | Some locs -> failwithf "expected a single location, got %d" (List.length locs) ()
;;

(** [(rel_path, start_line, start_character)] per reference. *)
let reference_positions (s : Server.t) (result : Location.t list option)
  : (string * int * int) list
  =
  Option.value result ~default:[]
  |> List.map ~f:(fun (loc : Location.t) ->
    Server.rel_path_of_uri s loc.uri, loc.range.start.line, loc.range.start.character)
;;

(** The markdown body of a hover response. *)
let hover_text (result : Hover.t option) : string option =
  Option.map result ~f:(fun (h : Hover.t) ->
    match h.contents with
    | `MarkupContent m -> m.value
    | `MarkedString m -> m.value
    | `List _ -> failwith "unexpected MarkedString list in hover contents")
;;

(** [(message, start_line, start_character)] per diagnostic. *)
let diagnostic_positions (diags : Diagnostic.t list) : (string * int * int) list =
  List.map diags ~f:(fun (d : Diagnostic.t) ->
    let message =
      match d.message with
      | `String s -> s
      | `MarkupContent m -> m.value
    in
    message, d.range.start.line, d.range.start.character)
;;

(** [(label, insertText)] per completion item. *)
let completion_items (result : CompletionItem.t list option) : (string * string) list =
  Option.value result ~default:[]
  |> List.map ~f:(fun (i : CompletionItem.t) ->
    i.label, Option.value_exn i.insertText ~message:"completion item without insertText")
;;

(** [(line, character, label)] per inlay hint. *)
let inlay_hint_positions (result : InlayHint.t list option) : (int * int * string) list =
  Option.value result ~default:[]
  |> List.map ~f:(fun (h : InlayHint.t) ->
    let label =
      match h.label with
      | `String s -> s
      | `List parts ->
        String.concat (List.map parts ~f:(fun (p : InlayHintLabelPart.t) -> p.value))
    in
    h.position.line, h.position.character, label)
;;

(** One line per document change: the operation kind, or [text-edits] for a
    batch of edits to an existing file. *)
let document_change_kinds (edit : WorkspaceEdit.t) : string list =
  Option.value edit.documentChanges ~default:[]
  |> List.map ~f:(function
    | `CreateFile _ -> "create"
    | `RenameFile _ -> "rename"
    | `DeleteFile _ -> "delete"
    | `TextDocumentEdit _ -> "text-edits")
;;

(** Every [newText] a workspace edit would insert, in order. *)
let inserted_texts (edit : WorkspaceEdit.t) : string list =
  Option.value edit.documentChanges ~default:[]
  |> List.concat_map ~f:(function
    | `TextDocumentEdit (e : TextDocumentEdit.t) ->
      List.map e.edits ~f:(function
        | `TextEdit (t : TextEdit.t) -> t.newText
        | `AnnotatedTextEdit (t : AnnotatedTextEdit.t) -> t.newText)
    | `CreateFile _ | `RenameFile _ | `DeleteFile _ -> [])
;;
