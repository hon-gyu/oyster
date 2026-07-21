(** Oystermark LSP server entrypoint.

    A [linol-eio] adapter over {!Lsp_lib.Server}: this file advertises
    capabilities, unwraps request params into paths and positions, and hands
    everything else off.  All response-shaping logic lives in the server core
    so that it is reachable from in-process tests. *)

open Core
open Linol_eio
module Server = Lsp_lib.Server

class oystermark_server ~sw =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super
    val server : Server.t = Server.create ()
    method spawn_query_handler f = Linol_eio.spawn ~sw f
    method! config_definition = Some (`Bool true)
    method! config_hover = Some (`Bool true)
    method! config_inlay_hints = Some (`Bool true)
    method! config_symbol = Some (`Bool true)

    method! config_code_action_provider =
      `CodeActionOptions
        (CodeActionOptions.create ~codeActionKinds:[ CodeActionKind.QuickFix ] ())

    method! config_completion : CompletionOptions.t option =
      (* [[[] opens a wikilink; [#] starts a fragment. See {!page-"feature-completion"}. *)
      Some (CompletionOptions.create ~triggerCharacters:[ "["; "#" ] ())

    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      (* Advertise UTF-16 position encoding (LSP mandatory baseline); all
         internal conversions default to it. See {!page-"feature-utf16-positions"}. *)
      { c with
        referencesProvider = Some (`Bool true)
      ; renameProvider =
          Some (`RenameOptions (RenameOptions.create ~prepareProvider:true ()))
      ; positionEncoding = Some PositionEncodingKind.UTF16
      }

    method! config_sync_opts : TextDocumentSyncOptions.t =
      TextDocumentSyncOptions.create
        ~change:TextDocumentSyncKind.Full
        ~openClose:true
        ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
        ()

    method! filter_text_document (uri : DocumentUri.t) : bool =
      Filename.check_suffix (DocumentUri.to_path uri) ".md"

    method private rel_path (uri : DocumentUri.t) : string =
      Server.rel_path_of_uri server uri

    method! on_req_initialize
      ~notify_back
      (params : InitializeParams.t)
      : InitializeResult.t =
      let root =
        match params.rootUri with
        | Some uri -> Some (DocumentUri.to_path uri)
        | None -> Option.join params.rootPath
      in
      Option.iter root ~f:(fun root -> Server.initialize server ~root);
      super#on_req_initialize ~notify_back params

    (* Document synchronization
       ========================= *)

    method on_notif_doc_did_open ~notify_back doc ~(content : string) : unit =
      let rel_path = self#rel_path doc.TextDocumentItem.uri in
      notify_back#send_diagnostic (Server.did_open server ~rel_path ~content)

    method on_notif_doc_did_close ~notify_back:_ doc : unit =
      Server.did_close server ~rel_path:(self#rel_path doc.TextDocumentIdentifier.uri)

    method on_notif_doc_did_change
      ~notify_back
      doc
      _changes
      ~old_content:_
      ~(new_content : string)
      : unit =
      let rel_path = self#rel_path doc.VersionedTextDocumentIdentifier.uri in
      notify_back#send_diagnostic
        (Server.did_change server ~rel_path ~content:new_content)

    (** [send_diagnostic] targets the notification's own document, so
        republishing for the {i other} open buffers means addressing each
        [publishDiagnostics] by hand. *)
    method! on_notif_doc_did_save
      ~notify_back
      (_params : DidSaveTextDocumentParams.t)
      : unit =
      Server.did_save server
      |> List.iter ~f:(fun (rel_path, diagnostics) ->
        let uri = Server.uri_of_rel_path server rel_path in
        notify_back#send_notification
          (Linol.Lsp.Server_notification.PublishDiagnostics
             (PublishDiagnosticsParams.create ~uri ~diagnostics ())))

    (* Features
       ========= *)

    method! on_req_hover
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(pos : Position.t)
      ~workDoneToken:_
      (_ : doc_state)
      : Hover.t option =
      Server.hover
        server
        ~rel_path:(self#rel_path uri)
        ~line:pos.line
        ~character:pos.character

    method! on_req_definition
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(pos : Position.t)
      ~workDoneToken:_
      ~partialResultToken:_
      (_ : doc_state)
      : [ `Location of Location.t list | `LocationLink of LocationLink.t list ] option =
      Server.definition
        server
        ~rel_path:(self#rel_path uri)
        ~line:pos.line
        ~character:pos.character
      |> Option.map ~f:(fun locs -> `Location locs)

    method! on_req_symbol
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~workDoneToken:_
      ~partialResultToken:_
      ()
      : [ `DocumentSymbol of DocumentSymbol.t list
        | `SymbolInformation of SymbolInformation.t list
        ]
          option =
      Server.document_symbol server ~rel_path:(self#rel_path uri)
      |> Option.map ~f:(fun symbols -> `DocumentSymbol symbols)

    method! on_req_code_action
      ~notify_back:_
      ~id:_
      (params : CodeActionParams.t)
      : CodeActionResult.t =
      let actions =
        Server.code_action
          server
          ?only:params.context.only
          ~rel_path:(self#rel_path params.textDocument.uri)
          ~start_line:params.range.start.line
          ~start_character:params.range.start.character
          ~end_line:params.range.end_.line
          ~end_character:params.range.end_.character
          ()
      in
      Some (List.map actions ~f:(fun action -> `CodeAction action))

    method! on_req_completion
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(pos : Position.t)
      ~ctx:_
      ~workDoneToken:_
      ~partialResultToken:_
      (_ : doc_state)
      : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option =
      Server.completion
        server
        ~rel_path:(self#rel_path uri)
        ~line:pos.line
        ~character:pos.character
      |> Option.map ~f:(fun items -> `List items)

    method! on_req_inlay_hint
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(range : Range.t)
      ()
      : InlayHint.t list option =
      Server.inlay_hint
        server
        ~rel_path:(self#rel_path uri)
        ~start_line:range.start.line
        ~end_line:range.end_.line

    (** [references], [prepareRename] and [rename] have no dedicated hook in
        {!Linol_eio.Jsonrpc2.server}, so they arrive here. *)
    method! on_request_unhandled
      : type r. notify_back:_ -> id:_ -> r Linol.Lsp.Client_request.t -> r =
      fun ~notify_back:_ ~id:_ (req : r Linol.Lsp.Client_request.t) ->
        match req with
        | Linol.Lsp.Client_request.TextDocumentReferences params ->
          Server.references
            server
            ~rel_path:(self#rel_path params.textDocument.uri)
            ~line:params.position.line
            ~character:params.position.character
        | Linol.Lsp.Client_request.TextDocumentPrepareRename params ->
          Server.prepare_rename
            server
            ~rel_path:(self#rel_path params.textDocument.uri)
            ~line:params.position.line
            ~character:params.position.character
        | Linol.Lsp.Client_request.TextDocumentRename params ->
          Server.rename
            server
            ~rel_path:(self#rel_path params.textDocument.uri)
            ~line:params.position.line
            ~character:params.position.character
            ~new_name:params.newName
        | _ -> failwith "unhandled request"
  end

let () =
  Eio_main.run
  @@ fun env ->
  let enable_otel = Option.is_some (Sys.getenv "OTEL_EXPORTER_OTLP_ENDPOINT") in
  Opentelemetry_client_cohttp_eio.with_setup ~enable:enable_otel env
  @@ fun () ->
  if enable_otel then Opentelemetry_trace.setup ();
  Trace_core.set_process_name "oystermark-lsp";
  Eio.Switch.run
  @@ fun sw ->
  let s = new oystermark_server ~sw in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  Linol_eio.Jsonrpc2.run server
;;
