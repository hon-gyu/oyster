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

    (** Whether the client advertised [window/showDocument] at [initialize].
        Set once, read by the daily-note command. *)
    val mutable can_show_document : bool = false

    method spawn_query_handler f = Linol_eio.spawn ~sw f
    method! config_definition = Some (`Bool true)
    method! config_hover = Some (`Bool true)
    method! config_inlay_hints = Some (`Bool true)
    method! config_symbol = Some (`Bool true)

    method! config_code_action_provider =
      (* [Refactor] carries the daily-note actions, which no diagnostic
         underlies. See {!page-"feature-daily-notes"}. *)
      `CodeActionOptions
        (CodeActionOptions.create
           ~codeActionKinds:[ CodeActionKind.QuickFix; CodeActionKind.Refactor ]
           ())

    method! config_code_lens_options : CodeLensOptions.t option =
      (* Lenses come fully formed, so no [resolveProvider].  See
         {!page-"feature-command-block"} for the command lenses and
         {!page-"feature-codelens-reference-counts"} for the counts. *)
      Some (CodeLensOptions.create ~resolveProvider:false ())

    method! config_completion : CompletionOptions.t option =
      (* [[[] opens a wikilink; [#] starts a fragment. See {!page-"feature-completion"}. *)
      Some (CompletionOptions.create ~triggerCharacters:[ "["; "#"; "(" ] ())

    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      (* Advertise UTF-16 position encoding (LSP mandatory baseline); all
         internal conversions default to it. See {!page-"feature-utf16-positions"}. *)
      { c with
        referencesProvider = Some (`Bool true)
      ; renameProvider =
          Some (`RenameOptions (RenameOptions.create ~prepareProvider:true ()))
      ; workspace =
          (* Every file and folder: moving a directory moves the notes in it.
             See {!page-"feature-rename".file_operations}. *)
          (let everything =
             FileOperationRegistrationOptions.create
               ~filters:
                 [ FileOperationFilter.create
                     ~scheme:"file"
                     ~pattern:(FileOperationPattern.create ~glob:"**" ())
                     ()
                 ]
           in
           Some
             (ServerCapabilities.create_workspace
                ~fileOperations:
                  (FileOperationOptions.create
                     ~willRename:everything
                     ~didRename:everything
                     ())
                ()))
      ; positionEncoding = Some PositionEncodingKind.UTF16
      ; executeCommandProvider =
          Some (ExecuteCommandOptions.create ~commands:[ Server.daily_note_command ] ())
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
      (* [initializationOptions] is forwarded raw: parsing it, and tolerating a
         malformed one, is {!Lsp_lib.Config}'s job. *)
      Option.iter root ~f:(fun root ->
        Server.initialize server ~root ?init_options:params.initializationOptions ());
      (* The client tells us here whether it can honor [window/showDocument];
         the daily-note command degrades when it cannot. See
         {!page-"feature-daily-notes"}. *)
      can_show_document
      <- (match params.capabilities.window with
          | Some { showDocument = Some { support }; _ } -> support
          | _ -> false);
      (* Anything the configuration asked for and could not have is reported
         here, once.  [showMessage] is one of the few notifications the
         protocol allows before the [initialize] result, and the alternative —
         staying quiet — makes a mistyped setting look like a missing feature.
         See {!page-"feature-configuration".tolerance}. *)
      List.iter (Server.config_warnings server) ~f:(fun message ->
        notify_back#send_notification
          (Linol.Lsp.Server_notification.ShowMessage
             (ShowMessageParams.create
                ~type_:MessageType.Warning
                ~message:(sprintf "oystermark: %s" message))));
      (* A disabled server answers every request emptily, which on its own is
         indistinguishable from a broken one — so it says once why.  Info, not
         Warning: this is what the configuration asked for.  See
         {!page-"feature-configuration".disable}. *)
      if Server.disabled server
      then
        notify_back#send_notification
          (Linol.Lsp.Server_notification.ShowMessage
             (ShowMessageParams.create
                ~type_:MessageType.Info
                ~message:
                  "oystermark: disabled by configuration (\"disable\": true); no \
                   features are offered"));
      let result = super#on_req_initialize ~notify_back params in
      { result with
        serverInfo =
          Some
            (InitializeResult.create_serverInfo
               ~name:"oystermark-lsp"
               ~version:(Oystermark.Version.to_string ())
               ())
      }

    method! on_req_code_lens
      ~notify_back:_
      ~id:_
      ~uri
      ~workDoneToken:_
      ~partialResultToken:_
      (_ : Linol_eio.Jsonrpc2.doc_state)
      : CodeLens.t list =
      Option.value (Server.code_lens server ~rel_path:(self#rel_path uri)) ~default:[]

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
      (params : DidSaveTextDocumentParams.t)
      : unit =
      let rel_path = self#rel_path params.textDocument.uri in
      Server.did_save server ~rel_path
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
      |> Option.map ~f:(fun list -> `CompletionList list)

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

    (** Run a server command.  The decision of {i what} to do is
        {!Lsp_lib.Server.execute_command}'s; this only turns the resulting
        {!Lsp_lib.Server.open_note} into the two protocol effects — create the
        file, then focus it.  See {!page-"feature-daily-notes"}.

        This overrides the dedicated hook rather than answering in
        {!on_request_unhandled}: [ExecuteCommand] is dispatched to
        [on_req_execute_command] before the unhandled path is consulted, so a
        branch there would never run. *)
    method! on_req_execute_command
      ~notify_back
      ~id:_
      ~workDoneToken:_
      (command : string)
      (arguments : Yojson.Safe.t list option)
      : Yojson.Safe.t =
      match Server.execute_command server ~command ~arguments with
      | None -> `Null
      | Some { uri; create; report_unfocused } ->
        (* The client's acknowledgement of the edit carries nothing this
           command needs. *)
        let ignore_response _ = () in
        Option.iter create ~f:(fun edit ->
          let _ =
            notify_back#send_request
              (Linol.Lsp.Server_request.WorkspaceApplyEdit
                 (ApplyWorkspaceEditParams.create ~edit ()))
              ignore_response
          in
          ());
        let created = Option.is_some create in
        (* Silent when the action that sent this command carried an edit that
           already opened the note: there is nothing to tell a reader who is
           looking at it.  See {!page-"feature-daily-notes".focus}. *)
        let report () =
          if report_unfocused then self#report_unfocused ~notify_back ~created uri
        in
        if can_show_document
        then (
          let _ =
            notify_back#send_request
              (Linol.Lsp.Server_request.ShowDocumentRequest
                 (ShowDocumentParams.create ~takeFocus:true ~uri ()))
              (function
                (* A client may advertise the capability and still decline: the
                 result is the only place that shows up. *)
                | Ok ({ success = true } : ShowDocumentResult.t) -> ()
                | Ok _ | Error _ -> report ())
          in
          ())
        else report ();
        (* A client that cannot focus still gets the note created; the path is
           the answer it can act on. *)
        `String (DocumentUri.to_path uri)

    (** Say that the note was not opened, and where it is.

        Focusing is [window/showDocument]'s job and the one effect the server
        cannot perform itself.  Where a client does not support it — or
        declines — the command would otherwise finish in complete silence,
        indistinguishable from one that failed.  See
        {!page-"feature-daily-notes".focus}. *)
    method
      private report_unfocused
      ~notify_back
      ~(created : bool)
      (uri : DocumentUri.t)
      : unit =
      notify_back#send_notification
        (Linol.Lsp.Server_notification.ShowMessage
           (ShowMessageParams.create
              ~type_:MessageType.Info
              ~message:
                (sprintf
                   "oystermark: %s %s — this client cannot open documents on request \
                    (window/showDocument)"
                   (if created then "created" else "daily note is at")
                   (DocumentUri.to_path uri))))

    method private renames (params : RenameFilesParams.t) =
      List.map params.files ~f:(fun (file : FileRename.t) ->
        ( self#rel_path (DocumentUri.of_string file.oldUri)
        , self#rel_path (DocumentUri.of_string file.newUri) ))

    (** [didRenameFiles] has no dedicated hook either. *)
    method! on_notification_unhandled ~notify_back:_ notification =
      match notification with
      | Linol.Lsp.Client_notification.DidRenameFiles params ->
        Server.did_rename_files server ~renames:(self#renames params)
      | _ -> ()

    (** [references], [prepareRename], [rename] and [willRenameFiles] have no
        dedicated hook in {!Linol_eio.Jsonrpc2.server}, so they arrive here. *)
    method! on_request_unhandled
      : type r. notify_back:_ -> id:_ -> r Linol.Lsp.Client_request.t -> r =
      fun ~notify_back ~id:_ (req : r Linol.Lsp.Client_request.t) ->
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
        | Linol.Lsp.Client_request.WillRenameFiles params ->
          Some (Server.will_rename_files server ~renames:(self#renames params))
        | _ -> failwith "unhandled request"
  end

(** A starting [oysterlsp.json], on stdout.

    Every key at its default, with the [$schema] association first so the file
    documents itself in any editor with a JSON language server. The point is a
    file to edit rather than a file to keep: defaults written down are frozen,
    and a key left out is a key that follows this server as it changes. See
    {!page-"feature-configuration".schema_file}. *)
let print_default_config () : unit =
  let fields =
    match Lsp_lib.Config.to_json Lsp_lib.Config.default with
    | `Assoc fields -> fields
    | other -> [ "config", other ]
  in
  print_endline
    (Yojson.Safe.pretty_to_string
       (`Assoc (("$schema", `String Lsp_lib.Config.schema_url) :: fields)))
;;

let run_server () =
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

(* Anything but the flag starts the server: an editor spawns this with no
   arguments, and an unrecognized one must not stop it from doing so. *)
let () =
  match Sys.get_argv () |> Array.to_list |> List.tl with
  | Some [ "--print-default-config" ] -> print_default_config ()
  | _ -> run_server ()
;;
