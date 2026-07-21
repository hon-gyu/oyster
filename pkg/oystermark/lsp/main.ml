(** Oystermark LSP server entrypoint. *)

open Core
open Linol_eio

let build_vault = Oystermark.Vault.of_root_path ~skip_expand:true

class oystermark_server ~sw =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super

    (** Current vault state.  [None] before [initialize]. *)
    val mutable vault : Oystermark.Vault.t option = None

    (** Vault-relative paths of every document the editor currently has
        open.  Used by [on_notif_doc_did_save] to refresh diagnostics
        across open docs after a save.  Does {i not} track buffer content
        — feature handlers read from disk.  See
        {!page-"feature-document-sync"}. *)
    val open_docs : String.Hash_set.t = String.Hash_set.create ()

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
      ; renameProvider = Some (`RenameOptions (RenameOptions.create ~prepareProvider:true ()))
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

    method! on_req_initialize
      ~notify_back
      (params : InitializeParams.t)
      : InitializeResult.t =
      let root =
        match params.rootUri with
        | Some uri -> Some (DocumentUri.to_path uri)
        | None -> Option.join params.rootPath
      in
      Option.iter root ~f:(fun r -> vault <- Some (build_vault r));
      super#on_req_initialize ~notify_back params

    method private rebuild_vault : unit =
      match vault with
      | None -> ()
      | Some v -> vault <- Some (build_vault v.vault_root)

    method private rel_path_of_uri (uri : DocumentUri.t) : string =
      let file_path = DocumentUri.to_path uri in
      match vault with
      | None -> file_path
      | Some v ->
        let prefix = v.vault_root ^ "/" in
        let plen = String.length prefix in
        if
          String.length file_path >= plen
          && String.equal (String.sub file_path ~pos:0 ~len:plen) prefix
        then String.sub file_path ~pos:plen ~len:(String.length file_path - plen)
        else file_path

    method private read_file (rp : string) : string option =
      match vault with
      | None -> None
      | Some v ->
        let fp = Filename.concat v.vault_root rp in
        (try Some (In_channel.read_all fp) with
         | _ -> None)

    method
      private publish_diagnostics
      ~notify_back
      ~(uri : DocumentUri.t)
      ~(content : string)
      : unit =
      match vault with
      | None -> ()
      | Some v ->
        let rel_path = self#rel_path_of_uri uri in
        let diags = Lsp_lib.Diagnostics.compute ~index:v.index ~rel_path ~content () in
        let lsp_diags =
          List.map diags ~f:(fun (d : Lsp_lib.Diagnostics.diagnostic) ->
            let start_pos = Lsp_lib.Util.position_of_byte_offset content d.first_byte in
            let end_pos = Lsp_lib.Util.position_of_byte_offset content d.last_byte in
            let to_lsp_pos (line, character) = Position.create ~line ~character in
            let range =
              Range.create ~start:(to_lsp_pos start_pos) ~end_:(to_lsp_pos end_pos)
            in
            Diagnostic.create
              ~range
              ~severity:DiagnosticSeverity.Warning
              ~source:"oystermark"
              ~message:(`String d.message)
              ())
        in
        notify_back#send_diagnostic lsp_diags

    method on_notif_doc_did_open ~notify_back doc ~(content : string) : unit =
      let uri = doc.TextDocumentItem.uri in
      let rel_path = self#rel_path_of_uri uri in
      Hash_set.add open_docs rel_path;
      self#rebuild_vault;
      self#publish_diagnostics ~notify_back ~uri ~content

    method on_notif_doc_did_close ~notify_back:_ doc : unit =
      let rel_path = self#rel_path_of_uri doc.TextDocumentIdentifier.uri in
      Hash_set.remove open_docs rel_path

    (** Recompute diagnostics live against the in-flight buffer so the
        user sees squigglies update as they type.  Feature handlers
        ([find_references], [inlay_hints], etc.) still answer against
        disk — they will only reflect the edit after a save.  See
        {!page-"feature-document-sync"}. *)
    method on_notif_doc_did_change
      ~notify_back
      doc
      _changes
      ~old_content:_
      ~(new_content : string)
      : unit =
      let uri = doc.VersionedTextDocumentIdentifier.uri in
      self#publish_diagnostics ~notify_back ~uri ~content:new_content

    (** Rebuild the vault, then republish diagnostics for every open
        document — this is the one moment where a stale warning in a
        sibling buffer (e.g. an [[[b]]] link that was just resolved by
        creating [b.md]) gets cleared.  See
        {!page-"feature-document-sync"}. *)
    method! on_notif_doc_did_save
      ~notify_back
      (_params : DidSaveTextDocumentParams.t)
      : unit =
      self#rebuild_vault;
      match vault with
      | None -> ()
      | Some v ->
        Hash_set.iter open_docs ~f:(fun rel_path ->
          let full = Filename.concat v.vault_root rel_path in
          match self#read_file rel_path with
          | None -> ()
          | Some content ->
            let uri = DocumentUri.of_path full in
            self#publish_diagnostics ~notify_back ~uri ~content)

    method! on_req_hover
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(pos : Position.t)
      ~workDoneToken:_
      (doc_st : doc_state)
      : Hover.t option =
      match vault with
      | None -> None
      | Some v ->
        let rel_path = self#rel_path_of_uri uri in
        let read_file = self#read_file in
        (match
           Lsp_lib.Hover.hover
             ~index:v.index
             ~rel_path
             ~content:doc_st.content
             ~line:pos.line
             ~character:pos.character
             ~read_file
             ()
         with
         | None -> None
         | Some (text, first_byte, last_byte) ->
           let contents =
             `MarkupContent (MarkupContent.create ~kind:MarkupKind.Markdown ~value:text)
           in
           let range =
             let start_pos =
               Lsp_lib.Util.position_of_byte_offset doc_st.content first_byte
             in
             let end_pos =
               Lsp_lib.Util.position_of_byte_offset doc_st.content last_byte
             in
             let to_lsp_pos (line, character) = Position.create ~line ~character in
             Some (Range.create ~start:(to_lsp_pos start_pos) ~end_:(to_lsp_pos end_pos))
           in
           Some (Hover.create ~contents ?range ()))

    method! on_req_definition
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(pos : Position.t)
      ~workDoneToken:_
      ~partialResultToken:_
      (doc_st : doc_state)
      : [ `Location of Location.t list | `LocationLink of LocationLink.t list ] option =
      match vault with
      | None -> None
      | Some v ->
        let rel_path = self#rel_path_of_uri uri in
        (match
           Lsp_lib.Go_to_definition.go_to_definition
             ~read_file:self#read_file
             ~index:v.index
             ~rel_path
             ~content:doc_st.content
             ~line:pos.line
             ~character:pos.character
             ()
         with
         | None -> None
         | Some { path; line; character } ->
           let full = Filename.concat v.vault_root path in
           let uri = DocumentUri.of_path full in
           let pos = Position.create ~line ~character in
           let range = Range.create ~start:pos ~end_:pos in
           Some (`Location [ Location.create ~uri ~range ]))

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
      match vault with
      | None -> None
      | Some v ->
        let rel_path = self#rel_path_of_uri uri in
        let content = Option.value (self#read_file rel_path) ~default:"" in
        let symbols =
          Lsp_lib.Document_outline.document_outline
            ~index:v.index
            ~rel_path
            ~content
        in
        let to_position offset =
          let line, character = Lsp_lib.Util.position_of_byte_offset content offset in
          Position.create ~line ~character
        in
        let rec to_lsp (symbol : Lsp_lib.Document_outline.symbol) =
          let range =
            Range.create
              ~start:(to_position symbol.first_byte)
              ~end_:(to_position symbol.last_byte)
          in
          let selectionRange =
            Range.create
              ~start:(to_position symbol.selection_first_byte)
              ~end_:(to_position symbol.selection_last_byte)
          in
          let kind, detail =
            match symbol.kind with
            | Heading level -> SymbolKind.Namespace, sprintf "heading %d" level
            | Block_id -> SymbolKind.Key, "block id"
            | Attribute_id -> SymbolKind.Key, "attribute id"
          in
          DocumentSymbol.create
            ~name:symbol.name
            ~kind
            ~detail
            ~range
            ~selectionRange
            ~children:(List.map symbol.children ~f:to_lsp)
            ()
        in
        Some (`DocumentSymbol (List.map symbols ~f:to_lsp))

    method! on_req_code_action
      ~notify_back:_
      ~id:_
      (params : CodeActionParams.t)
      : CodeActionResult.t =
      let quick_fixes_requested =
        match params.context.only with
        | None -> true
        | Some kinds -> List.mem kinds CodeActionKind.QuickFix ~equal:Poly.equal
      in
      if not quick_fixes_requested
      then Some []
      else
        match vault with
        | None -> Some []
        | Some v ->
          let uri = params.textDocument.uri in
          let rel_path = self#rel_path_of_uri uri in
          let content = Option.value (self#read_file rel_path) ~default:"" in
          let first_byte =
            Lsp_lib.Util.byte_offset_of_position
              content
              ~line:params.range.start.line
              ~character:params.range.start.character
          in
          let last_byte =
            Lsp_lib.Util.byte_offset_of_position
              content
              ~line:params.range.end_.line
              ~character:params.range.end_.character
          in
          (match
             Lsp_lib.Create_unresolved_note.action_at_range
               ~index:v.index
               ~rel_path
               ~content
               ~first_byte
               ~last_byte
           with
           | None -> Some []
           | Some action ->
             let target_uri =
               DocumentUri.of_path (Filename.concat v.vault_root action.rel_path)
             in
             let create =
               `CreateFile
                 (CreateFile.create
                    ~uri:target_uri
                    ~options:
                      (CreateFileOptions.create ~ignoreIfExists:false ~overwrite:false ())
                    ())
             in
             let zero = Position.create ~line:0 ~character:0 in
             let textDocument =
               OptionalVersionedTextDocumentIdentifier.create ~uri:target_uri ()
             in
             let initial_text = "# " ^ action.title ^ "\n" in
             let initialize =
               `TextDocumentEdit
                 (TextDocumentEdit.create
                    ~textDocument
                    ~edits:
                      [ `TextEdit
                          (TextEdit.create
                             ~range:(Range.create ~start:zero ~end_:zero)
                             ~newText:initial_text)
                      ])
             in
             let edit = WorkspaceEdit.create ~documentChanges:[ create; initialize ] () in
             let title = sprintf "Create note \"%s\"" action.rel_path in
             Some
               [ `CodeAction
                   (CodeAction.create
                      ~title
                      ~kind:CodeActionKind.QuickFix
                      ~isPreferred:true
                      ~edit
                      ())
               ])

    method! on_request_unhandled
      : type r. notify_back:_ -> id:_ -> r Linol.Lsp.Client_request.t -> r =
      fun ~notify_back:_ ~id:_ (req : r Linol.Lsp.Client_request.t) ->
        match req with
        | Linol.Lsp.Client_request.TextDocumentReferences params ->
          (match vault with
           | None -> None
           | Some v ->
             let uri = params.textDocument.uri in
             let pos = params.position in
             let rel_path = self#rel_path_of_uri uri in
             let content =
               match self#read_file rel_path with
               | Some c -> c
               | None -> ""
             in
             let refs =
               Lsp_lib.Find_references.find_references
                 ~index:v.index
                 ~docs:v.docs
                 ~rel_path
                 ~content
                 ~line:pos.line
                 ~character:pos.character
                 ()
             in
             let locations =
               List.map refs ~f:(fun (r : Lsp_lib.Find_references.reference) ->
                 let full = Filename.concat v.vault_root r.rel_path in
                 let ref_uri = DocumentUri.of_path full in
                 let ref_content =
                   match self#read_file r.rel_path with
                   | Some c -> c
                   | None -> ""
                 in
                 let start_pos =
                   Lsp_lib.Util.position_of_byte_offset ref_content r.first_byte
                 in
                 let end_pos =
                   Lsp_lib.Util.position_of_byte_offset ref_content r.last_byte
                 in
                 let to_lsp_pos (line, character) = Position.create ~line ~character in
                 let range =
                   Range.create ~start:(to_lsp_pos start_pos) ~end_:(to_lsp_pos end_pos)
                 in
                 Location.create ~uri:ref_uri ~range)
             in
             Some locations)
        | Linol.Lsp.Client_request.TextDocumentPrepareRename params ->
          (match vault with
           | None -> None
           | Some v ->
             let uri = params.textDocument.uri in
             let rel_path = self#rel_path_of_uri uri in
             let content = Option.value (self#read_file rel_path) ~default:"" in
             (match
                Lsp_lib.Find_references.detect_target
                  ~index:v.index
                  ~rel_path
                  ~content
                  ~line:params.position.line
                  ~character:params.position.character
              with
              | None -> None
              | Some _ ->
                Some (Range.create ~start:params.position ~end_:params.position)))
        | Linol.Lsp.Client_request.TextDocumentRename params ->
          (match vault with
           | None -> WorkspaceEdit.create ()
           | Some v ->
             let uri = params.textDocument.uri in
             let rel_path = self#rel_path_of_uri uri in
             let content = Option.value (self#read_file rel_path) ~default:"" in
             let edits =
               Lsp_lib.Rename.rename
                 ~index:v.index
                 ~docs:v.docs
                 ~read_file:self#read_file
                 ~rel_path
                 ~content
                 ~line:params.position.line
                 ~character:params.position.character
                 ~new_name:params.newName
                 ()
             in
             let grouped =
               List.group edits ~break:(fun a b -> not (String.equal a.rel_path b.rel_path))
             in
             let documentChanges =
               List.filter_map grouped ~f:(function
                 | [] -> None
                 | ({ Lsp_lib.Rename.rel_path; _ } :: _ as edits) ->
                   let content = Option.value (self#read_file rel_path) ~default:"" in
                   let uri = DocumentUri.of_path (Filename.concat v.vault_root rel_path) in
                   let textDocument =
                     OptionalVersionedTextDocumentIdentifier.create ~uri ()
                   in
                   let edits =
                     List.map edits ~f:(fun (edit : Lsp_lib.Rename.edit) ->
                       let to_position offset =
                         let line, character =
                           Lsp_lib.Util.position_of_byte_offset content offset
                         in
                         Position.create ~line ~character
                       in
                       `TextEdit
                         (TextEdit.create
                            ~range:
                              (Range.create
                                 ~start:(to_position edit.first_byte)
                                 ~end_:(to_position edit.last_byte))
                            ~newText:edit.new_text))
                   in
                   Some (`TextDocumentEdit (TextDocumentEdit.create ~edits ~textDocument)))
             in
             let documentChanges =
               match
                 Lsp_lib.Find_references.detect_target
                   ~index:v.index
                   ~rel_path
                   ~content
                   ~line:params.position.line
                   ~character:params.position.character
               with
               | Some (Path_only { path }) when Lsp_lib.Rename.valid_note_name params.newName ->
                 let new_path =
                   Lsp_lib.Rename.renamed_note_path ~path ~new_name:params.newName
                 in
                 let oldUri = DocumentUri.of_path (Filename.concat v.vault_root path) in
                 let newUri = DocumentUri.of_path (Filename.concat v.vault_root new_path) in
                 documentChanges @ [ `RenameFile (RenameFile.create ~oldUri ~newUri ()) ]
               | _ -> documentChanges
             in
             WorkspaceEdit.create ~documentChanges ())
        | _ -> failwith "unhandled request"

    method! on_req_completion
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(pos : Position.t)
      ~ctx:_
      ~workDoneToken:_
      ~partialResultToken:_
      (doc_st : doc_state)
      : [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option =
      match vault with
      | None -> None
      | Some v ->
        let rel_path = self#rel_path_of_uri uri in
        let items =
          Lsp_lib.Completion.complete
            ~index:v.index
            ~rel_path
            ~content:doc_st.content
            ~line:pos.line
            ~character:pos.character
            ()
        in
        let to_lsp (i : Lsp_lib.Completion.item) : CompletionItem.t =
          let kind =
            match i.kind with
            | Lsp_lib.Completion.File -> CompletionItemKind.File
            | Lsp_lib.Completion.Reference -> CompletionItemKind.Reference
          in
          CompletionItem.create
            ~label:i.label
            ?detail:i.detail
            ?filterText:i.filter_text
            ?insertText:i.insert_text
            ~kind
            ()
        in
        Some (`List (List.map items ~f:to_lsp))

    method! on_req_inlay_hint
      ~notify_back:_
      ~id:_
      ~(uri : DocumentUri.t)
      ~(range : Range.t)
      ()
      : InlayHint.t list option =
      match vault with
      | None -> None
      | Some v ->
        let rel_path = self#rel_path_of_uri uri in
        let content =
          match self#read_file rel_path with
          | Some c -> c
          | None -> ""
        in
        let range_start_line = range.Range.start.line in
        let range_end_line = range.end_.line + 1 in
        let hints =
          Lsp_lib.Inlay_hints.inlay_hints
            ~docs:v.docs
            ~rel_path
            ~content
            ~range_start_line
            ~range_end_line
            ()
        in
        (match hints with
         | [] -> Some []
         | _ ->
           let lsp_hints =
             List.map hints ~f:(fun (h : Lsp_lib.Inlay_hints.hint) ->
               let position = Position.create ~line:h.line ~character:h.character in
               InlayHint.create
                 ~position
                 ~label:(`String h.label)
                 ~kind:InlayHintKind.Parameter
                 ~paddingLeft:true
                 ())
           in
           Some lsp_hints)
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
