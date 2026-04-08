(** Oystermark LSP server entrypoint. *)

open Core
open Linol_eio

let build_vault = Oystermark.Vault.of_root_path ~skip_expand:true

class oystermark_server =
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

    method spawn_query_handler f = Linol_eio.spawn f
    method! config_definition = Some (`Bool true)
    method! config_hover = Some (`Bool true)
    method! config_inlay_hints = Some (`Bool true)

    method! config_modify_capabilities (c : ServerCapabilities.t) : ServerCapabilities.t =
      { c with referencesProvider = Some (`Bool true) }

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
             ~index:v.index
             ~rel_path
             ~content:doc_st.content
             ~line:pos.line
             ~character:pos.character
             ()
         with
         | None -> None
         | Some { path; line } ->
           let full = Filename.concat v.vault_root path in
           let uri = DocumentUri.of_path full in
           let pos = Position.create ~line ~character:0 in
           let range = Range.create ~start:pos ~end_:pos in
           Some (`Location [ Location.create ~uri ~range ]))

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
        | _ -> failwith "unhandled request"

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
  let s = new oystermark_server in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  Linol_eio.Jsonrpc2.run server
;;
