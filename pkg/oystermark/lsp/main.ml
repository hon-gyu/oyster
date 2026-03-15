(** Oystermark LSP server entrypoint. *)

open Linol_eio

(** Build a vault index by scanning the vault root directory. *)
let build_vault_index (vault_root : string) : Oystermark.Vault.Index.t =
  let all_entries = Oystermark.Vault.list_entries vault_root in
  let is_dir p = String.length p > 0 && p.[String.length p - 1] = '/' in
  let dirs = List.filter is_dir all_entries in
  let files = List.filter (fun p -> not (is_dir p)) all_entries in
  let is_md p = Filename.check_suffix p ".md" in
  let md_files = List.filter is_md files in
  let other_files = List.filter (fun p -> not (is_md p)) files in
  let md_docs =
    List.filter_map
      (fun rel_path ->
        let full_path = Filename.concat vault_root rel_path in
        try
          let content =
            In_channel.with_open_text full_path In_channel.input_all
          in
          let doc = Oystermark.Parse.of_string content in
          Some (rel_path, doc)
        with
        | _ -> None)
      md_files
  in
  Oystermark.Vault.build_index ~md_docs ~other_files ~dirs
;;

class oystermark_server =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super

    val mutable vault_root : string option = None

    val mutable index : Oystermark.Vault.Index.t =
      { Oystermark.Vault.Index.files = []; dirs = [] }

    method spawn_query_handler f = Linol_eio.spawn f

    method! config_definition = Some (`Bool true)

    method! config_sync_opts =
      TextDocumentSyncOptions.create
        ~change:TextDocumentSyncKind.Full
        ~openClose:true
        ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
        ()

    method! filter_text_document uri =
      Filename.check_suffix (DocumentUri.to_path uri) ".md"

    method! on_req_initialize ~notify_back (params : InitializeParams.t) =
      let root =
        match params.rootUri with
        | Some uri -> Some (DocumentUri.to_path uri)
        | None -> Option.join params.rootPath
      in
      vault_root <- root;
      self#rebuild_index;
      super#on_req_initialize ~notify_back params

    method private rebuild_index =
      match vault_root with
      | None -> ()
      | Some root -> index <- build_vault_index root

    method on_notif_doc_did_open ~notify_back:_ _doc ~content:_ =
      self#rebuild_index

    method on_notif_doc_did_close ~notify_back:_ _doc = ()

    method on_notif_doc_did_change ~notify_back:_ _doc _changes ~old_content:_
        ~new_content:_ =
      ()

    method! on_notif_doc_did_save ~notify_back:_ _params = self#rebuild_index

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ (doc_st : doc_state) =
      match vault_root with
      | None -> None
      | Some root ->
        let file_path = DocumentUri.to_path uri in
        let rel_path =
          let prefix = root ^ "/" in
          let plen = String.length prefix in
          if String.length file_path >= plen
             && String.sub file_path 0 plen = prefix
          then String.sub file_path plen (String.length file_path - plen)
          else file_path
        in
        let read_file rp =
          let fp = Filename.concat root rp in
          try Some (In_channel.with_open_text fp In_channel.input_all)
          with _ -> None
        in
        (match
           Lsp_lib.go_to_definition
             ~index
             ~rel_path
             ~content:doc_st.content
             ~line:pos.line
             ~character:pos.character
             ~read_file
         with
         | None -> None
         | Some { path; line } ->
           let full = Filename.concat root path in
           let uri = DocumentUri.of_path full in
           let pos = Position.create ~line ~character:0 in
           let range = Range.create ~start:pos ~end_:pos in
           Some (`Location [ Location.create ~uri ~range ]))
  end

let () =
  Eio_main.run @@ fun env ->
  let s = new oystermark_server in
  let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
  Linol_eio.Jsonrpc2.run server
;;
