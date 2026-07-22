(** Implementation of {!Server}

    Two content sources are in play and the difference is deliberate:
    diagnostics and the cursor-position features answer against
    {!buffer_content} (what the user sees, unsaved edits included), while
    everything vault-wide answers against {!disk_content}.  See
    {!page-"feature-document-sync"}. *)

open Core

(* Feature modules are aliased before [open Lsp.Types] so that names the
   protocol also claims — [Hover], [Diagnostic]-adjacent ones — keep
   referring to the pure logic layer. *)
module Feature = struct
  module Completion = Completion
  module Create_unresolved_note = Create_unresolved_note
  module Diagnostics = Diagnostics
  module Document_outline = Document_outline
  module Find_references = Find_references
  module Go_to_definition = Go_to_definition
  module Hover = Hover
  module Inlay_hints = Inlay_hints
  module Rename = Rename
end

open Linol_lsp.Lsp.Types

(* State
   ====== *)

type t =
  { mutable vault : Oystermark.Vault.t option
    (** [None] until {!initialize} has seen a workspace root. *)
  ; open_docs : string String.Table.t
    (** Vault-relative path → in-flight buffer content for every document
          the editor currently has open.  Diagnostics and the
          cursor-position features answer against this; the rest read from
          disk.  See {!page-"feature-document-sync"}. *)
  }

let build_vault = Oystermark.Vault.of_root_path ~skip_expand:true
let create () : t = { vault = None; open_docs = String.Table.create () }
let initialize (t : t) ~(root : string) : unit = t.vault <- Some (build_vault root)

let rebuild_vault (t : t) : unit =
  match t.vault with
  | None -> ()
  | Some v -> t.vault <- Some (build_vault v.vault_root)
;;

let vault_root (t : t) : string option =
  Option.map t.vault ~f:(fun (v : Oystermark.Vault.t) -> v.vault_root)
;;

(* Paths and URIs
   =============== *)

let rel_path_of_uri (t : t) (uri : DocumentUri.t) : string =
  let file_path = DocumentUri.to_path uri in
  match t.vault with
  | None -> file_path
  | Some v ->
    (match String.chop_prefix file_path ~prefix:(v.vault_root ^ "/") with
     | Some rel -> rel
     | None -> file_path)
;;

let uri_of_rel_path (t : t) (rel_path : string) : DocumentUri.t =
  match t.vault with
  | None -> DocumentUri.of_path rel_path
  | Some v -> DocumentUri.of_path (Filename.concat v.vault_root rel_path)
;;

let read_file (t : t) (rel_path : string) : string option =
  match t.vault with
  | None -> None
  | Some v ->
    (try Some (In_channel.read_all (Filename.concat v.vault_root rel_path)) with
     | _ -> None)
;;

let disk_content (t : t) (rel_path : string) : string =
  Option.value (read_file t rel_path) ~default:""
;;

let buffer_content (t : t) (rel_path : string) : string =
  match Hashtbl.find t.open_docs rel_path with
  | Some content -> content
  | None -> disk_content t rel_path
;;

(* Position conversion
   ==================== *)

let position_of_byte (content : string) (offset : int) : Position.t =
  let line, character = Lsp_util.position_of_byte_offset content offset in
  Position.create ~line ~character
;;

let range_of_bytes (content : string) ~(first_byte : int) ~(last_byte : int) : Range.t =
  Range.create
    ~start:(position_of_byte content first_byte)
    ~end_:(position_of_byte content last_byte)
;;

let byte_of_position (content : string) ~(line : int) ~(character : int) : int =
  Lsp_util.byte_offset_of_position content ~line ~character
;;

(* Document synchronization
   ========================= *)

let diagnostics (t : t) ~(rel_path : string) ~(content : string) : Diagnostic.t list =
  match t.vault with
  | None -> []
  | Some v ->
    Feature.Diagnostics.compute ~index:v.index ~rel_path ~content ()
    |> List.map ~f:(fun (d : Feature.Diagnostics.diagnostic) ->
      Diagnostic.create
        ~range:(range_of_bytes content ~first_byte:d.first_byte ~last_byte:d.last_byte)
        ~severity:DiagnosticSeverity.Warning
        ~source:"oystermark"
        ~message:(`String d.message)
        ())
;;

let did_open (t : t) ~(rel_path : string) ~(content : string) : Diagnostic.t list =
  Hashtbl.set t.open_docs ~key:rel_path ~data:content;
  rebuild_vault t;
  diagnostics t ~rel_path ~content
;;

let did_change (t : t) ~(rel_path : string) ~(content : string) : Diagnostic.t list =
  Hashtbl.set t.open_docs ~key:rel_path ~data:content;
  diagnostics t ~rel_path ~content
;;

let did_close (t : t) ~(rel_path : string) : unit = Hashtbl.remove t.open_docs rel_path

let did_save (t : t) : (string * Diagnostic.t list) list =
  rebuild_vault t;
  match t.vault with
  | None -> []
  | Some _ ->
    Hashtbl.keys t.open_docs
    |> List.sort ~compare:String.compare
    |> List.filter_map ~f:(fun rel_path ->
      match read_file t rel_path with
      | None -> None
      | Some content -> Some (rel_path, diagnostics t ~rel_path ~content))
;;

(* Features
   ========= *)

let hover (t : t) ~(rel_path : string) ~(line : int) ~(character : int) : Hover.t option =
  match t.vault with
  | None -> None
  | Some v ->
    let content = buffer_content t rel_path in
    (match
       Feature.Hover.hover
         ~index:v.index
         ~rel_path
         ~content
         ~line
         ~character
         ~read_file:(read_file t)
         ()
     with
     | None -> None
     | Some (text, first_byte, last_byte) ->
       let contents =
         `MarkupContent (MarkupContent.create ~kind:MarkupKind.Markdown ~value:text)
       in
       Some
         (Hover.create
            ~contents
            ~range:(range_of_bytes content ~first_byte ~last_byte)
            ()))
;;

let definition (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : Location.t list option
  =
  match t.vault with
  | None -> None
  | Some v ->
    (match
       Feature.Go_to_definition.go_to_definition
         ~read_file:(read_file t)
         ~index:v.index
         ~rel_path
         ~content:(buffer_content t rel_path)
         ~line
         ~character
         ()
     with
     | None -> None
     | Some { path; line; character } ->
       let pos = Position.create ~line ~character in
       Some
         [ Location.create
             ~uri:(uri_of_rel_path t path)
             ~range:(Range.create ~start:pos ~end_:pos)
         ])
;;

let references (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : Location.t list option
  =
  match t.vault with
  | None -> None
  | Some v ->
    let refs =
      Feature.Find_references.find_references
        ~index:v.index
        ~docs:v.docs
        ~rel_path
        ~content:(disk_content t rel_path)
        ~line
        ~character
        ()
    in
    Some
      (List.map refs ~f:(fun (r : Feature.Find_references.reference) ->
         Location.create
           ~uri:(uri_of_rel_path t r.rel_path)
           ~range:
             (range_of_bytes
                (disk_content t r.rel_path)
                ~first_byte:r.first_byte
                ~last_byte:r.last_byte)))
;;

let prepare_rename (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : Range.t option
  =
  match t.vault with
  | None -> None
  | Some v ->
    (match
       Feature.Find_references.detect_target
         ~index:v.index
         ~rel_path
         ~content:(disk_content t rel_path)
         ~line
         ~character
     with
     | None -> None
     | Some _ ->
       let pos = Position.create ~line ~character in
       Some (Range.create ~start:pos ~end_:pos))
;;

let rename
      (t : t)
      ~(rel_path : string)
      ~(line : int)
      ~(character : int)
      ~(new_name : string)
  : WorkspaceEdit.t
  =
  match t.vault with
  | None -> WorkspaceEdit.create ()
  | Some v ->
    let content = disk_content t rel_path in
    let edits =
      Feature.Rename.rename
        ~index:v.index
        ~docs:v.docs
        ~read_file:(read_file t)
        ~rel_path
        ~content
        ~line
        ~character
        ~new_name
        ()
    in
    let text_document_edits =
      List.group edits ~break:(fun a b -> not (String.equal a.rel_path b.rel_path))
      |> List.filter_map ~f:(function
        | [] -> None
        | { Feature.Rename.rel_path; _ } :: _ as edits ->
          let content = disk_content t rel_path in
          let textDocument =
            OptionalVersionedTextDocumentIdentifier.create
              ~uri:(uri_of_rel_path t rel_path)
              ()
          in
          let edits =
            List.map edits ~f:(fun (edit : Feature.Rename.edit) ->
              `TextEdit
                (TextEdit.create
                   ~range:
                     (range_of_bytes
                        content
                        ~first_byte:edit.first_byte
                        ~last_byte:edit.last_byte)
                   ~newText:edit.new_text))
          in
          Some (`TextDocumentEdit (TextDocumentEdit.create ~edits ~textDocument)))
    in
    let documentChanges =
      match
        Feature.Find_references.detect_target
          ~index:v.index
          ~rel_path
          ~content
          ~line
          ~character
      with
      | Some (Path_only { path }) when Feature.Rename.valid_note_name new_name ->
        let new_path = Feature.Rename.renamed_note_path ~path ~new_name in
        let rename_file =
          RenameFile.create
            ~oldUri:(uri_of_rel_path t path)
            ~newUri:(uri_of_rel_path t new_path)
            ()
        in
        text_document_edits @ [ `RenameFile rename_file ]
      | _ -> text_document_edits
    in
    WorkspaceEdit.create ~documentChanges ()
;;

let document_symbol (t : t) ~(rel_path : string) : DocumentSymbol.t list option =
  match t.vault with
  | None -> None
  | Some v ->
    let content = disk_content t rel_path in
    let to_position offset = position_of_byte content offset in
    let rec to_lsp (symbol : Feature.Document_outline.symbol) : DocumentSymbol.t =
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
        ~range:
          (Range.create
             ~start:(to_position symbol.first_byte)
             ~end_:(to_position symbol.last_byte))
        ~selectionRange:
          (Range.create
             ~start:(to_position symbol.selection_first_byte)
             ~end_:(to_position symbol.selection_last_byte))
        ~children:(List.map symbol.children ~f:to_lsp)
        ()
    in
    Feature.Document_outline.document_outline ~index:v.index ~rel_path ~content
    |> List.map ~f:to_lsp
    |> Option.return
;;

let code_action
      (t : t)
      ?(only : CodeActionKind.t list option)
      ~(rel_path : string)
      ~(start_line : int)
      ~(start_character : int)
      ~(end_line : int)
      ~(end_character : int)
      ()
  : CodeAction.t list
  =
  let quick_fixes_requested =
    match only with
    | None -> true
    | Some kinds -> List.mem kinds CodeActionKind.QuickFix ~equal:Poly.equal
  in
  match t.vault with
  | _ when not quick_fixes_requested -> []
  | None -> []
  | Some v ->
    let content = disk_content t rel_path in
    (match
       Feature.Create_unresolved_note.action_at_range
         ~index:v.index
         ~rel_path
         ~content
         ~first_byte:
           (byte_of_position content ~line:start_line ~character:start_character)
         ~last_byte:(byte_of_position content ~line:end_line ~character:end_character)
     with
     | None -> []
     | Some action ->
       let target_uri = uri_of_rel_path t action.rel_path in
       let create =
         `CreateFile
           (CreateFile.create
              ~uri:target_uri
              ~options:
                (CreateFileOptions.create ~ignoreIfExists:false ~overwrite:false ())
              ())
       in
       let zero = Position.create ~line:0 ~character:0 in
       let initialize =
         `TextDocumentEdit
           (TextDocumentEdit.create
              ~textDocument:
                (OptionalVersionedTextDocumentIdentifier.create ~uri:target_uri ())
              ~edits:
                [ `TextEdit
                    (TextEdit.create
                       ~range:(Range.create ~start:zero ~end_:zero)
                       ~newText:("# " ^ action.title ^ "\n"))
                ])
       in
       [ CodeAction.create
           ~title:(sprintf "Create note \"%s\"" action.rel_path)
           ~kind:CodeActionKind.QuickFix
           ~isPreferred:true
           ~edit:(WorkspaceEdit.create ~documentChanges:[ create; initialize ] ())
           ()
       ])
;;

let completion (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : CompletionItem.t list option
  =
  match t.vault with
  | None -> None
  | Some v ->
    Feature.Completion.complete
      ~index:v.index
      ~rel_path
      ~content:(buffer_content t rel_path)
      ~line
      ~character
      ()
    |> List.map ~f:(fun (i : Feature.Completion.item) ->
      let kind =
        match i.kind with
        | Feature.Completion.File -> CompletionItemKind.File
        | Feature.Completion.Reference -> CompletionItemKind.Reference
      in
      CompletionItem.create
        ~label:i.label
        ?detail:i.detail
        ?filterText:i.filter_text
        ?insertText:i.insert_text
        ~kind
        ())
    |> Option.return
;;

let inlay_hint (t : t) ~(rel_path : string) ~(start_line : int) ~(end_line : int)
  : InlayHint.t list option
  =
  match t.vault with
  | None -> None
  | Some v ->
    Feature.Inlay_hints.inlay_hints
      ~docs:v.docs
      ~rel_path
      ~content:(disk_content t rel_path)
      ~range_start_line:start_line
      ~range_end_line:(end_line + 1)
      ()
    |> List.map ~f:(fun (h : Feature.Inlay_hints.hint) ->
      InlayHint.create
        ~position:(Position.create ~line:h.line ~character:h.character)
        ~label:(`String h.label)
        ~kind:InlayHintKind.Parameter
        ~paddingLeft:true
        ())
    |> Option.return
;;
