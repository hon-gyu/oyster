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
  module Toc = Toc
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
  ; mutable config : Lsp_config.t
    (** Client-supplied settings, seen once at {!initialize}. *)
  ; mutable config_warnings : string list
    (** Everything the configuration sources asked for and could not have,
          gathered at {!initialize} for the adapter to report.  A setting
          ignored in silence is indistinguishable from one that does not
          exist.  See {!page-"feature-configuration".tolerance}. *)
  ; mutable daily_table : (Date.t * Daily_notes.Table.t) option
    (** Memoized recognition table with the date it was built for; a server
          left running overnight rebuilds it.  See
          {!page-"feature-daily-notes".recognition}. *)
  ; now : unit -> Date.t
    (** The clock, as a dependency: daily-note answers depend on today's date,
          and a test that cannot fix "today" would change meaning overnight. *)
  ; mutable workspace_root : string option
  ; project_prefix : string
  ; mutable nested_roots : string list
  ; mutable imported_roots : string list
  ; mutable nested_projects : (string * t) list
  }

let path_is_within ~root path =
  String.equal path root || String.is_prefix path ~prefix:(root ^ "/")
;;

let deepest_root roots path =
  List.filter roots ~f:(fun root -> path_is_within ~root path)
  |> List.max_elt ~compare:(fun a b -> Int.compare (String.length a) (String.length b))
;;

let build_vault ?(nested_roots = []) ?(imported_roots = []) ?(exclude = []) root =
  let excluded_by_config = Vault_fs.Fs_utils.Exclude.of_patterns exclude in
  Vault_fs.of_root_path
    ~skip_expand:true
    ~exclude:(fun path ->
      excluded_by_config path
      || deepest_root nested_roots path
         |> Option.value_map ~default:false ~f:(fun owner ->
           not (List.mem imported_roots owner ~equal:String.equal)))
    root
;;

let normalize_import path =
  if Filename.is_absolute path
  then Error "must be relative"
  else (
    let components =
      String.split path ~on:'/'
      |> List.filter ~f:(fun component ->
        not (String.is_empty component || String.equal component "."))
    in
    if List.is_empty components
    then Error "must name a descendant project"
    else if List.exists components ~f:(String.equal "..")
    then Error "must not contain .."
    else Ok (String.concat ~sep:"/" components))
;;

let validate_imports ~project_roots imports =
  List.fold imports ~init:([], []) ~f:(fun (accepted, warnings) import ->
    match normalize_import import with
    | Error reason -> accepted, sprintf "imports: %S %s" import reason :: warnings
    | Ok normalized when not (List.mem project_roots normalized ~equal:String.equal) ->
      ( accepted
      , sprintf "imports: %S does not name a descendant project" import :: warnings )
    | Ok normalized when List.mem accepted normalized ~equal:String.equal ->
      accepted, sprintf "imports: duplicate project %S" normalized :: warnings
    | Ok normalized -> normalized :: accepted, warnings)
  |> fun (accepted, warnings) -> List.rev accepted, List.rev warnings
;;

(** Today in the machine's own timezone — the real clock, used unless a caller
    substitutes one. *)
let system_today () : Date.t = Date.today ~zone:(Lazy.force Time_float_unix.Zone.local)

let create_project ?(now : unit -> Date.t = system_today) ?(project_prefix = "") () : t =
  { vault = None
  ; open_docs = String.Table.create ()
  ; config = Lsp_config.default
  ; config_warnings = []
  ; daily_table = None
  ; now
  ; workspace_root = None
  ; project_prefix
  ; nested_roots = []
  ; imported_roots = []
  ; nested_projects = []
  }
;;

let create ?now () = create_project ?now ()

let initialize_project
      (t : t)
      ~(root : string)
      ~(nested_roots : string list)
      ?(init_options : Yojson.Safe.t option)
      ()
  =
  let config, warnings = Lsp_config.load ~root ~init_options in
  let imported_roots, import_warnings =
    validate_imports ~project_roots:nested_roots config.imports
  in
  t.workspace_root <- Some root;
  t.nested_roots <- nested_roots;
  t.imported_roots <- imported_roots;
  t.config <- { config with imports = imported_roots };
  t.daily_table <- None;
  match config.disable with
  | true ->
    t.config_warnings <- [];
    t.vault <- None
  | false ->
    let daily_notes_warning =
      match Lsp_config.daily_notes_settings config with
      | Ok _ -> []
      | Error e -> [ sprintf "daily notes disabled: %s" e ]
    in
    t.config_warnings <- warnings @ import_warnings @ daily_notes_warning;
    t.vault
    <- Some (build_vault ~nested_roots ~imported_roots ~exclude:config.exclude root)
;;

let initialize (t : t) ~(root : string) ?(init_options : Yojson.Safe.t option) () : unit =
  let project_roots =
    Vault_fs.Fs_utils.walk ~root ()
    |> List.filter_map ~f:(fun path ->
      if String.equal (Filename.basename path) Lsp_config.file_name
      then Some (Filename.dirname path)
      else None)
    |> List.filter ~f:(fun path -> not (String.equal path "."))
    |> List.dedup_and_sort ~compare:String.compare
  in
  initialize_project t ~root ~nested_roots:project_roots ?init_options ();
  t.nested_projects
  <- List.map project_roots ~f:(fun prefix ->
       let child_root = Filename.concat root prefix in
       let child_nested_roots =
         List.filter_map project_roots ~f:(fun candidate ->
           match String.chop_prefix candidate ~prefix:(prefix ^ "/") with
           | Some relative -> Some relative
           | None -> None)
       in
       let child = create_project ~now:t.now ~project_prefix:prefix () in
       initialize_project
         child
         ~root:child_root
         ~nested_roots:child_nested_roots
         ?init_options
         ();
       prefix, child);
  let nested_warnings =
    List.concat_map t.nested_projects ~f:(fun (prefix, child) ->
      List.map child.config_warnings ~f:(sprintf "%s/%s" prefix))
  in
  t.config_warnings <- t.config_warnings @ nested_warnings
;;

(** Whether the configuration turned the server off.  The adapter reports it,
    since a server that answers nothing and says nothing is indistinguishable
    from a broken one.  See {!page-"feature-configuration".disable}. *)
let disabled (t : t) : bool =
  t.config.disable
  && List.for_all t.nested_projects ~f:(fun (_, child) -> child.config.disable)
;;

(** What the configuration sources asked for and could not have.  Empty when
    the configuration is clean; meaningful only after {!initialize}, which is
    also the only time it is computed.  See
    {!page-"feature-configuration".tolerance}. *)
let config_warnings (t : t) : string list = t.config_warnings

let vault_root (t : t) : string option =
  Option.map t.vault ~f:(fun (v : Oystermark.Vault.t) -> v.vault_root)
;;

(* Paths and URIs
   =============== *)

let rel_path_of_uri (t : t) (uri : DocumentUri.t) : string =
  let file_path = DocumentUri.to_path uri in
  match t.workspace_root with
  | None -> file_path
  | Some root ->
    (match String.chop_prefix file_path ~prefix:(root ^ "/") with
     | Some rel -> rel
     | None -> file_path)
;;

let uri_of_rel_path (t : t) (rel_path : string) : DocumentUri.t =
  match t.workspace_root with
  | None -> DocumentUri.of_path rel_path
  | Some root -> DocumentUri.of_path (Filename.concat root rel_path)
;;

let command_path (t : t) path =
  if String.is_empty t.project_prefix then path else Filename.concat t.project_prefix path
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

(** Every line of a command block that names nothing.  A misspelling would
    otherwise be inert, and inert looks exactly like unimplemented. *)
let command_block_diagnostics (content : string) : Diagnostic.t list =
  Command_block.entries content
  |> List.filter_map ~f:(fun (e : Command_block.entry) ->
    match e.command with
    | Some _ -> None
    | None ->
      let line_start = Lsp_util.line_start_byte content ~line:e.line in
      Some
        (Diagnostic.create
           ~range:
             (range_of_bytes
                content
                ~first_byte:line_start
                ~last_byte:(line_start + String.length e.text))
           ~severity:DiagnosticSeverity.Warning
           ~source:"oystermark"
           ~message:
             (`String
                 (sprintf
                    "unknown oysterlsp command %S; expected one of %s"
                    e.text
                    (String.concat
                       ~sep:", "
                       (List.map
                          Command_block.all_of_command
                          ~f:Command_block.name_of_command))))
           ()))
;;

(** A table of contents that no longer describes the document, and a [::: toc]
    fence left unclosed.  Unlike the link diagnostics this needs no vault: a
    TOC describes one file.  See {!page-"feature-toc".diagnostics}. *)
let toc_diagnostics (content : string) : Diagnostic.t list =
  Feature.Toc.diagnostics content
  |> List.map ~f:(fun (d : Feature.Toc.diagnostic) ->
    Diagnostic.create
      ~range:(range_of_bytes content ~first_byte:d.first_byte ~last_byte:d.last_byte)
      ~severity:DiagnosticSeverity.Warning
      ~source:"oystermark"
      ~message:(`String d.message)
      ())
;;

let diagnostics (t : t) ~(rel_path : string) ~(content : string) : Diagnostic.t list =
  match t.vault with
  | None -> []
  | Some v ->
    let links =
      Feature.Diagnostics.compute ~index:v.index ~rel_path ~content ()
      |> List.map ~f:(fun (d : Feature.Diagnostics.diagnostic) ->
        Diagnostic.create
          ~range:(range_of_bytes content ~first_byte:d.first_byte ~last_byte:d.last_byte)
          ~severity:DiagnosticSeverity.Warning
          ~source:"oystermark"
          ~message:(`String d.message)
          ())
    in
    links @ command_block_diagnostics content @ toc_diagnostics content
;;

let did_open_local (t : t) ~(rel_path : string) ~(content : string) : Diagnostic.t list =
  Hashtbl.set t.open_docs ~key:rel_path ~data:content;
  diagnostics t ~rel_path ~content
;;

let did_change_local (t : t) ~(rel_path : string) ~(content : string) : Diagnostic.t list =
  Hashtbl.set t.open_docs ~key:rel_path ~data:content;
  diagnostics t ~rel_path ~content
;;

let did_close_local (t : t) ~(rel_path : string) : unit =
  Hashtbl.remove t.open_docs rel_path
;;

let did_save_local (t : t) ~(rel_path : string) : (string * Diagnostic.t list) list =
  Option.iter t.vault ~f:(fun vault ->
    match read_file t rel_path with
    | None -> t.vault <- Some (Oystermark.Vault.remove_path vault rel_path)
    | Some content ->
      let doc = Oystermark.Parse.of_string ~locs:true content in
      t.vault <- Some (Oystermark.Vault.set_doc vault rel_path doc));
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

let hover_local (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : Hover.t option
  =
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

(* {!definition} answers command-block lines too, so it is defined with the
   other handlers that need {!resolve_command} — below the command-block
   section, alongside {!code_action} and {!completion}. *)

let prepare_rename_local (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
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

let document_symbol_local (t : t) ~(rel_path : string) : DocumentSymbol.t list option =
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

(* Daily notes
   ============

   See {!page-"feature-daily-notes"}. *)

(** The command a daily-note code action carries.  A code action cannot focus
    an editor by itself, so the action defers to this command, which the client
    sends back as [workspace/executeCommand]. *)
let daily_note_command = "oystermark.dailyNote.open"

(** What {!execute_command} decided should happen.  Returning an intent rather
    than performing it keeps the protocol effects — [workspace/applyEdit] and
    [window/showDocument] — in the adapter, and this decision testable. *)
type open_note =
  { uri : DocumentUri.t
  ; create : WorkspaceEdit.t option (** [None] when the note already exists. *)
  ; report_unfocused : bool
    (** Whether a client that cannot focus should be told where the note
            is.  [false] when the caller already gave the client an edit that
            opens it, so the reader has the note in front of them and the
            message would only contradict what they can see. *)
  }

(** Whether a vault-relative path exists on disk.  Read from disk rather than
    the index because a note created moments ago by this very command has not
    been indexed yet. *)
let file_exists (t : t) (rel_path : string) : bool =
  match vault_root t with
  | None -> false
  | Some root -> Stdlib.Sys.file_exists (Filename.concat root rel_path)
;;

(** The recognition table for the configured settings, rebuilt when the day
    turns over.  [Error] when the configured format is unsupported, which
    disables the daily-note actions. *)
let daily_table (t : t) : (Daily_notes.Table.t, string) Result.t =
  match Lsp_config.daily_notes_settings t.config with
  | Error _ as e -> e
  | Ok settings ->
    let today = t.now () in
    (match t.daily_table with
     | Some (built_for, table) when Date.equal built_for today -> Ok table
     | _ ->
       let table = Daily_notes.Table.create settings ~today in
       t.daily_table <- Some (today, table);
       Ok table)
;;

(** The edit that brings a missing note into existence, carried by the code
    action itself rather than deferred to {!daily_note_command}.

    A [CreateFile] on its own leaves a client with nothing to show: the file
    lands on disk and no editor opens, which is why creation used to end in the
    same silence as opening where [window/showDocument] is missing.  A
    [TextDocumentEdit] cannot be applied to a document without opening it, so
    the empty text edit below — it writes nothing, the created note stays empty
    — is what puts the note in front of the reader on such a client.  Focusing
    is still the client's (see {!page-"feature-daily-notes".focus}); this only
    makes creation reach a document by the better-supported half. *)
let create_note_edit (uri : DocumentUri.t) : WorkspaceEdit.t =
  let zero = Position.create ~line:0 ~character:0 in
  WorkspaceEdit.create
    ~documentChanges:
      [ `CreateFile
          (CreateFile.create
             ~uri
             ~options:(CreateFileOptions.create ~ignoreIfExists:true ~overwrite:false ())
             ())
      ; `TextDocumentEdit
          (TextDocumentEdit.create
             ~textDocument:(OptionalVersionedTextDocumentIdentifier.create ~uri ())
             ~edits:
               [ `TextEdit
                   (TextEdit.create
                      ~range:(Range.create ~start:zero ~end_:zero)
                      ~newText:"")
               ])
      ]
    ()
;;

(** One daily-note code action: a title that says whether the note will be
    created, the edit that creates it when it is missing, and the command that
    opens it.

    The command is asked to open only ([create = false]): the edit above has
    already created the note by the time it runs, and a second [CreateFile]
    would race the buffer the client just opened — unsaved, so the file may not
    be on disk yet. *)
let daily_note_action (t : t) ~(title : string) ~(path : string) : CodeAction.t =
  let exists = file_exists t path in
  CodeAction.create
    ~title:(sprintf "%s %s" (if exists then "Open" else "Create") title)
    ~kind:CodeActionKind.Refactor
    ?edit:(if exists then None else Some (create_note_edit (uri_of_rel_path t path)))
    ~command:
      (Command.create
         ~title
         ~command:daily_note_command
         ~arguments:[ `String (command_path t path); `Bool false; `Bool (not exists) ]
         ())
    ()
;;

(** The daily-note actions available at [rel_path].

    The calendar family is always offered and creates on demand; the existing
    family is offered only from a file that is itself a daily note, and only
    when there is somewhere to go.  See {!page-"feature-daily-notes"}. *)
let daily_note_actions (t : t) ~(rel_path : string) : CodeAction.t list =
  match daily_table t with
  | Error _ -> []
  | Ok table ->
    let settings = table.Daily_notes.Table.settings in
    let today = t.now () in
    let calendar =
      [ "today's daily note", today
      ; "yesterday's daily note", Date.add_days today (-1)
      ; "tomorrow's daily note", Date.add_days today 1
      ]
      |> List.map ~f:(fun (title, date) ->
        daily_note_action t ~title ~path:(Daily_notes.path_of_date settings date))
    in
    let existing =
      match Daily_notes.Table.date_of_path table rel_path with
      | None -> []
      | Some date ->
        let exists p = file_exists t p in
        let step title f =
          match f table ~exists date with
          | None -> []
          | Some (_, path) ->
            [ CodeAction.create
                ~title
                ~kind:CodeActionKind.Refactor
                ~command:
                  (Command.create
                     ~title
                     ~command:daily_note_command
                     ~arguments:[ `String (command_path t path); `Bool false ]
                     ())
                ()
            ]
        in
        step "Open previous daily note" Daily_notes.Table.previous_existing
        @ step "Open next daily note" Daily_notes.Table.next_existing
    in
    calendar @ existing
;;

(** Write a wikilink to today's daily note at the cursor, creating the note
    when it is missing.

    Every other action in the family reaches its note through
    [window/showDocument], which the protocol makes optional and not every
    client implements.  This one asks for nothing but a [WorkspaceEdit], and
    leaves behind a link that go-to-definition follows — so the note stays
    reachable wherever that capability is missing.  It is also, independently,
    an ordinary thing to want in a note.

    The link is the note's base name: [Index.resolve] matches a path
    subsequence, so [ [[2026-07-26]] ] finds [2026/07/2026-07-26.md] under a
    nested format without the writer spelling out the folders.  See
    {!page-"feature-daily-notes".link}. *)
let daily_note_link_actions (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : CodeAction.t list
  =
  match daily_table t with
  | Error _ -> []
  (* Unlike the rest of the family this one is offered in every menu in the
     vault, wanted there or not, so it is the one worth being able to turn off
     without disabling daily notes.  See {!page-"feature-daily-notes".link}. *)
  | Ok _ when not t.config.daily_notes.link_action -> []
  | Ok table ->
    let path = Daily_notes.path_of_date table.Daily_notes.Table.settings (t.now ()) in
    let name = Filename.chop_extension (Filename.basename path) in
    let create =
      if file_exists t path
      then []
      else
        [ `CreateFile
            (CreateFile.create
               ~uri:(uri_of_rel_path t path)
               ~options:
                 (CreateFileOptions.create ~ignoreIfExists:true ~overwrite:false ())
               ())
        ]
    in
    let at = Position.create ~line ~character in
    let insert =
      `TextDocumentEdit
        (TextDocumentEdit.create
           ~textDocument:
             (OptionalVersionedTextDocumentIdentifier.create
                ~uri:(uri_of_rel_path t rel_path)
                ())
           ~edits:
             [ `TextEdit
                 (TextEdit.create
                    ~range:(Range.create ~start:at ~end_:at)
                    ~newText:(sprintf "[[%s]]" name))
             ])
    in
    (* Creation first: the link should resolve the moment it is written. *)
    [ CodeAction.create
        ~title:"Insert link to today's daily note"
        ~kind:CodeActionKind.Refactor
        ~edit:(WorkspaceEdit.create ~documentChanges:(create @ [ insert ]) ())
        ()
    ]
;;

(* Command blocks
   ===============

   See {!page-"feature-command-block"}.  Nothing new happens here: a block line
   is another way to reach the daily-note command, lifted from the code-action
   menu onto a line where a lens can render it. *)

(** What a command-block line amounts to right now, in this note.  [target] is
    [None] when the command is understood but cannot run — then [label] says
    why, and the surfaces show it without anything to click. *)
type resolved_command =
  { label : string
  ; target : (string * bool) option (** [(path, create_it)]. *)
  }

let resolve_command (t : t) ~(rel_path : string) (c : Command_block.command)
  : resolved_command
  =
  match daily_table t with
  | Error _ -> { label = "daily notes disabled"; target = None }
  | Ok table ->
    let settings = table.Daily_notes.Table.settings in
    let today = t.now () in
    (* Calendar commands always have a target: a missing note is created. *)
    let calendar (what : string) (date : Date.t) =
      let path = Daily_notes.path_of_date settings date in
      let exists = file_exists t path in
      { label = sprintf "%s %s daily note" (if exists then "Open" else "Create") what
      ; target = Some (path, not exists)
      }
    in
    (* Previous/next answer relative to the {e host} note, which is why the
       block lives in a note.  Neither ever creates. *)
    let step (what : string) f =
      match Daily_notes.Table.date_of_path table rel_path with
      | None ->
        { label = sprintf "no %s daily note: this note is not one" what; target = None }
      | Some date ->
        (match f table ~exists:(fun p -> file_exists t p) date with
         | None -> { label = sprintf "no %s daily note" what; target = None }
         | Some (_, path) ->
           { label = sprintf "Open %s daily note (%s)" what path
           ; target = Some (path, false)
           })
    in
    (match c with
     | Daily_today -> calendar "today's" today
     | Daily_yesterday -> calendar "yesterday's" (Date.add_days today (-1))
     | Daily_tomorrow -> calendar "tomorrow's" (Date.add_days today 1)
     | Daily_prev -> step "previous" Daily_notes.Table.previous_existing
     | Daily_next -> step "next" Daily_notes.Table.next_existing)
;;

(** The lens for one line: a whole-line range, the resolved label, and the
    command when there is one to run. *)
let lens_of_entry
      (t : t)
      ~(rel_path : string)
      ~(content : string)
      (e : Command_block.entry)
  : CodeLens.t option
  =
  match e.command with
  (* An unknown name gets a diagnostic instead; a lens saying nothing useful
     would only compete with it. *)
  | None -> None
  | Some c ->
    let { label; target } = resolve_command t ~rel_path c in
    let line_start = Lsp_util.line_start_byte content ~line:e.line in
    let range =
      range_of_bytes
        content
        ~first_byte:line_start
        ~last_byte:(line_start + String.length e.text)
    in
    Some
      (CodeLens.create
         ~range
         ?command:
           (Option.map target ~f:(fun (path, create) ->
              Command.create
                ~title:label
                ~command:daily_note_command
                ~arguments:[ `String (command_path t path); `Bool create ]
                ()))
         ~data:(`String label)
         ())
;;

(** A reference count as a lens above its line: [3 references] over a heading,
    [12 backlinks] over the note, carrying the references themselves so that
    clicking the lens opens them.  The locations come from the same scan that
    produced the number, so the list can never disagree with the count above
    it.  See {!page-"feature-codelens-reference-counts"}. *)
let reference_lenses (t : t) ~(rel_path : string) ~(content : string) : CodeLens.t list =
  if not t.config.code_lens_references
  then []
  else (
    match t.vault with
    | None -> []
    | Some v ->
      Reference_counts.entries
        ~index:v.index
        ~docs:(Oystermark.Vault.docs v)
        ~rel_path
        ~content
        ~range_start_line:0
        ~range_end_line:Int.max_value
        ~count_toc_links:t.config.code_lens_count_toc_links
      |> List.map ~f:(fun (e : Reference_counts.entry) ->
        let at = Position.create ~line:e.line ~character:0 in
        let locations =
          List.map e.refs ~f:(fun (r : Feature.Find_references.reference) ->
            Location.create
              ~uri:(uri_of_rel_path t r.rel_path)
              ~range:
                (range_of_bytes
                   (disk_content t r.rel_path)
                   ~first_byte:r.first_byte
                   ~last_byte:r.last_byte)
            |> Location.yojson_of_t)
        in
        let title = Reference_counts.lens_title e in
        (* The command name is the client's, not the protocol's: there is no
           standard "show me these locations", only a convention VS Code named
           and others implement.  Configured, therefore — and [""] means the
           lens is a label, which is also what an unaware client makes of a
           name it does not know.  A [Command] is still required: a lens shows
           its title through one.  See
           {!page-"feature-codelens-reference-counts".click}. *)
        let command = t.config.code_lens_show_references_command in
        CodeLens.create
          ~range:(Range.create ~start:at ~end_:at)
          ~command:
            (Command.create
               ~title
               ~command
               ?arguments:
                 (if String.is_empty command
                  then None
                  else
                    Some
                      [ DocumentUri.yojson_of_t (uri_of_rel_path t rel_path)
                      ; Position.yojson_of_t at
                      ; `List locations
                      ])
               ())
          ()))
;;

(** Spec: {!page-"feature-command-block".surfaces} for the command-block
    lenses, {!page-"feature-codelens-reference-counts"} for the counts.  [None]
    outside a vault, so a client learns nothing is coming rather than seeing an
    empty list.

    Command lenses come first: they are the note's control panel, and a count
    is an annotation that should not push it down the response. *)
let code_lens_local ?(include_references = true) (t : t) ~(rel_path : string)
  : CodeLens.t list option
  =
  match t.vault with
  | None -> None
  | Some _ ->
    let content = buffer_content t rel_path in
    ((Command_block.entries content
      |> List.filter_map ~f:(lens_of_entry t ~rel_path ~content))
     @ if include_references then reference_lenses t ~rel_path ~content else [])
    |> Option.return
;;

(** The single action for the command-block line under the cursor, if that is
    where the cursor is.  The keyboard route to what the lens offers. *)
let command_block_actions (t : t) ~(rel_path : string) ~(line : int) : CodeAction.t list =
  let content = buffer_content t rel_path in
  match Command_block.entry_at content ~line with
  | None -> []
  | Some { command = None; _ } -> []
  | Some { command = Some c; _ } ->
    (match resolve_command t ~rel_path c with
     | { target = None; _ } -> []
     | { label; target = Some (path, create) } ->
       (* Same bargain as {!daily_note_action}: an action can carry the edit,
          so creation reaches a document even where the command cannot focus
          one.  The lens beside it has only the command — a [CodeLens] carries
          nothing else — and stays as it was. *)
       [ CodeAction.create
           ~title:label
           ~kind:CodeActionKind.Refactor
           ~isPreferred:true
           ?edit:
             (if create then Some (create_note_edit (uri_of_rel_path t path)) else None)
           ~command:
             (Command.create
                ~title:label
                ~command:daily_note_command
                ~arguments:[ `String (command_path t path); `Bool false; `Bool create ]
                ())
           ()
       ])
;;

(** The action that seeds a note with a panel of its own, at the cursor's line.

    Offered only where there is no block yet, and listing only the commands
    that can run in {e this} note — an ordinary note gets the calendar three,
    a daily note with neighbours gets all five.  Inserting the whole catalogue
    everywhere would hand most notes two lines the lenses immediately report as
    dead.  See {!page-"feature-command-block".insert}. *)
let insert_command_block_actions (t : t) ~(rel_path : string) ~(line : int)
  : CodeAction.t list
  =
  let content = buffer_content t rel_path in
  if Command_block.has_command_block content
  then []
  else (
    match
      List.filter Command_block.all_of_command ~f:(fun c ->
        Option.is_some (resolve_command t ~rel_path c).target)
    with
    (* Nothing runs here — daily notes are off — so there is no panel worth
       writing. *)
    | [] -> []
    | commands ->
      let at = Position.create ~line ~character:0 in
      [ CodeAction.create
          ~title:"Insert oysterlsp command block"
          ~kind:CodeActionKind.Refactor
          ~edit:
            (WorkspaceEdit.create
               ~documentChanges:
                 [ `TextDocumentEdit
                     (TextDocumentEdit.create
                        ~textDocument:
                          (OptionalVersionedTextDocumentIdentifier.create
                             ~uri:(uri_of_rel_path t rel_path)
                             ())
                        ~edits:
                          [ `TextEdit
                              (TextEdit.create
                                 ~range:(Range.create ~start:at ~end_:at)
                                 ~newText:(Command_block.render commands))
                          ])
                 ]
               ())
          ()
      ])
;;

(** The catalogue, offered inside a command block and nowhere else. *)
let command_block_completions (t : t) ~(rel_path : string) ~(line : int)
  : CompletionItem.t list option
  =
  let content = buffer_content t rel_path in
  if not (Command_block.in_command_block content ~line)
  then None
  else
    List.map Command_block.all_of_command ~f:(fun c ->
      let name = Command_block.name_of_command c in
      CompletionItem.create
        ~label:name
        ~kind:CompletionItemKind.Event
        ~detail:(Command_block.doc_of_command c)
          (* What it would do here, today — the same text the lens would show. *)
        ~documentation:(`String (resolve_command t ~rel_path c).label)
        ())
    |> Option.return
;;

(** The note a command-block line {e points at}, when that note is already
    there.  The command's own target, read rather than run.

    A calendar command whose note is missing yields [None]: its lens says
    [Create ...], and go-to-definition may not create anything — a jump that
    wrote a file would be a surprising answer to a navigation request.
    [daily/prev] and [daily/next] never create, so they point wherever they
    would open.  See {!page-"feature-command-block".surfaces}. *)
let command_block_definition (t : t) ~(rel_path : string) ~(line : int) : string option =
  let content = buffer_content t rel_path in
  match Command_block.entry_at content ~line with
  | None | Some { command = None; _ } -> None
  | Some { command = Some c; _ } ->
    (match (resolve_command t ~rel_path c).target with
     | None -> None
     (* [create] is exactly "the note is not there yet". *)
     | Some (_, true) -> None
     | Some (path, false) -> Some path)
;;

(** Spec: {!page-"feature-go-to-definition"}.  A command-block line is tried
    first: it is literal text inside a fence, so the link layer finds nothing
    there, and the note the line names is what a definition means on it.  See
    {!page-"feature-command-block".surfaces}. *)
let definition_local (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : Location.t list option
  =
  match t.vault with
  | None -> None
  | Some v ->
    let location ~path ~line ~character =
      let pos = Position.create ~line ~character in
      [ Location.create
          ~uri:(uri_of_rel_path t path)
          ~range:(Range.create ~start:pos ~end_:pos)
      ]
    in
    (match command_block_definition t ~rel_path ~line with
     | Some path -> Some (location ~path ~line:0 ~character:0)
     | None ->
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
        | Some { path; line; character } -> Some (location ~path ~line ~character)))
;;

(** Handle [workspace/executeCommand].  [None] for an unknown command or
    unusable arguments — an editor is free to send either. *)
let execute_command_local
      (t : t)
      ~(command : string)
      ~(arguments : Yojson.Safe.t list option)
  : open_note option
  =
  match command, arguments with
  (* Without a vault root there is no path to resolve the argument against —
     and nothing offered the command in the first place, disabled or not yet
     initialized. *)
  | _ when Option.is_none t.vault -> None
  | cmd, Some (`String path :: `Bool create :: rest)
    when String.equal cmd daily_note_command
         &&
         match rest with
         | [] | [ `Bool _ ] -> true
         | _ -> false ->
    let uri = uri_of_rel_path t path in
    (* The optional third argument says the caller already handed the client an
       edit that opens this note (see {!create_note_edit}).  Then a failure to
       focus is not worth a message: the reader is looking at the note. *)
    let report_unfocused =
      match rest with
      | [ `Bool opened_by_edit ] -> not opened_by_edit
      | _ -> true
    in
    let create =
      if create && not (file_exists t path)
      then
        Some
          (WorkspaceEdit.create
             ~documentChanges:
               [ `CreateFile
                   (CreateFile.create
                      ~uri
                      ~options:
                        (CreateFileOptions.create
                           ~ignoreIfExists:true
                           ~overwrite:false
                           ())
                      ())
               ]
             ())
      else None
    in
    Some { uri; create; report_unfocused }
  | _ -> None
;;

(* Table of contents
   ==================

   See {!page-"feature-toc"}.  Both actions read {!buffer_content} rather than
   disk: a TOC describes headings that were just typed, and an edit derived
   from a different version of the file would be applied to this one.  See
   {!page-"feature-toc".frame}. *)

let toc_workspace_edit
      (t : t)
      ~(rel_path : string)
      ~(content : string)
      (e : Feature.Toc.edit)
  : WorkspaceEdit.t
  =
  WorkspaceEdit.create
    ~documentChanges:
      [ `TextDocumentEdit
          (TextDocumentEdit.create
             ~textDocument:
               (OptionalVersionedTextDocumentIdentifier.create
                  ~uri:(uri_of_rel_path t rel_path)
                  ())
             ~edits:
               [ `TextEdit
                   (TextEdit.create
                      ~range:
                        (range_of_bytes
                           content
                           ~first_byte:e.first_byte
                           ~last_byte:e.last_byte)
                      ~newText:e.new_text)
               ])
      ]
    ()
;;

(** Offered anywhere outside a region — including in a note that already has
    one elsewhere, and in a note with no headings, which inserts the empty
    region a growing note fills in. *)
let toc_insert_actions (t : t) ~(rel_path : string) ~(line : int) : CodeAction.t list =
  let content = buffer_content t rel_path in
  match Feature.Toc.insertion content ~line with
  | None -> []
  | Some edit ->
    [ CodeAction.create
        ~title:"Insert table of contents"
        ~kind:CodeActionKind.Refactor
        ~edit:(toc_workspace_edit t ~rel_path ~content edit)
        ()
    ]
;;

(** The quick fix for the staleness diagnostic.  A region that is already up
    to date offers nothing: there is no edit to make. *)
let toc_update_actions
      (t : t)
      ~(rel_path : string)
      ~(start_line : int)
      ~(start_character : int)
      ~(end_line : int)
      ~(end_character : int)
  : CodeAction.t list
  =
  let content = buffer_content t rel_path in
  match
    Feature.Toc.update
      content
      ~first_byte:(byte_of_position content ~line:start_line ~character:start_character)
      ~last_byte:(byte_of_position content ~line:end_line ~character:end_character)
  with
  | None -> []
  | Some edit ->
    [ CodeAction.create
        ~title:"Update table of contents"
        ~kind:CodeActionKind.QuickFix
        ~isPreferred:true
        ~edit:(toc_workspace_edit t ~rel_path ~content edit)
        ()
    ]
;;

let code_action_local
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
  (* The daily-note actions below are the one family that needs no vault, only
     a clock — so unlike its neighbours this handler has to refuse the
     rootless case itself, or a disabled server would still fill the menu. *)
  if Option.is_none t.vault
  then []
  else (
    let requested (kind : CodeActionKind.t) =
      match only with
      | None -> true
      | Some kinds -> List.mem kinds kind ~equal:Poly.equal
    in
    let daily =
      if requested CodeActionKind.Refactor
      then (
        (* On a command-block line the panel's own action is the answer, and the
         whole daily-note menu would bury it.  See
         {!page-"feature-command-block".surfaces}. *)
        match command_block_actions t ~rel_path ~line:start_line with
        | [] ->
          daily_note_actions t ~rel_path
          @ daily_note_link_actions
              t
              ~rel_path
              ~line:start_line
              ~character:start_character
          @ insert_command_block_actions t ~rel_path ~line:start_line
          @ toc_insert_actions t ~rel_path ~line:start_line
        | actions -> actions)
      else []
    in
    let toc_fixes =
      if requested CodeActionKind.QuickFix
      then
        toc_update_actions
          t
          ~rel_path
          ~start_line
          ~start_character
          ~end_line
          ~end_character
      else []
    in
    let quick_fixes =
      match t.vault with
      | _ when not (requested CodeActionKind.QuickFix) -> []
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
    in
    quick_fixes @ toc_fixes @ daily)
;;

let completion_local (t : t) ~(rel_path : string) ~(line : int) ~(character : int)
  : CompletionList.t option
  =
  match t.vault with
  | None -> None
  | Some v ->
    (* Inside a command block the note-name and fragment completions make no
     sense: the content there is a command name.  See
     {!page-"feature-command-block".surfaces}. *)
    (match command_block_completions t ~rel_path ~line with
     | Some items -> Some (CompletionList.create ~isIncomplete:false ~items ())
     | None ->
       let content = buffer_content t rel_path in
       let { Feature.Completion.items; replace_from; incomplete } =
         Feature.Completion.complete ~index:v.index ~rel_path ~content ~line ~character ()
       in
       (* One range for the whole response: every item replaces the prefix the
          user has typed.  Left to [insertText], a client picks the replaced
          span by its own word rules, and VS Code's exclude [/] — which is how
          accepting [notes/deep.md] after typing [notes/] yields
          [notes/notes/deep.md].  See
          {!page-"feature-completion-markdown-links".replace_range}. *)
       let range =
         let start_line, start_character =
           Lsp_util.position_of_byte_offset content replace_from
         in
         Range.create
           ~start:(Position.create ~line:start_line ~character:start_character)
           ~end_:(Position.create ~line ~character)
       in
       List.map items ~f:(fun (i : Feature.Completion.item) ->
         let kind =
           match i.kind with
           | Feature.Completion.File -> CompletionItemKind.File
           | Feature.Completion.Reference -> CompletionItemKind.Reference
         in
         let new_text = Option.value i.insert_text ~default:i.label in
         CompletionItem.create
           ~label:i.label
           ?detail:i.detail
           ?filterText:i.filter_text
           ~textEdit:(`TextEdit (TextEdit.create ~range ~newText:new_text))
           ~kind
           ())
       |> fun items -> Some (CompletionList.create ~isIncomplete:incomplete ~items ()))
;;

let inlay_hint_local (t : t) ~(rel_path : string) ~(start_line : int) ~(end_line : int)
  : InlayHint.t list option
  =
  match t.vault with
  | None -> None
  | Some v ->
    Feature.Inlay_hints.inlay_hints
      ~config:t.config
      ~index:v.index
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

(* Project routing
   =============== *)

let route (t : t) (rel_path : string) : t * string =
  List.filter t.nested_projects ~f:(fun (prefix, _) ->
    path_is_within ~root:prefix rel_path)
  |> List.max_elt ~compare:(fun (a, _) (b, _) ->
    Int.compare (String.length a) (String.length b))
  |> Option.value_map ~default:(t, rel_path) ~f:(fun (prefix, project) ->
    let local =
      if String.equal rel_path prefix
      then ""
      else String.chop_prefix_exn rel_path ~prefix:(prefix ^ "/")
    in
    project, local)
;;

let projects (t : t) = t :: List.map t.nested_projects ~f:snd

let absolute_path (project : t) rel_path =
  Option.value_map project.workspace_root ~default:rel_path ~f:(fun root ->
    Filename.concat root rel_path)
;;

let local_path (project : t) absolute =
  Option.bind project.workspace_root ~f:(fun root ->
    if String.equal absolute root
    then Some ""
    else String.chop_prefix absolute ~prefix:(root ^ "/"))
;;

let vault_contains (project : t) path =
  match project.vault with
  | None -> false
  | Some vault ->
    Option.is_some (Oystermark.Vault.Index.find_note vault.index path)
    || Option.is_some (Oystermark.Vault.Index.find_asset vault.index path)
;;

let target_path (target : Feature.Find_references.target) = target.path
let target_with_path (target : Feature.Find_references.target) path = { target with path }

let target_in_project ~source_project target project =
  let absolute = absolute_path source_project (target_path target) in
  local_path project absolute
  |> Option.filter ~f:(vault_contains project)
  |> Option.map ~f:(target_with_path target)
;;

let collect_project_references t ~source_project target =
  projects t
  |> List.concat_map ~f:(fun project ->
    match project.vault, target_in_project ~source_project target project with
    | Some vault, Some target ->
      Feature.Find_references.scan_vault
        ~index:vault.index
        ~docs:(Oystermark.Vault.docs vault)
        target
      |> List.map ~f:(fun reference -> project, reference)
    | _ -> [])
  |> List.dedup_and_sort ~compare:(fun (pa, a) (pb, b) ->
    [%compare: string * int * int]
      (absolute_path pa a.rel_path, a.first_byte, a.last_byte)
      (absolute_path pb b.rel_path, b.first_byte, b.last_byte))
;;

let global_reference_lenses t project ~rel_path ~content =
  if not project.config.code_lens_references
  then []
  else (
    let targets =
      ( 0
      , Reference_counts.File
      , ({ path = rel_path; address = None } : Feature.Find_references.target) )
      :: (Reference_counts.headings_in_range
            ~content
            ~range_start_line:0
            ~range_end_line:Int.max_value
          |> List.map ~f:(fun (line, _, slug) ->
            ( line
            , Reference_counts.Heading { slug }
            , ({ path = rel_path; address = Some (Heading slug) }
               : Feature.Find_references.target) )))
    in
    List.filter_map targets ~f:(fun (line, kind, target) ->
      let refs =
        collect_project_references t ~source_project:project target
        |> List.filter ~f:(fun (_, reference) ->
          project.config.code_lens_count_toc_links || not reference.in_toc)
      in
      match refs with
      | [] -> None
      | refs ->
        let target_path = absolute_path project rel_path in
        let fake_refs =
          List.map refs ~f:(fun (owner, reference) ->
            { reference with rel_path = absolute_path owner reference.rel_path })
        in
        let entry : Reference_counts.entry =
          { line
          ; end_character = 0
          ; rel_path = target_path
          ; refs = fake_refs
          ; target = kind
          }
        in
        let at = Position.create ~line ~character:0 in
        let locations =
          List.map refs ~f:(fun (owner, reference) ->
            let path = absolute_path owner reference.rel_path in
            Location.create
              ~uri:(DocumentUri.of_path path)
              ~range:
                (range_of_bytes
                   (In_channel.read_all path)
                   ~first_byte:reference.first_byte
                   ~last_byte:reference.last_byte)
            |> Location.yojson_of_t)
        in
        let command = project.config.code_lens_show_references_command in
        Some
          (CodeLens.create
             ~range:(Range.create ~start:at ~end_:at)
             ~command:
               (Command.create
                  ~title:(Reference_counts.lens_title entry)
                  ~command
                  ?arguments:
                    (if String.is_empty command
                     then None
                     else
                       Some
                         [ DocumentUri.yojson_of_t (DocumentUri.of_path target_path)
                         ; Position.yojson_of_t at
                         ; `List locations
                         ])
                  ())
             ())))
;;

let did_open t ~rel_path ~content =
  let project, rel_path = route t rel_path in
  did_open_local project ~rel_path ~content
;;

let did_change t ~rel_path ~content =
  let project, rel_path = route t rel_path in
  did_change_local project ~rel_path ~content
;;

let did_close t ~rel_path =
  let project, rel_path = route t rel_path in
  did_close_local project ~rel_path
;;

let did_save t ~rel_path =
  let source_project, source_local = route t rel_path in
  let absolute = absolute_path source_project source_local in
  projects t
  |> List.concat_map ~f:(fun project ->
    match local_path project absolute with
    | None -> []
    | Some local ->
      let included =
        deepest_root project.nested_roots local
        |> Option.value_map ~default:true ~f:(fun owner ->
          List.mem project.imported_roots owner ~equal:String.equal)
      in
      if not included
      then []
      else
        did_save_local project ~rel_path:local
        |> List.map ~f:(fun (path, diagnostics) ->
          absolute_path project path, diagnostics))
  |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  |> List.map ~f:(fun (absolute, diagnostics) ->
    let rel_path = Option.value (local_path t absolute) ~default:absolute in
    rel_path, diagnostics)
;;

let hover t ~rel_path ~line ~character =
  let project, rel_path = route t rel_path in
  hover_local project ~rel_path ~line ~character
;;

let references t ~rel_path ~line ~character =
  let source_project, source_rel_path = route t rel_path in
  match source_project.vault with
  | None -> None
  | Some source_vault ->
    (match
       Feature.Find_references.detect_target
         ~index:source_vault.index
         ~rel_path:source_rel_path
         ~content:(disk_content source_project source_rel_path)
         ~line
         ~character
     with
     | None -> Some []
     | Some target ->
       collect_project_references t ~source_project target
       |> List.map ~f:(fun (project, reference) ->
         let path = absolute_path project reference.rel_path in
         let content = In_channel.read_all path in
         Location.create
           ~uri:(DocumentUri.of_path path)
           ~range:
             (range_of_bytes
                content
                ~first_byte:reference.first_byte
                ~last_byte:reference.last_byte))
       |> Option.return)
;;

let prepare_rename t ~rel_path ~line ~character =
  let project, rel_path = route t rel_path in
  prepare_rename_local project ~rel_path ~line ~character
;;

(** Per-project edits as one [TextDocumentEdit] per file, ordered by path. *)
let text_document_edits (edits : (t * Feature.Rename.edit) list) =
  List.dedup_and_sort edits ~compare:(fun (pa, a) (pb, b) ->
    [%compare: string * int * int * string]
      (absolute_path pa a.rel_path, a.first_byte, a.last_byte, a.new_text)
      (absolute_path pb b.rel_path, b.first_byte, b.last_byte, b.new_text))
  |> List.group ~break:(fun (pa, a) (pb, b) ->
    not (String.equal (absolute_path pa a.rel_path) (absolute_path pb b.rel_path)))
  |> List.filter_map ~f:(function
    | [] -> None
    | (project, first) :: _ as edits ->
      let path = absolute_path project first.rel_path in
      let content = In_channel.read_all path in
      let textDocument =
        OptionalVersionedTextDocumentIdentifier.create ~uri:(DocumentUri.of_path path) ()
      in
      let edits =
        List.map edits ~f:(fun (_, edit) ->
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
;;

let rename t ~rel_path ~line ~character ~new_name =
  let source_project, source_rel_path = route t rel_path in
  match source_project.vault with
  | None -> WorkspaceEdit.create ()
  | Some source_vault ->
    let content = disk_content source_project source_rel_path in
    (match
       Feature.Find_references.detect_target
         ~index:source_vault.index
         ~rel_path:source_rel_path
         ~content
         ~line
         ~character
     with
     | None -> WorkspaceEdit.create ()
     | Some target ->
       let edits =
         projects t
         |> List.concat_map ~f:(fun project ->
           match project.vault, target_in_project ~source_project target project with
           | Some vault, Some target ->
             (match
                Feature.Rename.plan_target
                  ~index:vault.index
                  ~docs:(Oystermark.Vault.docs vault)
                  ~read_file:(read_file project)
                  target
                  ~new_name
              with
              | Error _ -> []
              | Ok change -> List.map change.edits ~f:(fun edit -> project, edit))
           | _ -> [])
       in
       let text_document_edits = text_document_edits edits in
       let documentChanges =
         match target with
         | { path; address = None } when Feature.Rename.valid_note_name new_name ->
           let absolute = absolute_path source_project path in
           let owner, owner_path =
             match t.workspace_root with
             | None -> source_project, path
             | Some root ->
               let workspace_path =
                 String.chop_prefix absolute ~prefix:(root ^ "/")
                 |> Option.value ~default:absolute
               in
               route t workspace_path
           in
           let new_path = Feature.Rename.renamed_note_path ~path:owner_path ~new_name in
           text_document_edits
           @ [ `RenameFile
                 (RenameFile.create
                    ~oldUri:(DocumentUri.of_path absolute)
                    ~newUri:(DocumentUri.of_path (absolute_path owner new_path))
                    ())
             ]
         | _ -> text_document_edits
       in
       WorkspaceEdit.create ~documentChanges ())
;;

(* File operations
   =============== *)

(** The workspace-relative [renames] that move something [project] indexes to
    another place inside it, as project-local paths. *)
let project_moves t project renames =
  match project.vault with
  | None -> []
  | Some vault ->
    let indexed =
      List.map
        (Oystermark.Vault.Index.notes vault.index)
        ~f:Oystermark.Vault.Index.Entry.path
      @ List.map
          (Oystermark.Vault.Index.assets vault.index)
          ~f:Oystermark.Vault.Index.Asset.path
    in
    List.filter_map renames ~f:(fun (src, dst) ->
      match
        local_path project (absolute_path t src), local_path project (absolute_path t dst)
      with
      | Some src, Some dst when List.exists indexed ~f:(path_is_within ~root:src) ->
        Some (src, dst)
      | _ -> None)
;;

let will_rename_files t ~renames =
  let edits =
    projects t
    |> List.concat_map ~f:(fun project ->
      match project.vault, project_moves t project renames with
      | None, _ | _, [] -> []
      | Some vault, moves ->
        (match
           Oystermark.Vault.Rename.plan_moves
             ~index:vault.index
             ~read_file:(read_file project)
             moves
         with
         | Error _ -> []
         | Ok change -> List.map change.edits ~f:(fun edit -> project, edit)))
  in
  WorkspaceEdit.create ~documentChanges:(text_document_edits edits) ()
;;

let rescan (project : t) =
  match project.vault, project.workspace_root with
  | Some _, Some root ->
    project.vault
    <- Some
         (build_vault
            ~nested_roots:project.nested_roots
            ~imported_roots:project.imported_roots
            ~exclude:project.config.exclude
            root)
  | _ -> ()
;;

let did_rename_files t ~renames =
  (* A rename within one project moves index entries in place; one that
     crosses a project boundary changes which vaults hold the files, which only
     a rescan of the projects involved works out. *)
  let within_one_project (src, dst) =
    phys_equal (fst (route t src)) (fst (route t dst))
  in
  List.iter (projects t) ~f:(fun project ->
    let local path = local_path project (absolute_path t path) in
    let involved =
      List.filter renames ~f:(fun (src, dst) ->
        Option.is_some (local src) || Option.is_some (local dst))
    in
    if not (List.for_all involved ~f:within_one_project)
    then rescan project
    else (
      match
        ( project.vault
        , List.filter_map involved ~f:(fun (src, dst) ->
            Option.both (local src) (local dst)) )
      with
      | None, _ | _, [] -> ()
      | Some vault, moves ->
        project.vault
        <- Some
             (Oystermark.Vault.map_paths
                vault
                ~f:(Oystermark.Vault.Rename.moved_path moves))))
;;

let document_symbol t ~rel_path =
  let project, rel_path = route t rel_path in
  document_symbol_local project ~rel_path
;;

let code_lens t ~rel_path =
  let project, rel_path = route t rel_path in
  match code_lens_local ~include_references:false project ~rel_path with
  | None -> None
  | Some command_lenses ->
    let content = buffer_content project rel_path in
    Some (command_lenses @ global_reference_lenses t project ~rel_path ~content)
;;

let definition t ~rel_path ~line ~character =
  let project, rel_path = route t rel_path in
  definition_local project ~rel_path ~line ~character
;;

let execute_command t ~command ~arguments =
  match arguments with
  | Some (`String path :: rest) ->
    let project, local_path = route t path in
    execute_command_local project ~command ~arguments:(Some (`String local_path :: rest))
  | _ -> execute_command_local t ~command ~arguments
;;

let code_action t ?only ~rel_path ~start_line ~start_character ~end_line ~end_character ()
  =
  let project, rel_path = route t rel_path in
  code_action_local
    project
    ?only
    ~rel_path
    ~start_line
    ~start_character
    ~end_line
    ~end_character
    ()
;;

let completion t ~rel_path ~line ~character =
  let project, rel_path = route t rel_path in
  completion_local project ~rel_path ~line ~character
;;

let inlay_hint t ~rel_path ~start_line ~end_line =
  let project, rel_path = route t rel_path in
  inlay_hint_local project ~rel_path ~start_line ~end_line
;;
