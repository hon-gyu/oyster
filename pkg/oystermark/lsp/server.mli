(** Protocol-facing server core: vault state, document synchronization, and
    one LSP-typed handler per feature.

    Handlers take vault-relative paths and 0-based UTF-16 positions, mirroring
    the pure logic layer; conversion from a [DocumentUri.t] happens at the edge
    via {!rel_path_of_uri}.  Every handler answers [None] before {!initialize}
    has supplied a vault root.

    Handlers split into a buffer-reading group and a disk-reading group.  The
    incoming position, however, is always a {e buffer} coordinate, so the
    disk-reading handlers index a live position into content that may be one
    save behind.  For {!rename} and {!code_action} — the two that return edits
    a client applies back to the buffer — that is unsound, not merely stale.
    See {!page-"feature-document-sync".mixed_frame}.

    Spec: {!page-"feature-document-sync"}. *)

open Linol_lsp.Lsp.Types

type t

(** [create ?now ()] makes a server whose daily-note answers are dated by
    [now], defaulting to the system clock.  Substituting it is what lets a test
    of {!page-"feature-daily-notes"} mean the same thing tomorrow. *)
val create : ?now:(unit -> Core.Date.t) -> unit -> t

(** Discover the workspace-root and nested projects below [root], build each
    project's owned tree plus its configured imported views, and adopt each
    project's configuration. Client
    [initializationOptions] are overridden by the [oysterlsp.json] at that
    project root. See {!page-"feature-projects"} and
    {!page-"feature-configuration"}.

    A configuration with ["disable": true] adopts no vault: the server holds
    the settings, {!disabled} becomes true, and every handler goes on
    answering as it does before a root is known.  See
    {!page-"feature-configuration".disable}. *)
val initialize : t -> root:string -> ?init_options:Yojson.Safe.t -> unit -> unit

(** Whether every discovered project is disabled — [false] before
    {!initialize}. The adapter reports it once only when the whole server
    answers nothing. See {!page-"feature-configuration".disable}. *)
val disabled : t -> bool

(** [config_warnings t] is everything the configuration sources asked for and
    could not have: bad values, unknown keys, an unreadable [oysterlsp.json], a
    rejected daily-note format.  Empty when the configuration is clean.
    Computed by {!initialize}; the adapter reports each one, since a setting
    ignored in silence is indistinguishable from one that does not exist.
    See {!page-"feature-configuration".tolerance}. *)
val config_warnings : t -> string list

(** The vault root, or [None] before {!initialize}. *)
val vault_root : t -> string option

(** Strip the workspace root from [uri]'s path. Falls back to the absolute
    path for files outside the workspace (and before {!initialize}). *)
val rel_path_of_uri : t -> DocumentUri.t -> string

(** {1 Daily notes}

    See {!page-"feature-daily-notes"}.  The code actions carry
    {!daily_note_command}; running it yields an {!open_note} describing what the
    adapter should ask the client to do. *)

(** The command name advertised in [executeCommandProvider]. *)
val daily_note_command : string

(** What running {!daily_note_command} decided: the note to focus, the edit that
    creates it first when it does not exist, and whether a client that cannot
    focus should be told where the note is — [false] when the calling action
    already carried an edit that opens it. *)
type open_note =
  { uri : DocumentUri.t
  ; create : WorkspaceEdit.t option
  ; report_unfocused : bool
  }

(** Handle [workspace/executeCommand].  [None] for an unknown command or
    unusable arguments. *)
val execute_command
  :  t
  -> command:string
  -> arguments:Yojson.Safe.t list option
  -> open_note option

val uri_of_rel_path : t -> string -> DocumentUri.t

(** {1 Command blocks}

    See {!page-"feature-command-block"}.  A fenced [oysterlsp] block lifts the
    commands above onto lines, where a lens can render them; the other surfaces
    it adds — one code action on the cursor's line, an action that seeds a note
    with a block of its own, completion of the command names, a diagnostic on
    an unknown one, a jump from a line to the note it names — are folded into
    {!code_action}, {!completion}, {!definition} and the diagnostics the sync
    handlers return. *)

(** Spec: {!page-"feature-command-block".surfaces}.  One lens per recognized
    line; a line whose command cannot run right now keeps its lens, without a
    command attached, so that {i why} stays visible. *)
val code_lens : t -> rel_path:string -> CodeLens.t list option

(** {1 Document synchronization}

    See {!page-"feature-document-sync"} for the full state machine.  Each
    notification handler returns the diagnostics the caller should publish. *)

(** Track the buffer and return diagnostics for the opened document. *)
val did_open : t -> rel_path:string -> content:string -> Diagnostic.t list

(** Recompute diagnostics against the in-flight buffer so squigglies update as
    the user types.  The vault is {i not} rebuilt. *)
val did_change : t -> rel_path:string -> content:string -> Diagnostic.t list

(** Drop [rel_path] from the set of open documents; it stops being refreshed
    by {!did_save}. *)
val did_close : t -> rel_path:string -> unit

(** Replace the saved document's index entry, then recompute diagnostics for
    {i every} open document against its disk content.  This is the moment a stale warning in a
    sibling buffer clears — an unresolved [[[b]]] link in an already-open
    [a.md] loses its squiggly once [b.md] exists and a save fires.

    Returns [(rel_path, diagnostics)] sorted by path, so callers (and expect
    tests) see a stable order.  Documents that cannot be read are omitted. *)
val did_save : t -> rel_path:string -> (string * Diagnostic.t list) list

(** {1 Features} *)

(** Spec: {!page-"feature-hover"}. *)
val hover : t -> rel_path:string -> line:int -> character:int -> Hover.t option

(** Spec: {!page-"feature-go-to-definition"}.  At most one location.

    On a command-block line the location is the note that line's command would
    open, when it already exists — see
    {!page-"feature-command-block".surfaces}. *)
val definition
  :  t
  -> rel_path:string
  -> line:int
  -> character:int
  -> Location.t list option

(** Spec: {!page-"feature-find-references"}. *)
val references
  :  t
  -> rel_path:string
  -> line:int
  -> character:int
  -> Location.t list option

(** Spec: {!page-"feature-rename"}.  [Some] only when the cursor sits on
    something renameable, which is what makes the client offer the prompt. *)
val prepare_rename : t -> rel_path:string -> line:int -> character:int -> Range.t option

(** Spec: {!page-"feature-rename"}.  Text edits are grouped per file; renaming
    a whole note additionally emits a [RenameFile] operation so the note moves
    along with its links.

    Ranges are computed against disk content — unsound if [rel_path] has
    unsaved edits.  See {!page-"feature-document-sync".mixed_frame}. *)
val rename
  :  t
  -> rel_path:string
  -> line:int
  -> character:int
  -> new_name:string
  -> WorkspaceEdit.t

(** Spec: {!page-"feature-rename".file_operations}.  The link edits a client
    applies before it moves files or directories, given as workspace-relative
    [(old, new)] paths.  The edits address the old paths. *)
val will_rename_files : t -> renames:(string * string) list -> WorkspaceEdit.t

(** Spec: {!page-"feature-rename".file_operations}.  Move the index entries of
    files or directories the client has moved, given as workspace-relative
    [(old, new)] paths. *)
val did_rename_files : t -> renames:(string * string) list -> unit

(** Spec: {!page-"feature-document-outline"}. *)
val document_symbol : t -> rel_path:string -> DocumentSymbol.t list option

(** Code actions from several specs, filtered by [only], the client's
    requested code-action-kind filter:

    - {!page-"feature-codeaction-create-unresolved-link"} — a [QuickFix] that
      creates the missing note and seeds it with a title heading, in one
      workspace edit so the client applies both atomically;
    - {!page-"feature-toc"} — a [QuickFix] that rewrites a stale table of
      contents, and a [Refactor] that inserts one at the cursor;
    - {!page-"feature-daily-notes"} and {!page-"feature-command-block"} —
      [Refactor] actions that open, create or link a note.

    The requested range is resolved against disk content — unsound if
    [rel_path] has unsaved edits — except for the table-of-contents actions,
    which read the buffer.  See {!page-"feature-document-sync".mixed_frame}
    and {!page-"feature-toc".frame}. *)
val code_action
  :  t
  -> ?only:CodeActionKind.t list
  -> rel_path:string
  -> start_line:int
  -> start_character:int
  -> end_line:int
  -> end_character:int
  -> unit
  -> CodeAction.t list

(** Spec: {!page-"feature-completion"} and
    {!page-"feature-completion-markdown-links"}.  Items carry a [textEdit]
    rather than an [insertText]: the range they replace is the prefix already
    typed, which a client may not guess.  See
    {!page-"feature-completion-markdown-links".replace_range}. *)
val completion
  :  t
  -> rel_path:string
  -> line:int
  -> character:int
  -> CompletionList.t option

(** Spec: {!page-"feature-inlay-hints-link-direction"}.  [start_line] and
    [end_line] are the requested LSP range's lines, treated as inclusive of
    [end_line]. *)
val inlay_hint
  :  t
  -> rel_path:string
  -> start_line:int
  -> end_line:int
  -> InlayHint.t list option
