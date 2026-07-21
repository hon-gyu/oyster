(** Protocol-facing server core: vault state, document synchronization, and
    one LSP-typed handler per feature.

    Handlers take vault-relative paths and 0-based UTF-16 positions, mirroring
    the pure logic layer; conversion from a [DocumentUri.t] happens at the edge
    via {!rel_path_of_uri}.  Every handler answers [None] before {!initialize}
    has supplied a vault root.

    Spec: {!page-"feature-document-sync"}. *)

open Linol_lsp.Lsp.Types

type t

val create : unit -> t

(** Build the vault from [root].  Called from [initialize] with the client's
    [rootUri]. *)
val initialize : t -> root:string -> unit

(** The vault root, or [None] before {!initialize}. *)
val vault_root : t -> string option

(** Strip the vault root from [uri]'s path.  Falls back to the absolute path
    for files outside the vault (and before {!initialize}). *)
val rel_path_of_uri : t -> DocumentUri.t -> string

val uri_of_rel_path : t -> string -> DocumentUri.t

(** {1 Document synchronization}

    See {!page-"feature-document-sync"} for the full state machine.  Each
    notification handler returns the diagnostics the caller should publish. *)

(** Track the buffer, rebuild the vault (files may have appeared on disk since
    the last rebuild), and return diagnostics for the opened document. *)
val did_open : t -> rel_path:string -> content:string -> Diagnostic.t list

(** Recompute diagnostics against the in-flight buffer so squigglies update as
    the user types.  The vault is {i not} rebuilt. *)
val did_change : t -> rel_path:string -> content:string -> Diagnostic.t list

(** Drop [rel_path] from the set of open documents; it stops being refreshed
    by {!did_save}. *)
val did_close : t -> rel_path:string -> unit

(** Rebuild the vault, then recompute diagnostics for {i every} open document
    against its disk content.  This is the one moment a stale warning in a
    sibling buffer clears — an unresolved [[[b]]] link in an already-open
    [a.md] loses its squiggly once [b.md] exists and a save fires.

    Returns [(rel_path, diagnostics)] sorted by path, so callers (and expect
    tests) see a stable order.  Documents that cannot be read are omitted. *)
val did_save : t -> (string * Diagnostic.t list) list

(** {1 Features} *)

(** Spec: {!page-"feature-hover"}. *)
val hover : t -> rel_path:string -> line:int -> character:int -> Hover.t option

(** Spec: {!page-"feature-go-to-definition"}.  At most one location. *)
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
    along with its links. *)
val rename
  :  t
  -> rel_path:string
  -> line:int
  -> character:int
  -> new_name:string
  -> WorkspaceEdit.t

(** Spec: {!page-"feature-document-outline"}. *)
val document_symbol : t -> rel_path:string -> DocumentSymbol.t list option

(** Spec: {!page-"feature-codeaction-create-unresolved-link"}.  The single
    action offered creates the missing note and seeds it with a title heading,
    in one workspace edit so the client applies both atomically.

    [only] is the client's requested code-action-kind filter; we have nothing
    but quick fixes to offer, so anything that excludes them yields []. *)
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

(** Spec: {!page-"feature-completion"}. *)
val completion
  :  t
  -> rel_path:string
  -> line:int
  -> character:int
  -> CompletionItem.t list option

(** Spec: {!page-"feature-inlay-hints"}.  [start_line] and [end_line] are the
    requested LSP range's lines, treated as inclusive of [end_line]. *)
val inlay_hint
  :  t
  -> rel_path:string
  -> start_line:int
  -> end_line:int
  -> InlayHint.t list option
