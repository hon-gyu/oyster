(** Root API module for the oystermark LSP implementation.

    Re-exports feature modules so that [main.ml] (and tests) can access
    everything via [Lsp_lib.*].  As new features are added, register them
    here.

    Everything here but {!Server} is free of LSP protocol types: features take
    document content and byte offsets, and return byte offsets.  {!Server}
    holds the mutable vault state and is the one place those results become
    protocol values.  See {!page-"design-260322-lsp"}. *)

module Util = Lsp_util
module Config = Lsp_config
module Link_collect = Link_collect
module Go_to_definition = Go_to_definition
module Completion = Completion
module Diagnostics = Diagnostics
module Hover = Hover
module Find_references = Find_references
module Rename = Rename
module Document_outline = Document_outline
module Create_unresolved_note = Create_unresolved_note
module Inlay_hints = Inlay_hints
module Server = Server
