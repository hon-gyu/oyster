(** Root API module for the oystermark LSP pure logic layer.

    Re-exports feature modules so that [main.ml] (and tests) can access
    everything via [Lsp_lib.*].  As new features are added, register them
    here. *)

module Util = Lsp_util
module Config = Lsp_config
module Link_collect = Link_collect
module Go_to_definition = Go_to_definition
module Diagnostics = Diagnostics
module Hover = Hover
module Find_references = Find_references
module Inlay_hints = Inlay_hints
