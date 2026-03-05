(** Oystermark extends CommonMark with Obsidian-style wikilinks and block
    references. Processing follows a two-pass pipeline:

    {b Pass 1 — Doc-level parsing} ([of_string]):
    Each document is parsed independently. The mapper rewrites inline text nodes
    to recognize wikilink syntax and scans paragraph blocks for trailing block-id
    markers ({e ^id}), attaching them as block metadata.

    {b Index construction} (external):
    The vault is scanned across all documents to build an {!Index.t} that maps
    note names, headings, and block ids to their locations.

    {b Pass 2 — Resolution} ([of_string_resolved]):
    Each document is mapped again with the vault index. Wikilinks and standard
    markdown links/images are resolved to concrete targets, and the resolution
    result is attached to the node's metadata via {!Resolve.resolved_key}. *)

module Base = Oystermark_base
module Wikilink = Oystermark_base.Wikilink
module Block_id = Oystermark_base.Block_id
module Link_ref = Vault.Link_ref
module Index = Vault.Index
module Resolve = Vault.Resolve
module Html = Html

(** A scanned vault: the index plus pre-resolved docs from pass 1. *)
type vault =
  { index : Index.t
  ; docs : (string * Cmarkit.Doc.t) list (** [(rel_path, doc)] *)
  }

(** Scan a vault directory: parse every markdown file (pass 1) and build the
    index in a single traversal. *)
let build_vault (vault_root : string) : vault =
  let index, docs = Index.build vault_root in
  { index; docs }
;;

(** Create a resolution mapper that resolves links against the given index. *)
let resolution_mapper ~(index : Index.t) ~(curr_file : string) =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i ->
      match i with
      | Wikilink.Ext_wikilink (w, meta) ->
        let link_ref = Link_ref.of_wikilink w in
        let result = Resolve.resolve link_ref curr_file index in
        let meta' = Cmarkit.Meta.add Resolve.resolved_key result meta in
        Some (Wikilink.Ext_wikilink (w, meta'))
      | other -> Some other)
    ~inline:(fun _m i ->
      match i with
      | Cmarkit.Inline.Link (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = Resolve.resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add Resolve.resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Link (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | Cmarkit.Inline.Image (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = Resolve.resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add Resolve.resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Image (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default)
    ()
;;

(** Parse and resolve a markdown string against a vault index. *)
let resolve ~(index : Index.t) ~(curr_file : string) (pre_resolve : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
  let res_mapper = resolution_mapper ~index ~curr_file in
  Cmarkit.Mapper.map_doc res_mapper pre_resolve
;;
