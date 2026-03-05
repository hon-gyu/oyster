(** Oystermark extends CommonMark with Obsidian-style wikilinks and block
    references. Processing follows a two-pass pipeline:

    {b Pass 1 — Doc-level parsing} ({!Parse.of_string}):
    Each document is parsed independently. The mapper rewrites inline text nodes
    to recognize wikilink syntax and scans paragraph blocks for trailing block-id
    markers ({e ^id}), attaching them as block metadata.

    {b Index construction} ({!Vault.build}):
    The vault is scanned across all documents to build an {!Vault.Index.t} that
    maps note names, headings, and block ids to their locations.

    {b Pass 2 — Resolution} ({!resolve}):
    Each document is mapped again with the vault index. Wikilinks and standard
    markdown links/images are resolved to concrete targets, and the resolution
    result is attached to the node's metadata via {!Vault.Resolve.resolved_key}. *)

module Parse = Parse
module Vault = Vault
module Html = Html

(** Create a resolution mapper that resolves links against the given index. *)
let resolution_mapper ~(index : Vault.Index.t) ~(curr_file : string) =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i ->
      match i with
      | Parse.Wikilink.Ext_wikilink (w, meta) ->
        let link_ref = Vault.Link_ref.of_wikilink w in
        let result = Vault.Resolve.resolve link_ref curr_file index in
        let meta' = Cmarkit.Meta.add Vault.Resolve.resolved_key result meta in
        Some (Parse.Wikilink.Ext_wikilink (w, meta'))
      | other -> Some other)
    ~inline:(fun _m i ->
      match i with
      | Cmarkit.Inline.Link (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Vault.Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = Vault.Resolve.resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add Vault.Resolve.resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Link (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | Cmarkit.Inline.Image (link, meta) ->
        let ref_ = Cmarkit.Inline.Link.reference link in
        (match Vault.Link_ref.of_cmark_reference ref_ with
         | Some link_ref ->
           let result = Vault.Resolve.resolve link_ref curr_file index in
           let meta' = Cmarkit.Meta.add Vault.Resolve.resolved_key result meta in
           Cmarkit.Mapper.ret (Cmarkit.Inline.Image (link, meta'))
         | None -> Cmarkit.Mapper.default)
      | _ -> Cmarkit.Mapper.default)
    ()
;;

(** Resolve links in a pre-parsed document against a vault index. *)
let resolve ~(index : Vault.Index.t) ~(curr_file : string) (doc : Cmarkit.Doc.t)
  : Cmarkit.Doc.t
  =
  let res_mapper = resolution_mapper ~index ~curr_file in
  Cmarkit.Mapper.map_doc res_mapper doc
;;
