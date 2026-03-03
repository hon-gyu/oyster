module Wikilink = Oystermark_base.Wikilink
module Block_id = Oystermark_base.Block_id
module Link_ref = Vault.Link_ref
module Index = Vault.Index
module Resolve = Vault.Resolve

(** The mapper that transforms a cmarkit Doc, parsing wikilinks in inline
    text nodes and tag block identifiers at paragraph ends to meta. *)
let mapper =
  Cmarkit.Mapper.make
    ~inline_ext_default:(fun _m i -> Some i)
    ~inline:Wikilink.parse
    ~block:Block_id.tag_block_id_meta
    ()
;;

(** [of_string ?strict ?layout s] parses markdown string [s] into a cmarkit
    Doc with wikilinks and block IDs resolved via the mapper. *)
let of_string ?(strict = false) ?(layout = false) (s : string) : Cmarkit.Doc.t =
  let doc = Cmarkit.Doc.of_string ~strict ~layout s in
  Cmarkit.Mapper.map_doc mapper doc
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
let of_string_resolved ?(strict = false) ?(layout = false) ~index ~curr_file s =
  let doc = of_string ~strict ~layout s in
  let res_mapper = resolution_mapper ~index ~curr_file in
  Cmarkit.Mapper.map_doc res_mapper doc
;;
