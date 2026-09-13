open Core

type loc = Cmarkit.Textloc.t

let sexp_of_loc = Parse.Textloc_conv.sexp_of_t
let loc_of_sexp = Parse.Textloc_conv.t_of_sexp
let compare_loc = Parse.Textloc_conv.compare
let equal_loc a b = Int.equal (compare_loc a b) 0

type kind =
  | Link
  | Embed
[@@deriving sexp, equal, compare]

type t =
  { reference : Link_ref.t
  ; kind : kind
  ; loc : loc
  }
[@@deriving sexp, equal, compare]

let of_doc (doc : Cmarkit.Doc.t) : t list =
  let open Cmarkit in
  let links = ref [] in
  let add reference kind meta =
    links := { reference; kind; loc = Meta.textloc meta } :: !links
  in
  let folder =
    Folder.make
      ~block:(fun f acc b ->
        match b with
        | Block.Ext_keyed ((_label, body), _) -> Folder.ret (Folder.fold_block f acc body)
        | Block.Ext_attributes (a, _) ->
          Folder.ret (Folder.fold_block f acc (Block.Attributes.block a))
        | _ -> Folder.default)
      ~inline:(fun f acc i ->
        match i with
        | Inline.Ext_wikilink (w, meta) ->
          add
            (Link_ref.of_wikilink w)
            (if Inline.Wikilink.embed w then Embed else Link)
            meta;
          Folder.default
        | Inline.Link (l, meta) ->
          Option.iter
            (Link_ref.of_cmark_reference (Inline.Link.reference l))
            ~f:(fun r -> add r Link meta);
          Folder.default
        | Inline.Image (l, meta) ->
          Option.iter
            (Link_ref.of_cmark_reference (Inline.Link.reference l))
            ~f:(fun r -> add r Embed meta);
          Folder.default
        | Inline.Ext_attributes (a, _) ->
          Folder.ret (Folder.fold_inline f acc (Inline.Attributes.inline a))
        | _ -> Folder.default)
      ~inline_ext_default:(fun _ acc _ -> acc)
      ~block_ext_default:(fun _ acc _ -> acc)
      ()
  in
  Folder.fold_doc folder () doc;
  List.rev !links
;;
