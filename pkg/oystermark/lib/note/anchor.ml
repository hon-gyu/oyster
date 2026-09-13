open Core

module Address = struct
  type t =
    | Heading of string
    | Caret of string
    | Attr of string
  [@@deriving sexp, equal, compare]

  let id = function
    | Heading id | Caret id | Attr id -> id
  ;;
end

type loc = Cmarkit.Textloc.t

let sexp_of_loc = Parse.Textloc_conv.sexp_of_t
let loc_of_sexp = Parse.Textloc_conv.t_of_sexp
let compare_loc = Parse.Textloc_conv.compare
let equal_loc a b = Int.equal (compare_loc a b) 0

type heading =
  { text : string
  ; level : int
  ; slug : string
  }
[@@deriving sexp, equal, compare]

type value =
  | Heading of heading
  | Caret of string
  | Attr of
      { id : string
      ; inline : bool
      }
[@@deriving sexp, equal, compare]

type t =
  { value : value
  ; loc : loc
  }
[@@deriving sexp, equal, compare]

let address : value -> Address.t = function
  | Heading h -> Heading h.slug
  | Caret id -> Caret id
  | Attr { id; _ } -> Attr id
;;

let of_doc (doc : Cmarkit.Doc.t) : t list =
  let open Cmarkit in
  let anchors = ref [] in
  let add value meta = anchors := { value; loc = Meta.textloc meta } :: !anchors in
  let add_caret meta =
    Option.iter (Block.Block_id.find meta) ~f:(fun id ->
      add (Caret (Block.Block_id.id id)) meta)
  in
  let add_attr ~inline attr meta =
    Option.iter (Attribute.id attr) ~f:(fun id -> add (Attr { id; inline }) meta)
  in
  let folder =
    Folder.make
      ~block:(fun f acc b ->
        match b with
        | Block.Heading (h, meta) ->
          let text = Parse.Common.inline_to_plain_text (Block.Heading.inline h) in
          let slug =
            Parse.Common.heading_id h
            |> Option.value_exn
                 ~message:"heading missing identifier; parse with Oystermark.Parse"
          in
          add (Heading { text; level = Block.Heading.level h; slug }) meta;
          Folder.default
        | Block.Paragraph (_, meta) ->
          add_caret meta;
          Folder.default
        | Block.Ext_keyed ((_label, body), meta) ->
          add_caret meta;
          Folder.ret (Folder.fold_block f acc body)
        | Block.Ext_attributes (a, meta) ->
          add_attr ~inline:false (Block.Attributes.attributes a) meta;
          Folder.ret (Folder.fold_block f acc (Block.Attributes.block a))
        | _ -> Folder.default)
      ~inline:(fun f acc i ->
        match i with
        | Inline.Ext_attributes (a, meta) ->
          add_attr ~inline:true (Inline.Attributes.attributes a) meta;
          Folder.ret (Folder.fold_inline f acc (Inline.Attributes.inline a))
        | _ -> Folder.default)
      ~inline_ext_default:(fun _ acc _ -> acc)
      ~block_ext_default:(fun _ acc _ -> acc)
      ()
  in
  Folder.fold_doc folder () doc;
  List.rev !anchors
;;
