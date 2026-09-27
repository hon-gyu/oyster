open Core

module Address = struct
  type t =
    | Heading of string
    | Attr of string
  [@@deriving sexp, equal, compare]

  let id = function
    | Heading id | Attr id -> id
  ;;
end

type loc = Djot.Textloc.t

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

type definition =
  | Heading of heading
  | Attr of
      { id : string
      ; inline : bool
      }
[@@deriving sexp, equal, compare]

type t =
  { definition : definition
  ; loc : loc
  }
[@@deriving sexp, equal, compare]

let address : definition -> Address.t = function
  | Heading h -> Heading h.slug
  | Attr { id; _ } -> Attr id
;;

(** The location of [node] extended over its attribute specs, which a block's
    precede and an inline's follow. *)
let extent (doc : Djot.Doc.t) (node : _ Djot.node) : loc =
  let loc = Djot.Doc.textloc doc node in
  List.fold (Djot.Doc.syntax_locs doc node) ~init:loc ~f:(fun loc (role, spec) ->
    match role with
    | RAttrSpec when Djot.Textloc.first_byte spec < Djot.Textloc.first_byte loc ->
      Djot.Textloc.reloc ~first:spec ~last:loc
    | RAttrSpec when Djot.Textloc.last_byte spec > Djot.Textloc.last_byte loc ->
      Djot.Textloc.reloc ~first:loc ~last:spec
    | RAttrSpec | ROpenFence | RCloseFence -> loc)
;;

let of_doc (doc : Djot.Doc.t) : t list =
  let anchors = ref [] in
  let add definition node =
    let loc =
      match definition with
      | Heading _ -> Djot.Doc.textloc doc node
      | Attr _ -> extent doc node
    in
    anchors := { definition; loc } :: !anchors
  in
  let add_heading ~slug (Djot.Node (_, _, heading) as node : Djot.Block.t Djot.node) =
    match heading with
    | Heading (level, inlines) ->
      add (Heading { text = Parse.Common.plain_text inlines; level; slug }) node
    | _ -> ()
  in
  let folder =
    Djot.Folder.make
      ~block:(fun _ () (Node (_, attrs, block) as node) ->
        (match block, Djot.Attr.id attrs with
         | Section (heading :: _), Some slug -> add_heading ~slug heading
         | Heading _, Some slug -> add_heading ~slug node
         | Section _, None | Heading _, None -> ()
         | _, Some id -> add (Attr { id; inline = false }) node
         | _, None -> ());
        Djot.Folder.default)
      ~inline:(fun _ () (Node (_, attrs, _) as node) ->
        Option.iter (Djot.Attr.id attrs) ~f:(fun id ->
          add (Attr { id; inline = true }) node);
        Djot.Folder.default)
      ()
  in
  Djot.Folder.fold_doc folder () doc;
  List.rev !anchors
;;
