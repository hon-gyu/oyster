open Core
module B = Cmarkit.Block

type view =
  | V_root
  | V_block of B.t
  | V_item of B.List_item.t Cmarkit.node
  | V_section of
      { heading : B.t
      ; body : B.t list
      }

type t =
  { doc : Cmarkit.Doc.t
  ; named : (B.t * Anchor.Address.t) list
  ; view : view
  ; index : int
  ; parent : t option
  }

let rec unwrap_attributes (block : B.t) : B.t =
  match block with
  | B.Ext_attributes (a, _) -> unwrap_attributes (B.Attributes.block a)
  | block -> block
;;

let rec attributes_of (block : B.t) : Cmarkit.Attribute.t list =
  match block with
  | B.Ext_attributes (a, _) ->
    B.Attributes.attributes a :: attributes_of (B.Attributes.block a)
  | _ -> []
;;

let node_of_block_exn (block : B.t) : Node.t =
  Option.value_exn (Node.of_block (unwrap_attributes block))
;;

let rec blocks_of (block : B.t) : B.t list =
  match block with
  | B.Blocks (blocks, _) -> List.concat_map blocks ~f:blocks_of
  | block ->
    if Option.is_some (Node.of_block (unwrap_attributes block)) then [ block ] else []
;;

(* Navigation
   ========== *)

(** Group a container's blocks into views: what comes before the first heading
    stays as it is, and every heading becomes a section covering the blocks up
    to the next heading of its level or higher. A section never crosses a
    container boundary, since grouping only ever sees one container's blocks. *)
let group_sections (blocks : B.t list) : view list =
  let level_of block =
    match unwrap_attributes block with
    | B.Heading (h, _) -> Some (B.Heading.level h)
    | _ -> None
  in
  let rec go acc blocks =
    match blocks with
    | [] -> List.rev acc
    | block :: rest ->
      (match level_of block with
       | None -> go (V_block block :: acc) rest
       | Some level ->
         let body, after =
           List.split_while rest ~f:(fun b ->
             match level_of b with
             | Some l -> l > level
             | None -> true)
         in
         go (V_section { heading = block; body } :: acc) after)
  in
  go [] blocks
;;

let child_views (doc : Cmarkit.Doc.t) (view : view) : view list =
  let grouped block = group_sections (blocks_of block) in
  match view with
  | V_root -> grouped (Cmarkit.Doc.block doc)
  | V_section { body; _ } -> group_sections body
  | V_item (item, _) -> grouped (B.List_item.block item)
  | V_block block ->
    (match unwrap_attributes block with
     | B.Block_quote (bq, meta) ->
       let body = B.Block_quote.block bq in
       (match B.Callout.find meta with
        | Some _ -> grouped (B.Callout.strip_header body)
        | None -> grouped body)
     | B.Ext_div (d, _) -> grouped (B.Div.block d)
     | B.Ext_keyed ((_label, body), _) -> grouped body
     | B.Ext_footnote_definition (fn, _) -> grouped (B.Footnote.block fn)
     | B.List (l, _) -> List.map (B.List'.items l) ~f:(fun item -> V_item item)
     | _ -> [])
;;

let children (cursor : t) : t list =
  List.mapi (child_views cursor.doc cursor.view) ~f:(fun index view ->
    { cursor with view; index; parent = Some cursor })
;;

let descendants (cursor : t) : t list =
  let rec below cursor =
    List.concat_map (children cursor) ~f:(fun child -> child :: below child)
  in
  below cursor
;;

let rec path (cursor : t) : int list =
  match cursor.parent with
  | None -> []
  | Some parent -> path parent @ [ cursor.index ]
;;

(** Each address of [doc] with the block {!Read.read} resolves it to. Cursors
    look through [Ext_attributes] wrappers, so the block is unwrapped to
    match. *)
let root (doc : Cmarkit.Doc.t) : t =
  let blocks = [ Cmarkit.Doc.block doc ] in
  let named =
    Anchor.of_doc doc
    |> List.filter_map ~f:(fun (anchor : Anchor.t) ->
      let address = Anchor.address anchor.value in
      List.hd (Read.read blocks address)
      |> Option.map ~f:(fun block -> unwrap_attributes block, address))
  in
  { doc; named; view = V_root; index = 0; parent = None }
;;

(* Nodes and properties
   ==================== *)

let shallow_node (cursor : t) : Node.t =
  match cursor.view with
  | V_root -> Node.Root
  | V_block block -> node_of_block_exn block
  | V_item item -> Node.of_item item
  | V_section { heading; _ } ->
    Node.Section { heading = node_of_block_exn heading; children = [] }
;;

let rec node (cursor : t) : Node.t =
  match cursor.view with
  | V_section { heading; _ } ->
    Node.Section
      { heading = node_of_block_exn heading
      ; children = List.map (children cursor) ~f:node
      }
  | V_root | V_block _ | V_item _ -> shallow_node cursor
;;

let names (cursor : t) : Anchor.Address.t list =
  let named_by block =
    cursor.named
    |> List.filter_map ~f:(fun (named, address) ->
      Option.some_if (phys_equal named (unwrap_attributes block)) address)
    |> List.dedup_and_sort ~compare:Anchor.Address.compare
  in
  match cursor.view with
  | V_root | V_item _ -> []
  | V_block block -> named_by block
  | V_section { heading; _ } -> named_by heading
;;

(** Every property of the node, its own first, then [id] and [class], which a
    node cannot carry on its own: they come from the djot attribute or the
    caret marker written on it. An [id] is whatever names the node, so a [ ^id ]
    on a line of its own belongs to the block before it, and attribute and
    caret identifiers share one namespace. *)
let props (cursor : t) : (string * Node.value) list =
  let ids =
    List.filter_map (names cursor) ~f:(function
      | Anchor.Address.Attr id | Anchor.Address.Caret id -> Some ("id", Node.String id)
      | Anchor.Address.Heading _ -> None)
  in
  let classes =
    match cursor.view with
    | V_root | V_item _ -> []
    | V_block block | V_section { heading = block; _ } ->
      List.concat_map (attributes_of block) ~f:Cmarkit.Attribute.classes
      |> List.map ~f:(fun c -> "class", Node.String c)
  in
  Node.props (shallow_node cursor) @ ids @ classes
;;

let prop_values (name : string) (cursor : t) : Node.value list =
  List.filter_map (props cursor) ~f:(fun (n, value) ->
    Option.some_if (String.equal n name) value)
;;

(* Reporting a match
   ================= *)

(** The headings whose sections hold the cursor: its section ancestors, which a
    container boundary stops on its own. *)
let headings (cursor : t) : Anchor.heading list =
  let rec up cursor acc =
    match cursor.parent with
    | None -> acc
    | Some parent ->
      let acc =
        match parent.view with
        | V_section { heading; _ } ->
          (match node_of_block_exn heading with
           | Node.Heading { level; id; text } ->
             ({ text; level; slug = id } : Anchor.heading) :: acc
           | _ -> acc)
        | V_root | V_block _ | V_item _ -> acc
      in
      up parent acc
  in
  up cursor []
;;

let span (cursor : t) : Node.span option =
  let textloc_of block = Cmarkit.Meta.textloc (Parse.Common.meta_of_block block) in
  let textloc =
    match cursor.view with
    | V_root -> Cmarkit.Textloc.none
    | V_block block -> textloc_of block
    | V_item (_, meta) -> Cmarkit.Meta.textloc meta
    | V_section { heading; body } ->
      (* A section spans its heading and the blocks under it. *)
      (match
         List.filter (heading :: body) ~f:(fun block ->
           not (Cmarkit.Textloc.is_none (textloc_of block)))
       with
       | [] -> Cmarkit.Textloc.none
       | first :: _ as blocks ->
         Cmarkit.Textloc.reloc
           ~first:(textloc_of first)
           ~last:(textloc_of (List.last_exn blocks)))
  in
  Option.some_if (not (Cmarkit.Textloc.is_none textloc)) textloc
  |> Option.map ~f:(fun textloc : Node.span ->
    { first_line = fst (Cmarkit.Textloc.first_line textloc)
    ; last_line = fst (Cmarkit.Textloc.last_line textloc)
    ; first_byte = Cmarkit.Textloc.first_byte textloc
    ; last_byte = Cmarkit.Textloc.last_byte textloc
    })
;;

let markdown (cursor : t) : string =
  let render blocks =
    let blank = B.Blank_line ("", Cmarkit.Meta.none) in
    Parse.commonmark_of_doc
      (Cmarkit.Doc.make
         ~defs:(Cmarkit.Doc.defs cursor.doc)
         (B.Blocks (List.intersperse blocks ~sep:blank, Cmarkit.Meta.none)))
  in
  match cursor.view with
  | V_root -> render (blocks_of (Cmarkit.Doc.block cursor.doc))
  | V_block block -> render [ block ]
  | V_item (item, _) -> render (blocks_of (B.List_item.block item))
  | V_section { heading; body } -> render (heading :: body)
;;

let found (cursor : t) : Node.found_t =
  { node = node cursor
  ; path = path cursor
  ; names = names cursor
  ; headings = headings cursor
  ; span = span cursor
  ; markdown = markdown cursor
  }
;;
