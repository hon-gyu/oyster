open Core

type block = Djot.Block.t Djot.node

type view =
  | V_root
  | V_block of block
  | V_item of { list : block } (** The [index]th item of [list]. *)
  | V_footnote of
      { label : string
      ; blocks : block list
      }

type t =
  { doc : Djot.Doc.t
  ; view : view
  ; index : int
  ; parent : t option
  }

let root (doc : Djot.Doc.t) : t = { doc; view = V_root; index = 0; parent = None }

(* Navigation
   ========== *)

let items (Djot.Node (_, _, block) : block)
  : (Djot.Block.task_status option * block list) list option
  =
  match block with
  | BulletList (_, items) | OrderedList (_, _, items) ->
    Some (List.map items ~f:(fun blocks -> None, blocks))
  | TaskList (_, items) ->
    Some (List.map items ~f:(fun (status, blocks) -> Some status, blocks))
  | _ -> None
;;

let item (cursor : t) : (Djot.Block.task_status option * block list) option =
  match cursor.view with
  | V_item { list } ->
    Option.bind (items list) ~f:(fun items -> List.nth items cursor.index)
  | V_root | V_block _ | V_footnote _ -> None
;;

let is_node (Djot.Node (_, _, block) : block) : bool =
  match block with
  | RefDef _ | FootnoteDef _ -> false
  | _ -> true
;;

(** The blocks a node holds. [None] for a list, whose children are its items,
    and for a node that holds no blocks. *)
let inner_blocks (cursor : t) : block list option =
  match cursor.view with
  | V_root -> Some (Djot.Doc.blocks cursor.doc)
  | V_footnote { blocks; _ } -> Some blocks
  | V_item _ -> Option.map (item cursor) ~f:snd
  | V_block (Node (_, _, block)) ->
    (match block with
     | Section blocks | BlockQuote blocks | Div blocks | Ext_callout (_, _, _, blocks) ->
       Some blocks
     | Ext_keyed (_, block) -> Some [ block ]
     | _ -> None)
;;

let child_views (cursor : t) : view list =
  let blocks = Option.value (inner_blocks cursor) ~default:[] in
  let blocks = List.filter blocks ~f:is_node |> List.map ~f:(fun b -> V_block b) in
  match cursor.view with
  | V_root ->
    blocks
    @ List.map (Djot.Doc.footnotes cursor.doc) ~f:(fun (label, blocks) ->
      V_footnote { label; blocks })
  | V_block list when Option.is_some (items list) ->
    List.map (Option.value_exn (items list)) ~f:(fun _ -> V_item { list })
  | V_block _ | V_item _ | V_footnote _ -> blocks
;;

let children (cursor : t) : t list =
  List.mapi (child_views cursor) ~f:(fun index view ->
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

(* Kinds and properties
   ==================== *)

let kind (cursor : t) : string =
  match cursor.view with
  | V_root -> "root"
  | V_item _ -> "list_item"
  | V_footnote _ -> "footnote_definition"
  | V_block (Node (_, _, block)) ->
    (match block with
     | Para _ -> "paragraph"
     | Section _ -> "section"
     | Heading _ -> "heading"
     | BlockQuote _ -> "block_quote"
     | CodeBlock _ -> "code_block"
     | Div _ -> "div"
     | OrderedList _ | BulletList _ | TaskList _ -> "list"
     | DefinitionList _ -> "definition_list"
     | ThematicBreak -> "thematic_break"
     | Table _ -> "table"
     | RawBlock _ -> "raw_block"
     | FootnoteDef _ -> "footnote_definition"
     | RefDef _ -> "reference_definition"
     | Ext_keyed _ -> "keyed"
     | Ext_callout _ -> "callout")
;;

let key_of_label (label : Djot.Inline.t Djot.node list) : string =
  String.strip (Parse.Common.plain_text label)
;;

(** The heading of a section, or the heading itself. *)
let heading (cursor : t) : Anchor.heading option =
  match cursor.view with
  | V_block
      (Node
         ( _
         , attrs
         , ( Section (Node (_, _, Heading (level, inlines)) :: _)
           | Heading (level, inlines) ) )) ->
    Some
      { text = Parse.Common.plain_text inlines
      ; level
      ; slug = Option.value (Djot.Attr.id attrs) ~default:""
      }
  | V_root | V_block _ | V_item _ | V_footnote _ -> None
;;

(** A code block's text without the newline that ends its last line. *)
let code_text (text : string) : string = String.chop_suffix_if_exists text ~suffix:"\n"

let own_props (cursor : t) : (string * Node.value) list =
  let string s = Node.String s in
  match cursor.view with
  | V_root -> []
  | V_footnote { label; _ } -> [ "label", string label ]
  | V_item _ ->
    let status, blocks = Option.value_exn (item cursor) in
    List.filter_opt
      [ Option.map status ~f:(fun status ->
          ( "task"
          , string
              (match status with
               | Complete -> "checked"
               | Incomplete -> "unchecked") ))
      ; (match blocks with
         | [ Node (_, _, Ext_keyed (label, _)) ] ->
           Some ("key", string (key_of_label label))
         | _ -> None)
      ]
  | V_block (Node (_, _, block)) ->
    (match block with
     | Section _ | Heading _ ->
       Option.value_map (heading cursor) ~default:[] ~f:(fun { text; level; _ } ->
         [ "level", Node.Int level; "text", string text ])
     | CodeBlock (lang, text) ->
       (if String.is_empty lang then [] else [ "lang", string lang ])
       @ [ "text", string (code_text text) ]
     | RawBlock (format, text) ->
       [ "format", string format; "text", string (code_text text) ]
     | Ext_callout (type_, _, title, _) ->
       ("type", string (String.lowercase type_))
       ::
       (match String.strip (Parse.Common.plain_text title) with
        | "" -> []
        | title -> [ "title", string title ])
     | OrderedList (_, spacing, items) | BulletList (spacing, items) ->
       [ ( "ordered"
         , Node.Bool
             (match block with
              | OrderedList _ -> true
              | _ -> false) )
       ; "tight", Node.Bool (Poly.equal spacing Djot.Block.Tight)
       ; "length", Node.Int (List.length items)
       ]
     | TaskList (spacing, items) ->
       [ "ordered", Node.Bool false
       ; "tight", Node.Bool (Poly.equal spacing Djot.Block.Tight)
       ; "length", Node.Int (List.length items)
       ]
     | Ext_keyed (label, _) -> [ "key", string (key_of_label label) ]
     | FootnoteDef (label, _) -> [ "label", string label ]
     | Para _
     | BlockQuote _
     | Div _
     | DefinitionList _
     | ThematicBreak
     | Table _
     | RefDef _ -> [])
;;

let attrs (cursor : t) : Djot.Attr.t =
  match cursor.view with
  | V_block node -> Djot.Node.attrs node
  | V_root | V_item _ | V_footnote _ -> []
;;

let props (cursor : t) : (string * Node.value) list =
  let attrs = attrs cursor in
  (("kind", Node.String (kind cursor)) :: own_props cursor)
  @ List.map (Option.to_list (Djot.Attr.id attrs)) ~f:(fun id -> "id", Node.String id)
  @ List.map (Djot.Attr.classes attrs) ~f:(fun c -> "class", Node.String c)
  @ List.map (Djot.Attr.key_values attrs) ~f:(fun (k, v) -> k, Node.String v)
;;

let prop_values (name : string) (cursor : t) : Node.value list =
  List.filter_map (props cursor) ~f:(fun (n, value) ->
    Option.some_if (String.equal n name) value)
;;

let names (cursor : t) : Anchor.Address.t list =
  match cursor.view, Djot.Attr.id (attrs cursor) with
  | V_block (Node (_, _, (Section _ | Heading _))), Some id -> [ Heading id ]
  | _, Some id -> [ Attr id ]
  | _, None -> []
;;

(* Reporting a match
   ================= *)

let headings (cursor : t) : Anchor.heading list =
  let rec up cursor acc =
    match cursor.parent with
    | None -> acc
    | Some parent ->
      let acc =
        match parent.view with
        | V_block (Node (_, _, Section _)) ->
          Option.value_map (heading parent) ~default:acc ~f:(fun h -> h :: acc)
        | V_root | V_block _ | V_item _ | V_footnote _ -> acc
      in
      up parent acc
  in
  up cursor []
;;

let textloc (cursor : t) : Djot.Textloc.t =
  let of_blocks blocks =
    match
      List.filter_map blocks ~f:(fun block ->
        let loc = Djot.Doc.textloc cursor.doc block in
        Option.some_if (not (Djot.Textloc.is_none loc)) loc)
    with
    | [] -> Djot.Textloc.none
    | first :: _ as locs -> Djot.Textloc.reloc ~first ~last:(List.last_exn locs)
  in
  match cursor.view with
  | V_root -> Djot.Textloc.none
  | V_block block -> Anchor.extent cursor.doc block
  | V_footnote { blocks; _ } -> of_blocks blocks
  | V_item { list } ->
    (match Djot.Doc.parts cursor.doc list with
     | Items locs -> Option.value (List.nth locs cursor.index) ~default:Djot.Textloc.none
     | NoParts | DefItems _ | TableRows _ -> Djot.Textloc.none)
;;

let span (cursor : t) : Node.span option =
  let textloc = textloc cursor in
  Option.some_if (not (Djot.Textloc.is_none textloc)) textloc
  |> Option.map ~f:(fun textloc : Node.span ->
    { first_line = fst (Djot.Textloc.first_line textloc)
    ; last_line = fst (Djot.Textloc.last_line textloc)
    ; first_byte = Djot.Textloc.first_byte textloc
    ; last_byte = Djot.Textloc.last_byte textloc
    })
;;

(** The blocks a note rooted at the cursor holds: its {!inner_blocks} without a
    section's heading, or the block itself for a node that holds none. *)
let contents (cursor : t) : block list =
  match cursor.view, inner_blocks cursor with
  | V_block (Node (_, _, Section (_ :: blocks))), _ -> blocks
  | _, Some blocks -> blocks
  | V_block block, None -> [ block ]
  | (V_root | V_item _ | V_footnote _), None -> []
;;

let found (cursor : t) : Node.found_t =
  { kind = kind cursor
  ; path = path cursor
  ; names = names cursor
  ; headings = headings cursor
  ; span = span cursor
  ; props = props cursor
  ; blocks =
      (match cursor.view with
       | V_block block -> [ block ]
       | V_root | V_item _ | V_footnote _ -> contents cursor)
  }
;;
