open Core
module B = Cmarkit.Block

(* Cursor
   ======

   A node in place: what it is, where it sits among its siblings, and how to
   reach the nodes around it. Sections are not blocks, so a cursor carries a
   view rather than a block: [V_section] holds a heading and the blocks its
   section covers, grouped out of a container's block list by
   {!group_sections}. *)

type view =
  | V_root
  | V_block of B.t
  | V_item of B.List_item.t Cmarkit.node
  | V_section of
      { heading : B.t
      ; body : B.t list
      }

type cursor =
  { doc : Cmarkit.Doc.t
  ; view : view
  ; index : int
  ; parent : cursor option
  }

let rec path (cursor : cursor) : int list =
  match cursor.parent with
  | None -> []
  | Some parent -> path parent @ [ cursor.index ]
;;

let rec unwrap_attributes (block : B.t) : B.t =
  match block with
  | B.Ext_attributes (a, _) -> unwrap_attributes (B.Attributes.block a)
  | block -> block
;;

(** The djot attributes wrapping [block], outermost first. Cursors keep the
    wrapper so that [id] and [class] stay readable; everything else reads the
    block it wraps. *)
let rec attributes_of (block : B.t) : Cmarkit.Attribute.t list =
  match block with
  | B.Ext_attributes (a, _) ->
    B.Attributes.attributes a :: attributes_of (B.Attributes.block a)
  | _ -> []
;;

let rec blocks_of (block : B.t) : B.t list =
  match block with
  | B.Blocks (blocks, _) -> List.concat_map blocks ~f:blocks_of
  | block ->
    if Option.is_some (Node.of_block (unwrap_attributes block)) then [ block ] else []
;;

let heading_level_of (block : B.t) : int option =
  match unwrap_attributes block with
  | B.Heading (h, _) -> Some (B.Heading.level h)
  | _ -> None
;;

(** Group a container's blocks into views: what comes before the first heading
    stays as it is, and every heading becomes a section covering the blocks up
    to the next heading of its level or higher. A section never crosses a
    container boundary, since grouping only ever sees one container's blocks. *)
let group_sections (blocks : B.t list) : view list =
  let rec go acc blocks =
    match blocks with
    | [] -> List.rev acc
    | block :: rest ->
      (match heading_level_of block with
       | None -> go (V_block block :: acc) rest
       | Some level ->
         let body, after =
           List.split_while rest ~f:(fun b ->
             match heading_level_of b with
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

let children (cursor : cursor) : cursor list =
  List.mapi (child_views cursor.doc cursor.view) ~f:(fun index view ->
    { cursor with view; index; parent = Some cursor })
;;

let descendants (cursor : cursor) : cursor list =
  let rec below cursor =
    List.concat_map (children cursor) ~f:(fun child -> child :: below child)
  in
  below cursor
;;

let node_of_block_exn (block : B.t) : Node.t =
  Option.value_exn (Node.of_block (unwrap_attributes block))
;;

(** The node a cursor is on, without the children of a section: enough for
    every property, since a section's are its heading's. *)
let shallow_node (cursor : cursor) : Node.t =
  match cursor.view with
  | V_root -> Node.Root
  | V_block block -> node_of_block_exn block
  | V_item item -> Node.of_item item
  | V_section { heading; _ } ->
    Node.Section { heading = node_of_block_exn heading; children = [] }
;;

(** The node with its children, for a match a caller receives. *)
let rec node (cursor : cursor) : Node.t =
  match cursor.view with
  | V_section { heading; _ } ->
    Node.Section
      { heading = node_of_block_exn heading
      ; children = List.map (children cursor) ~f:node
      }
  | V_root | V_block _ | V_item _ -> shallow_node cursor
;;

(* Metadata
   -------- *)

let meta (cursor : cursor) : Cmarkit.Meta.t =
  match cursor.view with
  | V_root -> Cmarkit.Meta.none
  | V_block block -> Parse.Common.meta_of_block block
  | V_item (_, meta) -> meta
  | V_section { heading; _ } -> Parse.Common.meta_of_block heading
;;

(** The headings whose sections hold the cursor: its section ancestors, which a
    container boundary stops on its own. *)
let headings (cursor : cursor) : Anchor.heading list =
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

(** Each address of [doc] with the block {!Read.read} resolves it to. Cursors
    look through [Ext_attributes] wrappers, so the block is unwrapped to match.
    The last result is kept, since every cursor of a run shares one note. *)
let resolved : Cmarkit.Doc.t -> (B.t * Anchor.Address.t) list =
  let last = ref None in
  fun doc ->
    match !last with
    | Some (last_doc, table) when phys_equal last_doc doc -> table
    | _ ->
      let blocks = [ Cmarkit.Doc.block doc ] in
      let table =
        Anchor.of_doc doc
        |> List.filter_map ~f:(fun (anchor : Anchor.t) ->
          let address = Anchor.address anchor.value in
          List.hd (Read.read blocks address)
          |> Option.map ~f:(fun block -> unwrap_attributes block, address))
      in
      last := Some (doc, table);
      table
;;

let names (cursor : cursor) : Anchor.Address.t list =
  let named_by block =
    resolved cursor.doc
    |> List.filter_map ~f:(fun (named, address) ->
      Option.some_if (phys_equal named (unwrap_attributes block)) address)
    |> List.dedup_and_sort ~compare:Anchor.Address.compare
  in
  match cursor.view with
  | V_root | V_item _ -> []
  | V_block block -> named_by block
  | V_section { heading; _ } -> named_by heading
;;

(** [id] and [class], which a node cannot carry on its own: they come from the
    djot attribute or the caret marker written on it. An [id] is whatever names
    the node, so a [ ^id ] on a line of its own belongs to the block before it,
    and attribute and caret identifiers share one namespace. *)
let context_props (cursor : cursor) : (string * Node.value) list =
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
  ids @ classes
;;

(** Every property of the node, its own first. A name can repeat: a block can
    carry more than one identifier or class. *)
let props (cursor : cursor) : (string * Node.value) list =
  Node.props (shallow_node cursor) @ context_props cursor
;;

let prop_values (name : string) (cursor : cursor) : Node.value list =
  List.filter_map (props cursor) ~f:(fun (n, value) ->
    Option.some_if (String.equal n name) value)
;;

(* Query
   ===== *)

type cmp =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type pred =
  | Prop of string * cmp * Node.value
  | Has of string
  | Exists of steps
  | Count of steps * cmp * int
  | And of pred list
  | Or of pred list
  | Not of pred

and axis =
  | Self
  | Child
  | Descendant
  | Field of string
  | Section of
      { path : string list
      ; exact : bool
      }

and step =
  { axis : axis
  ; where : pred list
  ; nth : int option
  }

and steps = step list

type t = steps

let is (kind : string) : pred = Prop ("kind", Eq, String kind)

(* Axes
   ---- *)

let is_list (cursor : cursor) : bool =
  match cursor.view with
  | V_block block ->
    (match unwrap_attributes block with
     | B.List _ -> true
     | _ -> false)
  | V_root | V_item _ | V_section _ -> false
;;

let keyed_key (cursor : cursor) : string option =
  match shallow_node cursor with
  | Node.Keyed_paragraph { key } -> Some key
  | _ -> None
;;

let item_key (cursor : cursor) : string option =
  match shallow_node cursor with
  | Node.List_item { key; _ } -> key
  | _ -> None
;;

(** The keyed nodes under [cursor] holding [key]: its keyed children, and the
    keyed items of [cursor] when it is a list. *)
let keyed_under ~(key : string) (cursor : cursor) : cursor list =
  let matching k = Option.value_map k ~default:false ~f:(String.equal key) in
  if is_list cursor
  then
    List.concat_map (children cursor) ~f:(fun item ->
      if matching (item_key item)
      then List.filter (children item) ~f:(fun block -> Option.is_some (keyed_key block))
      else [])
  else List.filter (children cursor) ~f:(fun child -> matching (keyed_key child))
;;

(** The value of [key] on [cursor]: the blocks of the keyed node holding it.
    The fields of a list are its keyed items; those of any other node are its
    keyed children, and, under a list item or a keyed node, the keyed items of
    a list it holds — there a list describes the node above it, whereas under a
    section or a div a list is content of its own. *)
let field_axis ~(key : string) (cursor : cursor) : cursor list =
  let describing =
    match shallow_node cursor with
    | Node.List_item _ | Node.Keyed_paragraph _ ->
      List.filter (children cursor) ~f:is_list |> List.concat_map ~f:(keyed_under ~key)
    | _ -> []
  in
  keyed_under ~key cursor @ describing |> List.concat_map ~f:children
;;

let section_id (cursor : cursor) : string option =
  match cursor.view with
  | V_section { heading; _ } ->
    (match node_of_block_exn heading with
     | Node.Heading { id; _ } -> Some id
     | _ -> None)
  | V_root | V_block _ | V_item _ -> None
;;

let child_sections (cursor : cursor) : cursor list =
  List.filter (children cursor) ~f:(fun child -> Option.is_some (section_id child))
;;

(** The sections under [cursor] matching [path], compared by heading
    identifier. A name is read as one, so both [Setup] and [setup] match the
    heading [## Setup]. This axis walks sections only: a heading inside a
    container is reached through {!Child} or {!Field} first. *)
let section_axis ~(path : string list) ~(exact : bool) (cursor : cursor) : cursor list =
  let wanted = List.map path ~f:Parse.Common.heading_id_of_text in
  let matches name section =
    Option.value_map (section_id section) ~default:false ~f:(String.equal name)
  in
  let rec exactly cursor remaining =
    match remaining with
    | [] -> [ cursor ]
    | name :: rest ->
      List.concat_map (child_sections cursor) ~f:(fun section ->
        if matches name section then exactly section rest else [])
  in
  let rec anywhere cursor remaining =
    List.concat_map (child_sections cursor) ~f:(fun section ->
      let here =
        match remaining with
        | name :: rest when matches name section ->
          if List.is_empty rest then [ section ] else anywhere section rest
        | _ -> []
      in
      here @ anywhere section remaining)
  in
  match wanted with
  | [] -> [ cursor ]
  | wanted -> if exact then exactly cursor wanted else anywhere cursor wanted
;;

(* Running a step
   ------------- *)

let holds_cmp (cmp : cmp) (order : int) : bool =
  match cmp with
  | Eq -> order = 0
  | Ne -> order <> 0
  | Lt -> order < 0
  | Le -> order <= 0
  | Gt -> order > 0
  | Ge -> order >= 0
;;

let compare_values (a : Node.value) (b : Node.value) : int option =
  match a, b with
  | Int a, Int b -> Some (Int.compare a b)
  | String a, String b -> Some (String.compare a b)
  | Bool a, Bool b -> Some (Bool.compare a b)
  | (Int _ | String _ | Bool _), _ -> None
;;

(** Document order without duplicates: a path is the node's index at each
    level, so ordering paths orders the nodes. *)
let in_document_order (cursors : cursor list) : cursor list =
  List.dedup_and_sort cursors ~compare:(fun a b ->
    List.compare Int.compare (path a) (path b))
;;

let rec holds (pred : pred) (cursor : cursor) : bool =
  match pred with
  | Prop (name, cmp, wanted) ->
    List.exists (prop_values name cursor) ~f:(fun actual ->
      Option.value_map (compare_values actual wanted) ~default:false ~f:(holds_cmp cmp))
  | Has name -> not (List.is_empty (prop_values name cursor))
  | Exists steps -> not (List.is_empty (eval steps [ cursor ]))
  | Count (steps, cmp, n) ->
    holds_cmp cmp (Int.compare (List.length (eval steps [ cursor ])) n)
  | And preds -> List.for_all preds ~f:(fun pred -> holds pred cursor)
  | Or preds -> List.exists preds ~f:(fun pred -> holds pred cursor)
  | Not pred -> not (holds pred cursor)

and move (axis : axis) (cursor : cursor) : cursor list =
  match axis with
  | Self -> [ cursor ]
  | Child -> children cursor
  | Descendant -> descendants cursor
  | Field key -> field_axis ~key cursor
  | Section { path; exact } -> section_axis ~path ~exact cursor

(** The axis, then the filter, then the index: [candidates], [kept] and the
    step's output. The two before the output are what {!no_match} reports. *)
and apply (step : step) (input : cursor list) : cursor list * cursor list * cursor list =
  let candidates = in_document_order (List.concat_map input ~f:(move step.axis)) in
  let kept =
    List.filter candidates ~f:(fun cursor ->
      List.for_all step.where ~f:(fun pred -> holds pred cursor))
  in
  let output =
    match step.nth with
    | None -> kept
    | Some n ->
      let length = List.length kept in
      Option.to_list (List.nth kept (if n < 0 then length + n else n))
  in
  candidates, kept, output

and eval (steps : steps) (input : cursor list) : cursor list =
  List.fold steps ~init:input ~f:(fun input step ->
    let _, _, output = apply step input in
    output)
;;

(* Combinators
   ----------- *)

let empty : steps = []

let add (steps : steps) (axis : axis) ~where ~nth : steps =
  steps @ [ { axis; where; nth } ]
;;

let self ?(where = []) ?nth (steps : steps) : steps = add steps Self ~where ~nth
let child ?(where = []) ?nth (steps : steps) : steps = add steps Child ~where ~nth
let descend ?(where = []) ?nth (steps : steps) : steps = add steps Descendant ~where ~nth

let field ?(where = []) ?nth (key : string) (steps : steps) : steps =
  add steps (Field key) ~where ~nth
;;

let section ?(exact = true) ?(where = []) ?nth (path : string list) (steps : steps)
  : steps
  =
  add steps (Section { path; exact }) ~where ~nth
;;

(* Printing
   -------- *)

let cmp_to_string : cmp -> string = function
  | Eq -> "="
  | Ne -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
;;

let rec pred_to_string : pred -> string = function
  | Prop (name, cmp, value) ->
    sprintf "%s %s %s" name (cmp_to_string cmp) (Node.value_to_string value)
  | Has name -> "has " ^ name
  | Exists steps -> sprintf "exists(%s)" (to_string steps)
  | Count (steps, cmp, n) ->
    sprintf "count(%s) %s %d" (to_string steps) (cmp_to_string cmp) n
  | And [] -> "true"
  | Or [] -> "false"
  | And preds -> String.concat ~sep:" and " (List.map preds ~f:grouped)
  | Or preds -> String.concat ~sep:" or " (List.map preds ~f:grouped)
  | Not pred -> "not " ^ grouped pred

and grouped (pred : pred) : string =
  match pred with
  | And _ | Or _ -> "(" ^ pred_to_string pred ^ ")"
  | _ -> pred_to_string pred

and axis_to_string : axis -> string = function
  | Self -> "self"
  | Child -> "child"
  | Descendant -> "descend"
  | Field key -> sprintf "field(%s)" key
  | Section { path; exact } ->
    sprintf
      "section(%s%s)"
      (String.concat ~sep:"/" path)
      (if exact then "" else ", sub-path")

and step_to_string (step : step) : string =
  let where =
    match step.where with
    | [] -> ""
    | preds -> sprintf "[%s]" (String.concat ~sep:" and " (List.map preds ~f:grouped))
  in
  let nth =
    match step.nth with
    | None -> ""
    | Some n -> sprintf "(%d)" n
  in
  axis_to_string step.axis ^ where ^ nth

and to_string : t -> string = function
  | [] -> "self"
  | steps -> String.concat ~sep:" | " (List.map steps ~f:step_to_string)
;;

(* Run
   === *)

let span (cursor : cursor) : Node.span option =
  let of_textloc textloc =
    if Cmarkit.Textloc.is_none textloc
    then None
    else
      Some
        ({ first_line = fst (Cmarkit.Textloc.first_line textloc)
         ; last_line = fst (Cmarkit.Textloc.last_line textloc)
         ; first_byte = Cmarkit.Textloc.first_byte textloc
         ; last_byte = Cmarkit.Textloc.last_byte textloc
         }
         : Node.span)
  in
  match cursor.view with
  | V_section { heading; body } ->
    (* A section spans its heading and the blocks under it. *)
    let locs =
      List.filter_map (heading :: body) ~f:(fun block ->
        let loc = Cmarkit.Meta.textloc (Parse.Common.meta_of_block block) in
        Option.some_if (not (Cmarkit.Textloc.is_none loc)) loc)
    in
    (match List.hd locs, List.last locs with
     | Some first, Some last ->
       Some
         ({ first_line = fst (Cmarkit.Textloc.first_line first)
          ; last_line = fst (Cmarkit.Textloc.last_line last)
          ; first_byte = Cmarkit.Textloc.first_byte first
          ; last_byte = Cmarkit.Textloc.last_byte last
          }
          : Node.span)
     | _ -> None)
  | V_root | V_block _ | V_item _ -> of_textloc (Cmarkit.Meta.textloc (meta cursor))
;;

let render (doc : Cmarkit.Doc.t) (blocks : B.t list) : string =
  let blank = B.Blank_line ("", Cmarkit.Meta.none) in
  Parse.commonmark_of_doc
    (Cmarkit.Doc.make
       ~defs:(Cmarkit.Doc.defs doc)
       (B.Blocks (List.intersperse blocks ~sep:blank, Cmarkit.Meta.none)))
;;

let markdown (cursor : cursor) : string =
  match cursor.view with
  | V_root -> render cursor.doc (blocks_of (Cmarkit.Doc.block cursor.doc))
  | V_block block -> render cursor.doc [ block ]
  | V_item (item, _) -> render cursor.doc (blocks_of (B.List_item.block item))
  | V_section { heading; body } -> render cursor.doc (heading :: body)
;;

let found_of_cursor (cursor : cursor) : Node.found_t =
  { node = node cursor
  ; path = path cursor
  ; names = names cursor
  ; headings = headings cursor
  ; span = span cursor
  ; markdown = markdown cursor
  }
;;

type stage =
  | No_candidate
  | Filtered_out of Node.found_t list
  | Out_of_range of { length : int }

type no_match =
  { index : int
  ; step : step
  ; reached : Node.found_t list
  ; stage : stage
  }

type result =
  { matches : Node.found_t list
  ; why_empty : no_match option
  }

let run (query : t) (doc : Cmarkit.Doc.t) : result =
  let root = { doc; view = V_root; index = 0; parent = None } in
  let rec go index input failure steps =
    match steps with
    | [] -> input, failure
    | step :: rest ->
      let candidates, kept, output = apply step input in
      let failure =
        match failure with
        | Some _ -> failure
        | None ->
          if List.is_empty output
          then
            Some
              { index
              ; step
              ; reached = List.map input ~f:found_of_cursor
              ; stage =
                  (if List.is_empty candidates
                   then No_candidate
                   else if List.is_empty kept
                   then Filtered_out (List.map candidates ~f:found_of_cursor)
                   else Out_of_range { length = List.length kept })
              }
          else None
      in
      go (index + 1) output failure rest
  in
  let final, failure = go 0 [ root ] None query in
  { matches = List.map final ~f:found_of_cursor
  ; why_empty = (if List.is_empty final then failure else None)
  }
;;

let listing (values : string list) : string =
  List.fold values ~init:[] ~f:(fun seen v ->
    if List.mem seen v ~equal:String.equal then seen else v :: seen)
  |> List.rev
  |> function
  | [] -> "none"
  | values -> String.concat values ~sep:", "
;;

let kinds_of (founds : Node.found_t list) : string =
  listing (List.map founds ~f:(fun found -> Node.kind found.node))
;;

let no_match_to_string ({ index; step; reached; stage } : no_match) : string =
  let where = sprintf "step %d (%s)" index (step_to_string step) in
  match stage with
  | No_candidate -> sprintf "%s: nothing to move to from %s" where (kinds_of reached)
  | Filtered_out candidates ->
    let values =
      match step.where with
      | [ Prop (name, _, _) ] | Prop (name, _, _) :: _ ->
        (match List.filter_map candidates ~f:(fun found -> Node.prop name found.node) with
         | [] -> sprintf "none of them has %s" name
         | values ->
           sprintf "%s here: %s" name (listing (List.map values ~f:Node.value_to_string)))
      | _ -> sprintf "kinds here: %s" (kinds_of candidates)
    in
    sprintf "%s: %d nodes, none kept; %s" where (List.length candidates) values
  | Out_of_range { length } -> sprintf "%s: only %d nodes to index" where length
;;
