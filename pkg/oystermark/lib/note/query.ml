open Core
module B = Cmarkit.Block

(* Cursor
   ====== *)

type view =
  | Block of B.t
  | Item of B.List_item.t Cmarkit.node

type cursor =
  { doc : Cmarkit.Doc.t
  ; view : view
  ; index : int
  ; parent : cursor option
  }

let node (cursor : cursor) : Node.t =
  match cursor.view with
  | Block block -> Option.value_exn (Node.of_block block)
  | Item item -> Node.of_item item
;;

let path (cursor : cursor) : int list =
  let rec up cursor acc =
    let acc = cursor.index :: acc in
    match cursor.parent with
    | None -> acc
    | Some parent -> up parent acc
  in
  up cursor []
;;

let rec unwrap_attributes (block : B.t) : B.t =
  match block with
  | B.Ext_attributes (a, _) -> unwrap_attributes (B.Attributes.block a)
  | block -> block
;;

let rec blocks_of (block : B.t) : B.t list =
  match block with
  | B.Blocks (blocks, _) -> List.concat_map blocks ~f:blocks_of
  | B.Ext_attributes (a, _) -> blocks_of (B.Attributes.block a)
  | block -> if Option.is_some (Node.of_block block) then [ block ] else []
;;

let child_views (view : view) : view list =
  let blocks block = List.map (blocks_of block) ~f:(fun block -> Block block) in
  match view with
  | Item (item, _) -> blocks (B.List_item.block item)
  | Block (B.Block_quote (bq, meta)) ->
    let body = B.Block_quote.block bq in
    (match B.Callout.find meta with
     | Some _ -> blocks (B.Callout.strip_header body)
     | None -> blocks body)
  | Block (B.Ext_div (d, _)) -> blocks (B.Div.block d)
  | Block (B.Ext_keyed ((_label, body), _)) -> blocks body
  | Block (B.Ext_footnote_definition (fn, _)) -> blocks (B.Footnote.block fn)
  | Block (B.List (l, _)) -> List.map (B.List'.items l) ~f:(fun item -> Item item)
  | Block _ -> []
;;

let top (doc : Cmarkit.Doc.t) : cursor list =
  List.mapi
    (blocks_of (Cmarkit.Doc.block doc))
    ~f:(fun index block -> { doc; view = Block block; index; parent = None })
;;

let children (cursor : cursor) : cursor list =
  List.mapi (child_views cursor.view) ~f:(fun index view ->
    { cursor with view; index; parent = Some cursor })
;;

let siblings (cursor : cursor) : cursor list =
  match cursor.parent with
  | None -> top cursor.doc
  | Some parent -> children parent
;;

let heading_level (view : view) : int option =
  match view with
  | Block (B.Heading (h, _)) -> Some (B.Heading.level h)
  | Block _ | Item _ -> None
;;

let section ~nested (cursor : cursor) : cursor list =
  match heading_level cursor.view with
  | Some level ->
    List.drop (siblings cursor) (cursor.index + 1)
    |> List.take_while ~f:(fun sibling ->
      match heading_level sibling.view with
      | Some sibling_level -> nested && sibling_level > level
      | None -> true)
  | None -> []
;;

(* Metadata
   -------- *)

let meta (cursor : cursor) : Cmarkit.Meta.t =
  match cursor.view with
  | Block block -> Parse.Common.meta_of_block block
  | Item (_, meta) -> meta
;;

let headings (cursor : cursor) : Anchor.heading list =
  (* Scan the siblings before [cursor] from nearest to farthest: a heading
     encloses [cursor] if its level is lower than every heading seen so far,
     [cursor] included. Then do the same for the parent. *)
  let rec enclosing cursor =
    let _, here =
      List.take (siblings cursor) cursor.index
      |> List.rev
      |> List.fold
           ~init:(Option.value (heading_level cursor.view) ~default:Int.max_value, [])
           ~f:(fun (lowest, acc) sibling ->
             match node sibling with
             | Heading { level; id; text } when level < lowest ->
               level, ({ level; slug = id; text } : Anchor.heading) :: acc
             | _ -> lowest, acc)
    in
    match cursor.parent with
    | None -> here
    | Some parent -> enclosing parent @ here
  in
  enclosing cursor
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
  match cursor.view with
  | Item _ -> []
  | Block block ->
    resolved cursor.doc
    |> List.filter_map ~f:(fun (named, address) ->
      Option.some_if (phys_equal named block) address)
    |> List.dedup_and_sort ~compare:Anchor.Address.compare
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
  | Named of Anchor.Address.t
  | Count of t * cmp * int
  | And of pred list
  | Or of pred list
  | Not of pred
  | Custom of string * (cursor -> bool)

and step =
  | Children
  | Section of { nested : bool }
  | Filter of pred
  | Nth of int
  | Each of t list
  | Recurse of
      { steps : t
      ; emit : pred
      ; descend : pred
      }

and t = step list

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

let rec holds (pred : pred) (cursor : cursor) : bool =
  match pred with
  | Prop (prop, cmp, wanted) ->
    (match Node.prop prop (node cursor) with
     | None -> false
     | Some actual ->
       Option.value_map (compare_values actual wanted) ~default:false ~f:(holds_cmp cmp))
  | Has prop -> Option.is_some (Node.prop prop (node cursor))
  | Named address -> List.mem (names cursor) address ~equal:Anchor.Address.equal
  | Count (query, cmp, n) ->
    holds_cmp cmp (Int.compare (List.length (eval query [ cursor ])) n)
  | And preds -> List.for_all preds ~f:(fun pred -> holds pred cursor)
  | Or preds -> List.exists preds ~f:(fun pred -> holds pred cursor)
  | Not pred -> not (holds pred cursor)
  | Custom (_, f) -> f cursor

and apply (step : step) (input : cursor list) : cursor list =
  match step with
  | Children -> List.concat_map input ~f:children
  | Section { nested } -> List.concat_map input ~f:(section ~nested)
  | Filter pred -> List.filter input ~f:(holds pred)
  | Nth n -> Option.to_list (List.nth input (n - 1))
  | Each queries ->
    List.concat_map input ~f:(fun cursor ->
      List.concat_map queries ~f:(fun query -> eval query [ cursor ]))
  | Recurse { steps; emit; descend } ->
    let rec expand cursor =
      List.concat_map (eval steps [ cursor ]) ~f:(fun found ->
        (if holds emit found then [ found ] else [])
        @ if holds descend found then expand found else [])
    in
    List.concat_map input ~f:expand

and eval (query : t) (input : cursor list) : cursor list =
  List.fold query ~init:input ~f:(fun input step -> apply step input)
;;

module Sugar = struct
  let is (kind : string) : pred = Prop ("kind", Eq, String kind)
  let recurse (steps : t) : t = [ Recurse { steps; emit = And []; descend = And [] } ]

  let until (steps : t) (pred : pred) : t =
    [ Recurse { steps; emit = pred; descend = Not pred } ]
  ;;

  let descendants : t = recurse [ Children ]
  let descendants_or_self : t = [ Each [ []; descendants ] ]
  let times (n : int) (query : t) : t = List.concat (List.init n ~f:(fun _ -> query))
  let exists (query : t) (pred : pred) : pred = Count (query @ [ Filter pred ], Gt, 0)

  let for_all (query : t) (pred : pred) : pred =
    Count (query @ [ Filter (Not pred) ], Eq, 0)
  ;;

  let value : t =
    [ Each
        [ [ Filter (is "keyed"); Children ]
        ; [ Filter (And [ is "list_item"; Has "key" ]); Children; Children ]
        ]
    ]
  ;;

  let field (key : string) : t =
    [ Each [ []; [ Children ] ]; Filter (Prop ("key", Eq, String key)) ] @ value
  ;;
end

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

let address_to_string : Anchor.Address.t -> string = function
  | Heading id -> "#" ^ id
  | Attr id -> "{#" ^ id ^ "}"
  | Caret id -> "^" ^ id
;;

let rec pred_to_string : pred -> string = function
  | Prop (prop, cmp, value) ->
    sprintf "%s %s %s" prop (cmp_to_string cmp) (Node.value_to_string value)
  | Has prop -> "has " ^ prop
  | Named address -> "named " ^ address_to_string address
  | Count (query, cmp, n) ->
    sprintf "count(%s) %s %d" (to_string query) (cmp_to_string cmp) n
  | And [] -> "true"
  | Or [] -> "false"
  | And preds -> String.concat ~sep:" and " (List.map preds ~f:grouped)
  | Or preds -> String.concat ~sep:" or " (List.map preds ~f:grouped)
  | Not pred -> "not " ^ grouped pred
  | Custom (label, _) -> label

and grouped (pred : pred) : string =
  match pred with
  | And _ | Or _ -> "(" ^ pred_to_string pred ^ ")"
  | _ -> pred_to_string pred

and step_to_string : step -> string = function
  | Children -> "children"
  | Section { nested = true } -> "section"
  | Section { nested = false } -> "section(direct)"
  | Filter pred -> "filter(" ^ pred_to_string pred ^ ")"
  | Nth n -> sprintf "nth(%d)" n
  | Each queries ->
    "each(" ^ String.concat ~sep:"; " (List.map queries ~f:to_string) ^ ")"
  | Recurse { steps; emit; descend } ->
    sprintf
      "recurse(%s; emit %s; descend %s)"
      (to_string steps)
      (pred_to_string emit)
      (pred_to_string descend)

and to_string : t -> string = function
  | [] -> "self"
  | query -> String.concat ~sep:" | " (List.map query ~f:step_to_string)
;;

(* Run
   --- *)

type stage =
  { step : step
  ; input : cursor list
  ; output : cursor list
  }

type result =
  { matches : cursor list
  ; stages : stage list
  }

let run (query : t) (start : cursor list) : result =
  let matches, stages =
    List.fold_map query ~init:start ~f:(fun input step ->
      let output = apply step input in
      output, { step; input; output })
  in
  { matches; stages }
;;

let listing (values : string list) : string =
  List.fold values ~init:[] ~f:(fun seen v ->
    if List.mem seen v ~equal:String.equal then seen else v :: seen)
  |> List.rev
  |> function
  | [] -> "none"
  | values -> String.concat values ~sep:", "
;;

let explain ({ step; input; output = _ } : stage) : string =
  let kinds () = listing (List.map input ~f:(fun cursor -> Node.kind (node cursor))) in
  let heading_ids () =
    List.filter_map input ~f:(fun cursor ->
      match node cursor with
      | Heading { id; _ } -> Some id
      | _ -> None)
  in
  let ids select =
    listing
      (List.concat_map input ~f:(fun cursor -> List.filter_map (names cursor) ~f:select))
  in
  match step with
  | Children | Recurse { emit = And []; _ } ->
    sprintf "nothing inside the selected blocks (%s)" (kinds ())
  | Recurse { emit; _ } ->
    sprintf
      "no block where %s inside the selected blocks (%s)"
      (pred_to_string emit)
      (kinds ())
  | Each _ -> sprintf "%s returned nothing from: %s" (step_to_string step) (kinds ())
  | Section _ ->
    (match heading_ids () with
     | [] -> sprintf "only a heading has a section; selected: %s" (kinds ())
     | ids -> sprintf "the section of %s is empty" (listing ids))
  | Filter (Prop (prop, _, _) as pred) ->
    (match List.filter_map input ~f:(fun cursor -> Node.prop prop (node cursor)) with
     | [] ->
       sprintf
         "no block where %s; none here has %s (kinds here: %s)"
         (pred_to_string pred)
         prop
         (kinds ())
     | values ->
       sprintf
         "no block where %s; %s here: %s"
         (pred_to_string pred)
         prop
         (listing (List.map values ~f:Node.value_to_string)))
  | Filter (Named (Heading id)) ->
    sprintf "no heading #%s; headings here: %s" id (listing (heading_ids ()))
  | Filter (Named (Attr id)) ->
    sprintf
      "no block named {#%s}; attribute ids here: %s"
      id
      (ids (function
         | Attr id -> Some id
         | Heading _ | Caret _ -> None))
  | Filter (Named (Caret id)) ->
    sprintf
      "no block named ^%s; caret ids here: %s"
      id
      (ids (function
         | Caret id -> Some id
         | Heading _ | Attr _ -> None))
  | Filter pred ->
    sprintf "no block where %s; kinds here: %s" (pred_to_string pred) (kinds ())
  | Nth n -> sprintf "no match number %d; %d matched before it" n (List.length input)
;;

let why_empty (result : result) : string option =
  if not (List.is_empty result.matches)
  then None
  else (
    match List.find result.stages ~f:(fun stage -> List.is_empty stage.output) with
    | Some ({ input = _ :: _; _ } as stage) -> Some (explain stage)
    | Some { input = []; _ } | None -> Some "the note has no blocks")
;;

(* Content
   ======= *)

let render (cursor : cursor) (cursors : cursor list) : string =
  let blocks =
    List.filter_map cursors ~f:(fun cursor ->
      match cursor.view with
      | Block block -> Some block
      | Item _ -> None)
  in
  let blank = B.Blank_line ("", Cmarkit.Meta.none) in
  Parse.commonmark_of_doc
    (Cmarkit.Doc.make
       ~defs:(Cmarkit.Doc.defs cursor.doc)
       (B.Blocks (List.intersperse blocks ~sep:blank, Cmarkit.Meta.none)))
;;

let content_string (cursor : cursor) : (string, string) Result.t =
  match node cursor with
  | Code_block { text; _ }
  | Math_block { text }
  | Html_block { text }
  | Raw_block { text; _ } -> Ok text
  | Heading _ -> Ok (render cursor (section ~nested:true cursor))
  | Callout _ | Block_quote | Div _ | Keyed _ | Footnote_definition _ | List_item _ ->
    Ok (render cursor (children cursor))
  | (Paragraph | List _ | Table | Definition_list | Thematic_break) as node ->
    Error (Node.kind node)
;;

let markdown (cursor : cursor) : string =
  match cursor.view with
  | Block _ -> render cursor [ cursor ]
  | Item _ -> render cursor (children cursor)
;;
