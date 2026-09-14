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

let view cursor = cursor.view

let path cursor =
  let rec up cursor acc =
    let acc = cursor.index :: acc in
    match cursor.parent with
    | None -> acc
    | Some parent -> up parent acc
  in
  up cursor []
;;

let kind_of_block (block : B.t) : string =
  match block with
  | B.Blank_line _ -> "blank_line"
  | B.Block_quote (_, meta) ->
    (match B.Callout.find meta with
     | Some _ -> "callout"
     | None -> "block_quote")
  | B.Blocks _ -> "blocks"
  | B.Code_block _ -> "code_block"
  | B.Heading _ -> "heading"
  | B.Html_block _ -> "html_block"
  | B.Link_reference_definition _ -> "link_reference_definition"
  | B.List _ -> "list"
  | B.Paragraph _ -> "paragraph"
  | B.Thematic_break _ -> "thematic_break"
  | B.Ext_attributes _ -> "attributes"
  | B.Ext_definition_list _ -> "definition_list"
  | B.Ext_div _ -> "div"
  | B.Ext_footnote_definition _ -> "footnote_definition"
  | B.Ext_jsx_block _ -> "jsx_block"
  | B.Ext_keyed _ -> "keyed"
  | B.Ext_math_block _ -> "math_block"
  | B.Ext_raw_block _ -> "raw_block"
  | B.Ext_table _ -> "table"
  | _ -> "unknown"
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
  | B.Blank_line _ | B.Link_reference_definition _ -> []
  | block -> if String.equal (kind_of_block block) "unknown" then [] else [ block ]
;;

let child_views (view : view) : view list =
  let blocks block = List.map (blocks_of block) ~f:(fun block -> Block block) in
  match view with
  | Item (item, _) -> blocks (B.List_item.block item)
  | Block (B.Block_quote (bq, _)) -> blocks (B.Block_quote.block bq)
  | Block (B.Ext_div (d, _)) -> blocks (B.Div.block d)
  | Block (B.Ext_keyed ((_label, body), _)) -> blocks body
  | Block (B.Ext_footnote_definition (fn, _)) -> blocks (B.Footnote.block fn)
  | Block (B.List (l, _)) -> List.map (B.List'.items l) ~f:(fun item -> Item item)
  | Block _ -> []
;;

let top doc =
  List.mapi
    (blocks_of (Cmarkit.Doc.block doc))
    ~f:(fun index block -> { doc; view = Block block; index; parent = None })
;;

let children cursor =
  List.mapi (child_views cursor.view) ~f:(fun index view ->
    { cursor with view; index; parent = Some cursor })
;;

let siblings cursor =
  match cursor.parent with
  | None -> top cursor.doc
  | Some parent -> children parent
;;

let rec descendants cursor =
  List.concat_map (children cursor) ~f:(fun child -> child :: descendants child)
;;

let heading_level (view : view) : int option =
  match view with
  | Block (B.Heading (h, _)) -> Some (B.Heading.level h)
  | Block _ | Item _ -> None
;;

let section ~nested cursor =
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

let kind cursor =
  match cursor.view with
  | Item _ -> "list_item"
  | Block block -> kind_of_block block
;;

let meta cursor =
  match cursor.view with
  | Block block -> Parse.Common.meta_of_block block
  | Item (_, meta) -> meta
;;

let info_string cursor =
  match cursor.view with
  | Block block -> Parse.Common.info_string_of_block block
  | Item _ -> None
;;

let heading_of (h : B.Heading.t) : Anchor.heading =
  { text = Parse.Common.inline_to_plain_text (B.Heading.inline h)
  ; level = B.Heading.level h
  ; slug = Option.value (Parse.Common.heading_id h) ~default:""
  }
;;

let headings cursor =
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
             match sibling.view with
             | Block (B.Heading (h, _)) when B.Heading.level h < lowest ->
               B.Heading.level h, heading_of h :: acc
             | _ -> lowest, acc)
    in
    match cursor.parent with
    | None -> here
    | Some parent -> enclosing parent @ here
  in
  enclosing cursor
;;

(** Each address of [doc] with the block {!Read.read} resolves it to. The walk
    looks through [Ext_attributes] wrappers, so the block is unwrapped to match.
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

let names cursor =
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

type pred =
  | Kind of string
  | Lang of string
  | Named of Anchor.Address.t

type step =
  | Children
  | Descendants
  | Descendants_or_self
  | Section of { nested : bool }
  | Filter of pred
  | Nth of int

type t = step list

type stage =
  { step : step
  ; input : cursor list
  ; output : cursor list
  }

type result =
  { matches : cursor list
  ; stages : stage list
  }

let holds pred cursor =
  match pred with
  | Kind wanted -> String.equal (kind cursor) wanted
  | Lang wanted -> Option.equal String.equal (info_string cursor) (Some wanted)
  | Named address -> List.mem (names cursor) address ~equal:Anchor.Address.equal
;;

let apply step input =
  match step with
  | Children -> List.concat_map input ~f:children
  | Descendants -> List.concat_map input ~f:descendants
  | Descendants_or_self -> List.concat_map input ~f:(fun c -> c :: descendants c)
  | Section { nested } -> List.concat_map input ~f:(section ~nested)
  | Filter pred -> List.filter input ~f:(holds pred)
  | Nth n -> Option.to_list (List.nth input (n - 1))
;;

let run query start =
  let matches, stages =
    List.fold_map query ~init:start ~f:(fun input step ->
      let output = apply step input in
      output, { step; input; output })
  in
  { matches; stages }
;;

let explain { step; input; output = _ } =
  let listing values =
    List.fold values ~init:[] ~f:(fun seen v ->
      if List.mem seen v ~equal:String.equal then seen else v :: seen)
    |> List.rev
    |> function
    | [] -> "none"
    | values -> String.concat values ~sep:", "
  in
  let heading_slugs () =
    List.filter_map input ~f:(fun cursor ->
      match cursor.view with
      | Block (B.Heading (h, _)) -> Some (heading_of h).slug
      | _ -> None)
  in
  let ids select =
    listing (List.concat_map input ~f:(fun c -> List.filter_map (names c) ~f:select))
  in
  match step with
  | Children | Descendants | Descendants_or_self ->
    sprintf "nothing inside the selected blocks (%s)" (listing (List.map input ~f:kind))
  | Section _ ->
    (match heading_slugs () with
     | [] ->
       sprintf
         "only a heading has a section; selected: %s"
         (listing (List.map input ~f:kind))
     | slugs -> sprintf "the section of %s is empty" (listing slugs))
  | Filter (Kind wanted) ->
    sprintf
      "no block of kind %s; kinds here: %s"
      wanted
      (listing (List.map input ~f:kind))
  | Filter (Lang wanted) ->
    sprintf
      "no code block with info string %s; info strings here: %s"
      wanted
      (listing (List.filter_map input ~f:info_string))
  | Filter (Named (Heading id)) ->
    sprintf "no heading #%s; headings here: %s" id (listing (heading_slugs ()))
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
  | Nth n -> sprintf "no match number %d; %d matched before it" n (List.length input)
;;

let why_empty result =
  if not (List.is_empty result.matches)
  then None
  else (
    match List.find result.stages ~f:(fun stage -> List.is_empty stage.output) with
    | Some ({ input = _ :: _; _ } as stage) -> Some (explain stage)
    | Some { input = []; _ } | None -> Some "the note has no blocks")
;;

(* Flags
   ----- *)

type flags =
  { under : string option
  ; direct : bool
  ; kind : string option
  ; lang : string option
  ; attr_id : string option
  ; caret_id : string option
  ; nth : int option
  }

let of_flags (flags : flags) : t =
  let scope =
    match flags.under with
    | None -> [ Descendants_or_self ]
    | Some heading ->
      [ Descendants_or_self
      ; Filter (Named (Heading (Parse.Common.heading_id_of_text heading)))
      ; Section { nested = not flags.direct }
      ; Descendants_or_self
      ]
  in
  let filter make value = Option.map value ~f:(fun v -> Filter (make v)) in
  scope
  @ List.filter_opt
      [ filter (fun k -> Kind k) flags.kind
      ; filter (fun l -> Lang l) flags.lang
      ; filter (fun id -> Named (Attr id)) flags.attr_id
      ; filter (fun id -> Named (Caret id)) flags.caret_id
      ; Option.map flags.nth ~f:(fun n -> Nth n)
      ]
;;

(* Content
   ======= *)

type content =
  | Literal of string
  | Markdown of B.t
  | Not_a_container

let content cursor =
  let code_lines cb =
    Literal
      (B.Code_block.code cb
       |> List.map ~f:Cmarkit.Block_line.to_string
       |> String.concat ~sep:"\n")
  in
  match cursor.view with
  | Item (item, _) -> Markdown (B.List_item.block item)
  | Block block ->
    (match block with
     | B.Code_block (cb, _) | B.Ext_math_block (cb, _) -> code_lines cb
     | B.Ext_raw_block (rb, _) -> code_lines (B.Raw_block.code_block rb)
     | B.Html_block (lines, _) ->
       Literal
         (lines |> List.map ~f:Cmarkit.Block_line.to_string |> String.concat ~sep:"\n")
     | B.Block_quote (bq, meta) ->
       let inner = B.Block_quote.block bq in
       (* Drop the callout's [ [!note] Title ] header line. *)
       (match B.Callout.find meta with
        | Some _ -> Markdown (B.Callout.strip_header inner)
        | None -> Markdown inner)
     | B.Ext_div (d, _) -> Markdown (B.Div.block d)
     | B.Ext_keyed ((_label, body), _) -> Markdown body
     | B.Ext_footnote_definition (fn, _) -> Markdown (B.Footnote.block fn)
     | B.Heading _ ->
       let blocks =
         List.filter_map (section ~nested:true cursor) ~f:(fun sibling ->
           match sibling.view with
           | Block block -> Some block
           | Item _ -> None)
       in
       let blank = B.Blank_line ("", Cmarkit.Meta.none) in
       Markdown (B.Blocks (List.intersperse blocks ~sep:blank, Cmarkit.Meta.none))
     | _ -> Not_a_container)
;;

let content_string cursor =
  match content cursor with
  | Literal text -> Ok text
  | Markdown block ->
    Ok
      (Parse.commonmark_of_doc
         (Cmarkit.Doc.make ~defs:(Cmarkit.Doc.defs cursor.doc) block))
  | Not_a_container -> Error (kind cursor)
;;
