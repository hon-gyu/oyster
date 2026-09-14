open Core

(* Walk
   ==== *)

type located_block =
  { block : Cmarkit.Block.t
  ; index : int
  ; addresses : Anchor.Address.t list
  ; heading_path : string list
  ; heading_text : string list
  }

let kind_of_block (block : Cmarkit.Block.t) : string =
  let open Cmarkit in
  match block with
  | Block.Blank_line _ -> "blank_line"
  | Block.Block_quote (_, meta) ->
    (match Block.Callout.find meta with
     | Some _ -> "callout"
     | None -> "block_quote")
  | Block.Blocks _ -> "blocks"
  | Block.Code_block _ -> "code_block"
  | Block.Heading _ -> "heading"
  | Block.Html_block _ -> "html_block"
  | Block.Link_reference_definition _ -> "link_reference_definition"
  | Block.List _ -> "list"
  | Block.Paragraph _ -> "paragraph"
  | Block.Thematic_break _ -> "thematic_break"
  | Block.Ext_attributes _ -> "attributes"
  | Block.Ext_definition_list _ -> "definition_list"
  | Block.Ext_div _ -> "div"
  | Block.Ext_footnote_definition _ -> "footnote_definition"
  | Block.Ext_jsx_block _ -> "jsx_block"
  | Block.Ext_keyed _ -> "keyed"
  | Block.Ext_math_block _ -> "math_block"
  | Block.Ext_raw_block _ -> "raw_block"
  | Block.Ext_table _ -> "table"
  | _ -> "unknown"
;;

let rec unwrap_attributes (block : Cmarkit.Block.t) : Cmarkit.Block.t =
  match block with
  | Cmarkit.Block.Ext_attributes (a, _) ->
    unwrap_attributes (Cmarkit.Block.Attributes.block a)
  | block -> block
;;

(** Each caret and attribute address of [blocks], paired with the block
    {!Read.read} resolves it to. The walk skips [Ext_attributes] wrappers, so the
    block is unwrapped to match. *)
let named_blocks (blocks : Cmarkit.Block.t list)
  : (Cmarkit.Block.t * Anchor.Address.t) list
  =
  Anchor.of_doc (Cmarkit.Doc.make (Cmarkit.Block.Blocks (blocks, Cmarkit.Meta.none)))
  |> List.filter_map ~f:(fun (anchor : Anchor.t) ->
    match anchor.value with
    | Heading _ -> None
    | Caret _ | Attr _ ->
      let address = Anchor.address anchor.value in
      List.hd (Read.read blocks address)
      |> Option.map ~f:(fun block -> unwrap_attributes block, address))
;;

let walk (blocks : Cmarkit.Block.t list) : located_block list =
  let open Cmarkit in
  let named = named_blocks blocks in
  let next_index =
    let count = ref 0 in
    fun () ->
      incr count;
      !count
  in
  let emit ~headings block =
    { block
    ; index = next_index ()
    ; addresses =
        List.filter_map named ~f:(fun (named, address) ->
          Option.some_if (phys_equal named block) address)
        |> List.dedup_and_sort ~compare:Anchor.Address.compare
    ; heading_path = List.rev_map headings ~f:(fun (_level, id, _text) -> id)
    ; heading_text = List.rev_map headings ~f:(fun (_level, _id, text) -> text)
    }
  in
  let acc = ref [] in
  let push located = acc := located :: !acc in
  (* [headings] is the enclosing heading stack, innermost first, as
     (level, id, text). *)
  let rec siblings ~headings blocks =
    List.fold blocks ~init:headings ~f:(fun headings block -> visit ~headings block)
  and visit ~headings block =
    match block with
    | Block.Blank_line _ | Block.Link_reference_definition _ -> headings
    | Block.Blocks (bs, _) -> siblings ~headings bs
    | Block.Ext_attributes (a, _) -> visit ~headings (Block.Attributes.block a)
    | Block.Heading (h, _) ->
      let level = Block.Heading.level h in
      let headings =
        List.drop_while headings ~f:(fun (enclosing, _, _) -> enclosing >= level)
      in
      push (emit ~headings block);
      let id = Option.value (Parse.Common.heading_id h) ~default:"" in
      let text = Parse.Common.inline_to_plain_text (Block.Heading.inline h) in
      (level, id, text) :: headings
    | Block.Block_quote (bq, _) ->
      push (emit ~headings block);
      ignore (siblings ~headings [ Block.Block_quote.block bq ] : _ list);
      headings
    | Block.Ext_div (d, _) ->
      push (emit ~headings block);
      ignore (siblings ~headings [ Block.Div.block d ] : _ list);
      headings
    | Block.Ext_keyed ((_label, body), _) ->
      push (emit ~headings block);
      ignore (siblings ~headings [ body ] : _ list);
      headings
    | Block.Ext_footnote_definition (fn, _) ->
      push (emit ~headings block);
      ignore (siblings ~headings [ Block.Footnote.block fn ] : _ list);
      headings
    | Block.List (l, _) ->
      push (emit ~headings block);
      List.iter (Block.List'.items l) ~f:(fun (item, _meta) ->
        ignore (siblings ~headings [ Block.List_item.block item ] : _ list));
      headings
    | block ->
      (* Skip kinds this module does not know, such as frontmatter. *)
      if String.equal (kind_of_block block) "unknown"
      then headings
      else (
        push (emit ~headings block);
        headings)
  in
  ignore (siblings ~headings:[] blocks : _ list);
  List.rev !acc
;;

(* Query
   ===== *)

type t =
  { under : string option
  ; direct : bool
  ; kind : string option
  ; lang : string option
  ; attr_id : string option
  ; caret_id : string option
  ; nth : int option
  }

let run (query : t) (blocks : Cmarkit.Block.t list) : located_block list =
  let matches wanted actual =
    match wanted, actual with
    | None, _ -> true
    | Some wanted, Some actual -> String.equal wanted actual
    | Some _, None -> false
  in
  let is_under (located : located_block) =
    match query.under with
    | None -> true
    | Some wanted ->
      let wanted = Parse.Common.heading_id_of_text wanted in
      if query.direct
      then matches (Some wanted) (List.last located.heading_path)
      else List.mem located.heading_path wanted ~equal:String.equal
  in
  let is_named wanted (located : located_block) =
    Option.for_all wanted ~f:(fun address ->
      List.mem located.addresses address ~equal:Anchor.Address.equal)
  in
  let kept =
    walk blocks
    |> List.filter ~f:(fun located ->
      is_under located
      && matches query.kind (Some (kind_of_block located.block))
      && matches query.lang (Parse.Common.info_string_of_block located.block)
      && is_named (Option.map query.attr_id ~f:(fun id -> Anchor.Address.Attr id)) located
      && is_named
           (Option.map query.caret_id ~f:(fun id -> Anchor.Address.Caret id))
           located)
  in
  match query.nth with
  | None -> kept
  | Some n -> Option.to_list (List.nth kept (n - 1))
;;

(* Content
   ======= *)

type content =
  | Literal of string
  | Markdown of Cmarkit.Block.t
  | Not_a_container

let content_of_located_block (located : located_block) : content =
  let open Cmarkit in
  let code_lines cb =
    Literal
      (Block.Code_block.code cb
       |> List.map ~f:Block_line.to_string
       |> String.concat ~sep:"\n")
  in
  match located.block with
  | Block.Code_block (cb, _) | Block.Ext_math_block (cb, _) -> code_lines cb
  | Block.Ext_raw_block (rb, _) -> code_lines (Block.Raw_block.code_block rb)
  | Block.Html_block (lines, _) ->
    Literal (lines |> List.map ~f:Block_line.to_string |> String.concat ~sep:"\n")
  | Block.Block_quote (bq, meta) ->
    let inner = Block.Block_quote.block bq in
    (* Drop the callout's [ [!note] Title ] header line. *)
    (match Block.Callout.find meta with
     | Some _ -> Markdown (Block.Callout.strip_header inner)
     | None -> Markdown inner)
  | Block.Ext_div (d, _) -> Markdown (Block.Div.block d)
  | Block.Ext_keyed ((_label, body), _) -> Markdown body
  | Block.Ext_footnote_definition (fn, _) -> Markdown (Block.Footnote.block fn)
  | _ -> Not_a_container
;;

let content_string ~(defs : Cmarkit.Label.defs) (located : located_block)
  : (string, string) Result.t
  =
  match content_of_located_block located with
  | Literal text -> Ok text
  | Markdown block -> Ok (Cmarkit_commonmark.of_doc (Cmarkit.Doc.make ~defs block))
  | Not_a_container -> Error (kind_of_block located.block)
;;
