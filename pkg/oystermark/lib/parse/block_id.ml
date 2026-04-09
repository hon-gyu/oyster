(** {0 Obsidian caret block identifier}

  - Adds metadata only

  {1 Syntax}

  ```markdown
  (* Inline at end of paragraph *)
  Some paragraph text ^blockid

  (* Inline at end of list item *)
  - List item text ^blockid

  (* Separate line after block - references previous block *)
  | Table |
  | ----- |
  | Cell  |

  ^tableid

  (* Inline in nested list *)
  - Parent item ^parentid
      - Child item ^childid
  ```
*)
open Core

open Cmarkit

(** The type for block identifiers *)
type t =
  { id : string (** without the [^] prefix *)
  ; byte_pos : int
    (** The byte position of the start of the block identifier in the inline text. *)
  }
[@@deriving sexp]

let meta_key : t Cmarkit.Meta.key = Cmarkit.Meta.key ()

let is_valid_block_id (s : string) : bool =
  String.length s > 0
  && String.for_all s ~f:(fun c -> Char.is_alphanum c || Char.equal c '-')
  && Char.is_alphanum (String.get s 0)
;;

let make_opt (s : string) : t option =
  match String.rsplit2 s ~on:'^' with
  | None -> None
  | Some (text_before, ident_candidate) ->
    let ident_candidate_stripped = String.rstrip ident_candidate in
    if is_valid_block_id ident_candidate_stripped
    then Some { id = ident_candidate_stripped; byte_pos = String.length text_before }
    else None
;;

(** Extract block ID from the last text node of a paragraph's inline. *)
let extract_block_id_from_inline (inline : Cmarkit.Inline.t) : t option =
  (* Find and modify the last Text node in the inline tree *)
  let rec last_text = function
    | Cmarkit.Inline.Text (s, _meta) -> Some s
    | Cmarkit.Inline.Inlines (inlines, _meta) ->
      let rec try_last = function
        | [] -> None
        | [ x ] -> last_text x
        | _ :: rest -> try_last rest
      in
      try_last inlines
    | _ -> None
  in
  match last_text inline with
  | None -> None
  | Some s -> make_opt s
;;

(** Block mapper that attaches block IDs to paragraphs' metadata if they have one. *)
let block_map : Block.t Mapper.mapper =
  fun (mapper : Mapper.t) (block : Block.t) : Block.t Mapper.result ->
  match block with
  | Block.Paragraph (p, meta) ->
    let inline = Block.Paragraph.inline p in
    (match extract_block_id_from_inline inline with
     | None -> Mapper.default
     | Some block_id ->
       let meta_with_block_id = Meta.add meta_key block_id meta in
       Mapper.ret (Block.Paragraph (p, meta_with_block_id)))
  | _ -> Mapper.default
;;
