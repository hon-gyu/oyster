(** Vault index: walks a vault directory and indexes files, headings, and block IDs. *)

open Core
open Parse

type heading_entry =
  { text : string
  ; level : int
  ; slug : string
    (** GitHub-style anchor: lowercase, punctuation stripped, deduped with [-1], [-2], etc. *)
  ; loc : Cmarkit.Textloc.t option
  }

type block_entry =
  { id : string
  ; loc : Cmarkit.Textloc.t option
  }

type file_entry =
  { rel_path : string
  ; headings : heading_entry list
  ; blocks : block_entry list
  }

type t =
  { files : file_entry list
  ; dirs : string list (** directory relative paths with trailing [/] *)
  }

(* Use Cmarkit.Folder to extract headings from a document.
   Reads slugs from heading block meta (stamped during parsing). *)
let extract_headings (doc : Cmarkit.Doc.t) : heading_entry list =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc block ->
        match block with
        | Cmarkit.Block.Heading (h, meta) ->
          let level = Cmarkit.Block.Heading.level h in
          let text = Heading_slug.inline_to_plain_text (Cmarkit.Block.Heading.inline h) in
          let slug =
            Cmarkit.Meta.find Heading_slug.meta_key meta
            |> Option.value_exn
                 ~message:
                   "heading missing slug meta — doc must be parsed via Parse.of_string"
          in
          let loc =
            let tl = Cmarkit.Meta.textloc meta in
            if Cmarkit.Textloc.is_none tl then None else Some tl
          in
          Cmarkit.Folder.ret (acc @ [ { text; level; slug; loc } ])
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc
;;

(* Use Cmarkit.Folder to extract block IDs from a document. *)
let extract_block_ids (doc : Cmarkit.Doc.t) : block_entry list =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc block ->
        match block with
        | Cmarkit.Block.Paragraph (_p, meta) ->
          (* TODO: block id itself points to the block that contains
             its previous sibling inline.
             We should get it.
             *)
          (match Cmarkit.Meta.find Block_id.meta_key meta with
           | Some (bid : Block_id.t) ->
             let loc =
               let tl = Cmarkit.Meta.textloc meta in
               if Cmarkit.Textloc.is_none tl then None else Some tl
             in
             Cmarkit.Folder.ret (acc @ [ { id = bid.id; loc } ])
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc
;;

(** Recursively list all entries (files and directories), returning relative
    paths.  Directories have a trailing [/].  Hidden entries are excluded. *)
let rec list_entries_recursive ~(root : string) ~(rel_prefix : string) : string list =
  let (entries : string list) =
    try Sys_unix.ls_dir root with
    | _ -> []
  in
  List.concat_map entries ~f:(fun name ->
    let is_hidden (name : string) : bool = String.is_prefix name ~prefix:"." in
    if is_hidden name
    then []
    else (
      let full_path = Filename.concat root name in
      let rel_path =
        if String.is_empty rel_prefix then name else Filename.concat rel_prefix name
      in
      match Sys_unix.is_directory full_path with
      | `Yes ->
        (rel_path ^ "/") :: list_entries_recursive ~root:full_path ~rel_prefix:rel_path
      | _ -> [ rel_path ]))
;;

let%expect_test "extract_headings" =
  let md =
    {|
# Title

## Chapter 1

### Section 1.1

## Chapter 2

#### Deep
|}
  in
  let doc = Parse.of_string md in
  let headings = extract_headings doc in
  List.iter headings ~f:(fun (h : heading_entry) ->
    Printf.printf "H%d: %s [%s]\n" h.level h.text h.slug);
  [%expect
    {|
    H1: Title [title]
    H2: Chapter 1 [chapter-1]
    H3: Section 1.1 [section-1-1]
    H2: Chapter 2 [chapter-2]
    H4: Deep [deep]
    |}]
;;

let%expect_test "extract_block_ids" =
  let md =
    {|
First paragraph ^para1

Second paragraph without block id

Third paragraph ^block-2
|}
  in
  let doc = Cmarkit.Doc.of_string ~strict:false md in
  (* Need to tag block_id mapper first *)
  let mapper =
    Cmarkit.Mapper.make
      ~inline_ext_default:(fun _m i -> Some i)
      ~block:Block_id.block_map
      ()
  in
  let doc = Cmarkit.Mapper.map_doc mapper doc in
  let block_ids = extract_block_ids doc in
  List.iter block_ids ~f:(fun (b : block_entry) -> Printf.printf "%s\n" b.id);
  [%expect
    {|
    para1
    block-2
    |}]
;;
