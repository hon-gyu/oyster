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

(** An explicit djot attribute id ([{#id}]) attached to an inline span or block.

    Unlike {!heading_entry} (matched by derived slug) and {!block_entry}
    (whole-paragraph [^caret] ids), an attribute anchor can pin an arbitrary
    inline sub-span, so [loc] may carry a column, not only a line.
    See {!page-"feature-attribute-anchors"}. *)
type attr_entry =
  { id : string
  ; loc : Cmarkit.Textloc.t option
  }

type file_entry =
  { rel_path : string
  ; headings : heading_entry list
  ; blocks : block_entry list
  ; attrs : attr_entry list
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
          (match Cmarkit.Block.Block_id.find meta with
           | Some (bid : Cmarkit.Block.Block_id.t) ->
             let loc =
               let tl = Cmarkit.Meta.textloc meta in
               if Cmarkit.Textloc.is_none tl then None else Some tl
             in
             Cmarkit.Folder.ret
               (acc @ [ ({ id = Cmarkit.Block.Block_id.id bid; loc } : block_entry) ])
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc
;;

(* Extract explicit djot attribute ids ([{#id}]) from a document.

   Attribute ids come from three AST sources (see {!page-"feature-attribute-anchors"}):
   - [Block.Ext_attributes]: a djot attribute line attached to a block.
   - [Inline.Ext_attributes]: [{#id}] attached to an inline span (column-precise).
   - a heading carrying an explicitly-supplied [`Id], distinct from its slug.

   The [Ext_attributes] wrappers are visited by the top-level folder callback and
   then recursed into manually, so ids nested inside an attributed block/span are
   still collected. *)
let extract_attr_ids (doc : Cmarkit.Doc.t) : attr_entry list =
  let loc_of_meta meta =
    let tl = Cmarkit.Meta.textloc meta in
    if Cmarkit.Textloc.is_none tl then None else Some tl
  in
  let add_id acc id meta = ({ id; loc = loc_of_meta meta } : attr_entry) :: acc in
  let add_attr acc attr meta =
    match Cmarkit.Attribute.id attr with
    | Some id -> add_id acc id meta
    | None -> acc
  in
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun f acc i ->
        match i with
        | Cmarkit.Inline.Ext_attributes (a, meta) ->
          let acc = add_attr acc (Cmarkit.Inline.Attributes.attributes a) meta in
          Cmarkit.Folder.ret
            (Cmarkit.Folder.fold_inline f acc (Cmarkit.Inline.Attributes.inline a))
        | _ -> Cmarkit.Folder.default)
      ~block:(fun f acc b ->
        match b with
        | Cmarkit.Block.Ext_attributes (a, meta) ->
          let acc = add_attr acc (Cmarkit.Block.Attributes.attributes a) meta in
          Cmarkit.Folder.ret
            (Cmarkit.Folder.fold_block f acc (Cmarkit.Block.Attributes.block a))
        | Cmarkit.Block.Heading (h, meta) ->
          (match Cmarkit.Block.Heading.id h with
           | Some (`Id id) -> Cmarkit.Folder.ret (add_id acc id meta)
           | Some (`Auto _) | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  List.rev (Cmarkit.Folder.fold_doc folder [] doc)
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
  let doc = Cmarkit.Doc.of_string ~strict:false ~block_id:true md in
  let block_ids = extract_block_ids doc in
  List.iter block_ids ~f:(fun (b : block_entry) -> Printf.printf "%s\n" b.id);
  [%expect
    {|
    para1
    block-2
    |}]
;;

let%expect_test "extract_attr_ids" =
  let md =
    {|
{#intro}
## Overview

The [key term]{#key-term} is defined here.

{#aside}
> A blockquote.

Plain paragraph, no id.
|}
  in
  let doc = Parse.of_string md in
  let attrs = extract_attr_ids doc in
  List.iter attrs ~f:(fun (a : attr_entry) ->
    let line =
      match a.loc with
      | Some tl -> Int.to_string (fst (Cmarkit.Textloc.first_line tl))
      | None -> "?"
    in
    Printf.printf "%s @ line %s\n" a.id line);
  [%expect
    {|
    intro @ line 3
    key-term @ line 5
    aside @ line 8
    |}]
;;
