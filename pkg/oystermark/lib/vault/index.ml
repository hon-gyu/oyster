(** Vault index: walks a vault directory and indexes files, headings, and block IDs. *)

open Core
open Parse

type heading_entry =
  { text : string
  ; level : int
  ; slug : string (** GitHub-style anchor: lowercase, punctuation stripped, deduped with [-1], [-2], etc. *)
  }

type file_entry =
  { rel_path : string
  ; headings : heading_entry list
  ; block_ids : string list
  }

type t =
  { files : file_entry list
  ; dirs : string list (** directory relative paths with trailing [/] *)
  }

(** GitHub-style slug: lowercase, non-alphanum to [-], collapse runs, strip edges. *)
let slugify (s : string) : string =
  s
  |> String.lowercase
  |> String.map ~f:(fun c ->
    if Char.is_alphanum c || Char.equal c '-' || Char.equal c '_' then c else '-')
  |> String.split ~on:'-'
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> String.concat ~sep:"-"
;;

(** Compute a deduplicated slug. [seen] tracks base slug → count. *)
let dedup_slug (seen : (string, int) Hashtbl.t) (text : string) : string =
  let base : string = slugify text in
  let count : int = Hashtbl.find seen base |> Option.value ~default:0 in
  Hashtbl.set seen ~key:base ~data:(count + 1);
  if count = 0 then base else sprintf "%s-%d" base count
;;

(* Use Cmarkit.Folder to extract headings from a document. *)
let extract_headings (doc : Cmarkit.Doc.t) : heading_entry list =
  let folder =
    Cmarkit.Folder.make
      ~block:(fun _f acc block ->
        match block with
        | Cmarkit.Block.Heading (h, _meta) ->
          let level = Cmarkit.Block.Heading.level h in
          let inline = Cmarkit.Block.Heading.inline h in
          let text = inline_to_plain_text inline in
          Cmarkit.Folder.ret (acc @ [ (text, level) ])
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc _i -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  let raw : (string * int) list = Cmarkit.Folder.fold_doc folder [] doc in
  let seen = Hashtbl.create (module String) in
  List.map raw ~f:(fun (text, level) ->
    let slug = dedup_slug seen text in
    { text; level; slug })
;;

(* Use Cmarkit.Folder to extract block IDs from a document. *)
let extract_block_ids (doc : Cmarkit.Doc.t) : string list =
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
           | Some (bid : Block_id.t) -> Cmarkit.Folder.ret (acc @ [ bid.id ])
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
  let doc = Cmarkit.Doc.of_string ~strict:false md in
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
      ~block:Block_id.tag_block_id_meta
      ()
  in
  let doc = Cmarkit.Mapper.map_doc mapper doc in
  let block_ids = extract_block_ids doc in
  List.iter block_ids ~f:(fun id -> Printf.printf "%s\n" id);
  [%expect
    {|
    para1
    block-2
    |}]
;;
