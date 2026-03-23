(** Core types and AST extraction shared by all code-executor sub-modules. *)

open Core
module Attribute = Parse.Attribute
module Frontmatter = Parse.Frontmatter

(** Config key for Oyster-specific frontmatter in frontmatter *)
let oyster_config_key = "oyster"

type cell =
  { id : int
    (** Unique code block id. Most of the time it will be the order of appearance in code blocks in the document *)
  ; lang : string option
  ; attr : Attribute.t option
  ; content : string
  }
[@@deriving sexp_of]

type output =
  { id : int
  ; res : [ `Html of string | `Markdown of string | `Error of string ]
    (** [`Error] is only used when
    nbconvert itself fails (process-level failure) *)
  }
[@@deriving sexp]

type exec_ctx =
  { config : Yaml.value
  ; inputs : cell list
  }

type executor = exec_ctx -> output list

(** Walk the Cmarkit AST and collect every fenced code block as a {!cell}.
    Cells are numbered in document order starting from 0.

    [lang] and [attr] are populated from the {!Attribute.code_block_info} attached to the
    block by {!Attribute.tag_cb_attr_meta} during parsing, which tags every fenced code
    block that has a non-empty info string. *)
let extract_code_blocks (doc : Cmarkit.Doc.t) : cell list =
  let block_id = ref 0 in
  let block _folder (acc : cell list) (b : Cmarkit.Block.t)
    : cell list Cmarkit.Folder.result
    =
    match b with
    | Cmarkit.Block.Code_block (cb, meta) ->
      let (content : string) =
        List.map (Cmarkit.Block.Code_block.code cb) ~f:Cmarkit.Block_line.to_string
        |> String.concat ~sep:"\n"
      in
      let cb_info = Cmarkit.Meta.find Attribute.meta_key meta in
      let cell =
        { id = !block_id
        ; lang = cb_info |> Option.map ~f:(fun ci -> ci.lang)
        ; attr = cb_info |> Option.bind ~f:(fun ci -> ci.attribute)
        ; content
        }
      in
      incr block_id;
      Cmarkit.Folder.ret (cell :: acc)
    | _ -> Cmarkit.Folder.default
  in
  let folder = Cmarkit.Folder.make ~block_ext_default:(fun _f acc _b -> acc) ~block () in
  Cmarkit.Folder.fold_doc folder [] doc |> List.rev
;;

(** Build an {!exec_ctx} from a parsed document.
    @return
    - [config]: the [oyster] mapping from the YAML frontmatter, or an empty
      mapping if absent. Executors read their own sub-keys from this value.
    - [inputs]: all code blocks in document.
    Dev note:
    - Blocks are collected via {!extract_code_blocks}  *)
let extract_exec_ctx (doc : Cmarkit.Doc.t) : exec_ctx =
  let config =
    match Parse.Frontmatter.of_doc doc with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal oyster_config_key with
       | Some (`O oys_fields) -> Yaml.(`O oys_fields)
       | Some _ -> failwith "Invalid frontmatter"
       | None -> Yaml.(`O []))
    | Some _ -> failwith "Invalid frontmatter"
    | None -> Yaml.(`O [])
  in
  let inputs = extract_code_blocks doc in
  { config; inputs }
;;

(** Filter [cells] down to cells matching [lang_filter],
    optionally restricted further by [attr_filter]. *)
let filter_cells
      ~(lang_filter : string -> bool)
      ~(attr_filter : Attribute.t option -> bool)
      (cells : cell list)
  : cell list
  =
  List.filter cells ~f:(fun cell ->
    match cell.lang with
    | Some l -> lang_filter l && attr_filter cell.attr
    | None -> false)
;;

(** Extract a session ID from a cell's attribute's [session_id] key.
  - Returns default session [ "1" ] if no attribute is present.
  - Returns default session [ "1" ] if no [session_id] key is found.
*)
let session_id_of_attr (attr_opt : Attribute.t option) : string =
  attr_opt
  |> Option.value_map ~default:"1" ~f:(fun attr ->
    List.Assoc.find attr.kvs ~equal:String.equal "session_id"
    |> Option.value_map ~default:"1" ~f:(fun session_id -> session_id))
;;

(** Filter and group cells by language and session ID.
    @param lang_filter filter cells by language
    @param attr_filter filter cells by attribute
    @param attr_session_map A function that extracts a session ID from a cell's attribute.
*)
let filter_group_cells
      ~(lang_filter : string -> bool)
      ~(attr_filter : Attribute.t option -> bool)
      ~(attr_session_map : Attribute.t option -> string)
      (cells : cell list)
  : (string * cell list) list
  =
  let groups : (string * cell list ref) list ref = ref [] in
  List.iter cells ~f:(fun cell ->
    match cell.lang with
    | Some l when lang_filter l && attr_filter cell.attr ->
      let session_id = attr_session_map cell.attr in
      (match List.Assoc.find !groups ~equal:String.equal session_id with
       | Some cells_ref -> cells_ref := !cells_ref @ [ cell ]
       | None -> groups := !groups @ [ session_id, ref [ cell ] ])
    | _ -> ());
  List.map !groups ~f:(fun (session_id, cells_ref) -> session_id, !cells_ref)
;;

let%expect_test "extract_code_blocks" =
  let doc =
    Parse.of_string
      {|
Hi
```python
print("Hello")
```
.
```bash {.foo baz=zzz}
bar
```
|}
  in
  let cells = extract_code_blocks doc in
  print_s [%sexp (cells : cell list)];
  [%expect
    {|
    (((id 0) (lang (python)) (attr ()) (content "print(\"Hello\")"))
     ((id 1) (lang (bash)) (attr (((id ()) (classes (.foo)) (kvs ((baz zzz))))))
      (content bar)))
    |}]
;;
