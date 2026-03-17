open Core
module Attribute = Parse.Attribute
module Frontmatter = Parse.Frontmatter

type cell =
  { id : int
    (** Unique code block id. Most of the time it will be the order of appearance in code blocks in the document *)
  ; lang : string option
  ; info : Attribute.t option
  ; content : string
  }
  [@@deriving sexp_of]

type output =
  { id : int
  ; res : [ `Html of string | `Markdown of string | `Error of string ]
  }
  [@@deriving sexp_of]

type exec_ctx =
  { config : Yaml.value
  ; inputs : cell list
  }

type exec_result = output list
type executor = exec_ctx -> exec_result

let todo () = failwith "TODO"

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
      let cell = {
        id = !block_id;
        lang = (cb_info |> Option.map ~f:(fun ci -> ci.lang));
        info = (cb_info |> Option.map ~f:(fun ci -> ci.attribute));
        content;
      } in
      incr block_id;
      Cmarkit.Folder.ret (cell :: acc)
    | _ -> Cmarkit.Folder.default
  in
  let folder = Cmarkit.Folder.make ~block_ext_default:(fun _f acc _b -> acc) ~block () in
  Cmarkit.Folder.fold_doc folder [] doc |> List.rev
;;

let%expect_test "extract_code_blocks" =
  let doc = Parse.of_string {|
Hi
```python
print("Hello")
```
.
```bash {.foo baz=zzz}
bar
```
|} in
  let cells = extract_code_blocks doc in
  print_s [%sexp (cells : cell list)];
  [%expect {|
    (((id 0) (lang ()) (info ()) (content "print(\"Hello\")"))
     ((id 1) (lang (bash)) (info (((id ()) (classes (.foo)) (kvs ((baz zzz))))))
      (content bar)))
    |}];
;;

let extract_exec_ctx (doc : Cmarkit.Doc.t) : exec_ctx =
  let config =
    match Parse.Frontmatter.of_doc doc with
    | Some (`O fields) ->
      (match List.Assoc.find fields ~equal:String.equal "oyster" with
       | Some (`O oys_fields) -> Yaml.(`O oys_fields)
       | Some _ -> failwith "Invalid frontmatter"
       | None -> Yaml.(`O []))
    | Some _ -> failwith "Invalid frontmatter"
    | None -> Yaml.(`O [])
  in
  let inputs = extract_code_blocks doc in
  {config; inputs}
;;

(* uv
==================== *)

type uv_config =
  { version : float
  ; dependencies : string list
  }

let default_uv_config = { version = 3.13; dependencies = [] }
let uv (default_config : uv_config) : executor = todo ()
