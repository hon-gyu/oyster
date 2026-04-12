(** {1 Pandoc code block attribute parsing}

    - Implements {!page-"pandoc-attribute"}
    - Attribute will be attached to the metadata code block if it can be parsed out.
    - Only codeblock's metadata will be changed.

    Note: we didn't really consider the number of spaces between cmark info string (lang) and attribute.
*)
open Core

open Common
open Cmarkit

type t =
  { id : string option
  ; classes : string list
  ; kvs : (string * string) list
  }
[@@deriving sexp_of]

type code_block_info =
  { lang : string
    (** Cmarkit code block info string as in {{:https://spec.commonmark.org/0.31.2/#info-string}info string} *)
  ; attribute : t option (** Pandoc attribute *)
  }
[@@deriving sexp_of]

let empty = { id = None; classes = []; kvs = [] }
let meta_key : code_block_info Cmarkit.Meta.key = Cmarkit.Meta.key ()

let sexp_of_meta : Common.meta_sexp =
  fun meta ->
  Cmarkit.Meta.find meta_key meta
  |> Option.map ~f:(fun a -> Sexp.List [ Atom "attribute"; sexp_of_code_block_info a ])
;;

let strip_paired_double_quotes (s : string) : string =
  if
    String.length s >= 2
    && String.is_prefix s ~prefix:"\""
    && String.is_suffix s ~suffix:"\""
  then String.sub s ~pos:1 ~len:(String.length s - 2)
  else s
;;

let of_string_or_error (s : string) : (t, Error.t) result =
  let err_msg = ref None in
  let (items : string list) =
    String.split ~on:' ' s |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  (* id *)
  let ids = List.filter items ~f:(fun s -> String.is_prefix s ~prefix:"#") in
  if List.length ids > 1
  then (
    let msg = sprintf "Too many ids: %s" (String.concat ~sep:" " ids) in
    err_msg := Some msg);
  let id = List.hd ids in
  (* class *)
  let classes = List.filter items ~f:(fun s -> String.is_prefix s ~prefix:".") in
  (* kv *)
  let (kv_candidates : string list) =
    List.filter items ~f:(fun s ->
      (not (String.is_prefix s ~prefix:"#")) && not (String.is_prefix s ~prefix:"."))
  in
  let invalid_attrs = ref [] in
  let (kvs : (string * string) list) =
    List.filter_map kv_candidates ~f:(fun kv ->
      match String.lsplit2 ~on:'=' kv with
      | Some (key, value) -> Some (key, strip_paired_double_quotes value)
      | None ->
        invalid_attrs := kv :: !invalid_attrs;
        None)
  in
  if not (List.is_empty !invalid_attrs)
  then err_msg := Some ("Invalid attributes: " ^ String.concat ~sep:", " !invalid_attrs);
  match !err_msg with
  | Some msg -> Error (Error.of_string msg)
  | None -> Ok { id; classes; kvs }
;;

let of_string_exn (s : string) : t =
  match of_string_or_error s with
  | Ok t -> t
  | Error e -> raise (Error.to_exn e)
;;

(** Attach a {!code_block_info} to the meta of any fenced code block that has a lang.

    Handles three info-string shapes:
    - ["lang {attr}"] — lang + Pandoc attribute block → [attribute = Some t]
    - ["lang"]        — lang only, no attribute block  → [attribute = None]
    - ["lang other"]  — lang + non-attribute suffix    → [attribute = None] (suffix ignored)

    Fences with no info string, or whose info string starts with ['{'] (attribute-only,
    no lang), are left untagged — the outer [code_block_info option] in the meta stays
    [None] for those blocks.

    Invariant: whenever a [code_block_info] is attached, [lang] is a non-empty string.
    Callers can therefore match on [Meta.find meta_key meta] and rely on [lang] always
    being meaningful — there is no need for [lang : string option]. *)
let block_map : Block.t Mapper.mapper =
  fun (mapper : Mapper.t) (b : Block.t) : Block.t Mapper.result ->
  match b with
  | Cmarkit.Block.Code_block (cb, cb_meta) ->
    (match Block.Code_block.info_string cb with
     | None -> Mapper.default
     | Some (info, _) ->
       let info' = String.strip info in
       (* A fence whose info starts with '{' has attributes but no lang tag. *)
       if String.is_empty info' || String.is_prefix info' ~prefix:"{"
       then Mapper.default
       else (
         let lang, rest =
           match String.lsplit2 ~on:' ' info' with
           | Some (l, r) -> l, Some (String.strip r)
           | None -> info', None
         in
         let attribute =
           match rest with
           | Some attr_str
             when String.is_prefix attr_str ~prefix:"{"
                  && String.is_suffix attr_str ~suffix:"}" ->
             let inner = String.sub attr_str ~pos:1 ~len:(String.length attr_str - 2) in
             (match of_string_or_error inner with
              | Ok attr -> Some attr
              | Error _ -> None)
           | _ -> None
         in
         let new_meta = cb_meta |> Meta.add meta_key { lang; attribute } in
         Mapper.ret (Cmarkit.Block.Code_block (cb, new_meta))))
  | _ -> Mapper.default
;;

module For_test = struct
  (** Count code blocks that have a non-[None] attribute parsed *)
  let count_attr (doc : Cmarkit.Doc.t) : int =
    let folder =
      Cmarkit.Folder.make
        ~block:(fun _f acc -> function
           | Cmarkit.Block.Code_block (_, meta) ->
             (match Cmarkit.Meta.find meta_key meta with
              | Some { attribute = Some _; _ } -> Cmarkit.Folder.ret (acc + 1)
              | _ -> Cmarkit.Folder.default)
           | _ -> Cmarkit.Folder.default)
        ()
    in
    Cmarkit.Folder.fold_doc folder 0 doc
  ;;

  let example_no_attribute =
    {|```python
II
```|}
  ;;

  let example_with_attribute =
    {|```python {#myid .class_a .class_b key1=val1 key2="val2"}
II
```|}
  ;;

  let non_example_invalid_multiple_ids =
    {|```python {#myid #myid2 .class_a .class_b key1=val1 key2="val2"}
II
```|}
  ;;

  let non_example_invalid_item =
    {|```python {#myid .class_a .class_b hi}
II
```|}
  ;;

  let non_example_no_info_string =
    {|```{#myid .class_a .class_b}
II
```|}
  ;;

  let examples =
    [ example_no_attribute
    ; example_with_attribute
    ; non_example_invalid_multiple_ids
    ; non_example_invalid_item
    ; non_example_no_info_string
    ]
  ;;
end

let%test_module "parse attribute" =
  (module struct
    let parse (s : string) : unit =
      let info = of_string_or_error s in
      print_s @@ Or_error.sexp_of_t sexp_of_t info
    ;;

    let%expect_test "good" =
      parse {|#myid .class_a .class_b key1=val1 key2="val2"|};
      [%expect
        {|
        (Ok
         ((id (#myid)) (classes (.class_a .class_b)) (kvs ((key1 val1) (key2 val2)))))
        |}]
    ;;

    let%expect_test "invalid attribute: multiple ids" =
      parse {|#myid #myid2 .class_a .class_b key1=val1 key2="val2"|};
      [%expect {| (Error "Too many ids: #myid #myid2") |}]
    ;;

    let%expect_test "invalid attribute: invalid syntax" =
      parse {|#myid .class_a .class_b key1=val1 key2="val2" hi|};
      [%expect {| (Error "Invalid attributes: hi") |}]
    ;;
  end)
;;

let%test_module "Doc" =
  (module struct
    open Common.For_test
    open For_test

    let doc_of_string s = mk_doc_of_string ~block:block_map () s
    let pp_doc doc = mk_pp_doc ~metas:[ sexp_of_meta ] () doc

    let%expect_test _ =
      let doc = doc_of_string example_no_attribute in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {| ((Code_block python II) (meta (attribute ((lang python) (attribute ()))))) |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_with_attribute in
      [%test_result: int] (count_attr doc) ~expect:1;
      pp_doc doc;
      [%expect
        {|
        ((Code_block "python {#myid .class_a .class_b key1=val1 key2=\"val2\"}" II)
          (meta
            (attribute
              ((lang python)
                (attribute
                  (((id (#myid)) (classes (.class_a .class_b))
                     (kvs ((key1 val1) (key2 val2))))))))))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_invalid_multiple_ids in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        ((Code_block
           "python {#myid #myid2 .class_a .class_b key1=val1 key2=\"val2\"}" II)
          (meta (attribute ((lang python) (attribute ())))))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_invalid_item in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect
        {|
        ((Code_block "python {#myid .class_a .class_b hi}" II)
          (meta (attribute ((lang python) (attribute ())))))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_no_info_string in
      [%test_result: int] (count_attr doc) ~expect:0;
      pp_doc doc;
      [%expect {| (Code_block "{#myid .class_a .class_b}" II) |}]
    ;;

    let%test_unit "roundtrip: commonmark output is idempotent" =
      let commonmark_of_doc =
        Cmarkit_renderer.doc_to_string (Cmarkit_commonmark.renderer ())
      in
      List.iter
        examples
        ~f:(commonmark_of_doc_idempotent ~doc_of_string ~commonmark_of_doc)
    ;;
  end)
;;
