(** {1 Pandoc code block attribute parsing}

    - Implements {!page-"pandoc-attribute"}
    - A {!t} will be attached to the metadata code block if it can be parsed out.
    - Only codeblock's metadata will be changed.

    The brace body is parsed with the fork's {!Cmarkit.Attribute} parser, so the
    accepted syntax matches Djot/inline-block attributes. Identifiers and classes
    are stored {e without} their [#] / [.] markers (matching {!Cmarkit.Attribute}).

    Note: we didn't really consider the number of spaces between cmark info string
    (lang) and attribute.
*)
open Core

open Common
open Cmarkit

(** A code-block attribute spec. Kept as a self-contained, [sexp]-able record
    (rather than the abstract {!Cmarkit.Attribute.t}) because the code executor
    serializes it for cache keys. *)
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

let of_oymarkit (a : Cmarkit.Attribute.t) : t =
  { id = Cmarkit.Attribute.id a
  ; classes = Cmarkit.Attribute.classes a
  ; kvs = Cmarkit.Attribute.key_values a
  }
;;

(** Parse the contents of a [{...}] specifier (without the braces). Returns
    [None] when the body is not a well-formed attribute spec. *)
let of_string (s : string) : t option =
  Option.map (Cmarkit.Attribute.of_string s) ~f:of_oymarkit
;;

let sexp_of_meta : Common.meta_sexp =
  fun meta ->
  Cmarkit.Meta.find meta_key meta
  |> Option.map ~f:(fun a -> Sexp.List [ Atom "attribute"; sexp_of_code_block_info a ])
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
             of_string inner
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

  let example_multiple_ids_override =
    {|```python {#myid #myid2 .class_a .class_b key1=val1 key2="val2"}
II
```|}
  ;;

  let example_with_attribute =
    {|```python {#myid .class_a .class_b key1=val1 key2="val2"}
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
    ; example_multiple_ids_override
    ; non_example_invalid_item
    ; non_example_no_info_string
    ]
  ;;
end

let%test_module "Doc" =
  (module struct
    open Common.For_test
    open For_test

    let doc_of_string s = mk_doc_of_string ~block:block_map () s
    let pp_doc ppf doc = mk_pp_doc ~metas:[ sexp_of_meta ] () ppf doc

    let%expect_test _ =
      let doc = doc_of_string example_no_attribute in
      [%test_result: int] (count_attr doc) ~expect:0;
      Format.printf "%a%!" pp_doc doc;
      [%expect
        {| ((Code_block python II) (meta (attribute ((lang python) (attribute ()))))) |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string example_with_attribute in
      [%test_result: int] (count_attr doc) ~expect:1;
      Format.printf "%a%!" pp_doc doc;
      [%expect
        {|
        ((Code_block "python {#myid .class_a .class_b key1=val1 key2=\"val2\"}" II)
          (meta
            (attribute
              ((lang python)
                (attribute
                  (((id (myid)) (classes (class_a class_b))
                     (kvs ((key1 val1) (key2 val2))))))))))
        |}]
    ;;

    (* The fork's attribute parser keeps the last id when several are given
       (Djot semantics), so multiple ids parse rather than being rejected. *)
    let%expect_test _ =
      let doc = doc_of_string example_multiple_ids_override in
      [%test_result: int] (count_attr doc) ~expect:1;
      Format.printf "%a%!" pp_doc doc;
      [%expect
        {|
        ((Code_block
           "python {#myid #myid2 .class_a .class_b key1=val1 key2=\"val2\"}" II)
          (meta
            (attribute
              ((lang python)
                (attribute
                  (((id (myid2)) (classes (class_a class_b))
                     (kvs ((key1 val1) (key2 val2))))))))))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_invalid_item in
      [%test_result: int] (count_attr doc) ~expect:0;
      Format.printf "%a%!" pp_doc doc;
      [%expect
        {|
        ((Code_block "python {#myid .class_a .class_b hi}" II)
          (meta (attribute ((lang python) (attribute ())))))
        |}]
    ;;

    let%expect_test _ =
      let doc = doc_of_string non_example_no_info_string in
      [%test_result: int] (count_attr doc) ~expect:0;
      Format.printf "%a%!" pp_doc doc;
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
