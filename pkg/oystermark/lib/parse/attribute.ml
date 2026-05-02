(** {1 Generic attribute spec}

    The shared [{ id; classes; kvs }] record used by both Pandoc-style code
    block attributes ({!Cb_attribute}) and Djot-style block/inline
    attributes ({!Djot_attribute}).

    The parser implemented here is the simple space-separated form (as in
    Pandoc's reference syntax). It does not yet handle quoted values with
    embedded whitespace, [%comment%] sequences, or backslash escapes —
    those will be added when the Djot inline-attribute pass needs them.
*)
open Core

type t =
  { id : string option
  ; classes : string list
  ; kvs : (string * string) list
  }
[@@deriving sexp_of]

let empty = { id = None; classes = []; kvs = [] }

(** Stack two attribute specs. The right-hand id wins when both are
    present (matching the Djot rule "if multiple identifiers are given,
    the last one is used"). Classes accumulate; key/value pairs concat
    with later entries shadowing earlier ones at render time. *)
let merge (a : t) (b : t) : t =
  { id =
      (match b.id with
       | Some _ -> b.id
       | None -> a.id)
  ; classes = a.classes @ b.classes
  ; kvs = a.kvs @ b.kvs
  }
;;

let strip_paired_double_quotes (s : string) : string =
  if
    String.length s >= 2
    && String.is_prefix s ~prefix:"\""
    && String.is_suffix s ~suffix:"\""
  then String.sub s ~pos:1 ~len:(String.length s - 2)
  else s
;;

(** Parse the contents of a [{...}] specifier (without the braces).

    Items are whitespace-separated. Recognised forms:
    - [#foo] — identifier
    - [.foo] — class
    - [key=value] or [key="value"] — key/value pair

    Returns [Error] if there are multiple ids or an unrecognised item.
*)
let of_string_or_error (s : string) : (t, Error.t) result =
  let err_msg = ref None in
  let (items : string list) =
    String.split_on_chars ~on:[ ' '; '\t'; '\n' ] s
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  let ids = List.filter items ~f:(fun s -> String.is_prefix s ~prefix:"#") in
  if List.length ids > 1
  then (
    let msg = sprintf "Too many ids: %s" (String.concat ~sep:" " ids) in
    err_msg := Some msg);
  let id = List.hd ids in
  let classes = List.filter items ~f:(fun s -> String.is_prefix s ~prefix:".") in
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

let%test_module "parse" =
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

    let%expect_test "multiple ids" =
      parse {|#myid #myid2 .class_a|};
      [%expect {| (Error "Too many ids: #myid #myid2") |}]
    ;;

    let%expect_test "invalid item" =
      parse {|#myid .class_a hi|};
      [%expect {| (Error "Invalid attributes: hi") |}]
    ;;

    let%expect_test "newline-separated (multi-line attrs)" =
      (* Newlines are accepted as separators, but quoted values containing
         whitespace are not yet handled — they get split. *)
      parse "#foo\n.bar .baz key=val";
      [%expect {| (Ok ((id (#foo)) (classes (.bar .baz)) (kvs ((key val))))) |}]
    ;;

    let%expect_test "quoted value with spaces (current limitation)" =
      parse {|key="my value"|};
      [%expect {| (Error "Invalid attributes: value\"") |}]
    ;;
  end)
;;
