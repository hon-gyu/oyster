(** {1 Generic attribute spec}

    The shared [{ id; classes; kvs }] record used by both Pandoc-style code
    block attributes ({!Cb_attribute}) and Djot-style block/inline
    attributes ({!Djot_attribute}).

    Recognised item forms inside [{...}] (without the braces):
    - [#foo] — identifier (stored with the leading [#])
    - [.foo] — class (stored with the leading [.])
    - [key=value] — bare value; runs until the next whitespace
    - [key="value"] — quoted value; may contain whitespace and supports
      backslash escapes (a backslash followed by any character is
      replaced by that character verbatim)

    Items are separated by whitespace (spaces, tabs, newlines). Djot's
    [%comment%] syntax is not supported and will be reported as an
    invalid attribute.
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

(** Tokenise the contents of a [{...}] specifier into whitespace-separated
    items. A double-quoted run is treated as a single token, with backslash
    escapes decoded inside it ([\\c] -> [c]); the surrounding quotes are
    preserved on the returned token so callers can tell quoted from bare. *)
let tokenize (s : string) : (string list, string) result =
  let len = String.length s in
  let tokens = ref [] in
  let buf = Buffer.create 16 in
  let in_token = ref false in
  let flush () =
    if !in_token
    then (
      tokens := Buffer.contents buf :: !tokens;
      Buffer.clear buf;
      in_token := false)
  in
  let i = ref 0 in
  let err = ref None in
  while !i < len && Option.is_none !err do
    let c = s.[!i] in
    match c with
    | ' ' | '\t' | '\n' | '\r' ->
      flush ();
      incr i
    | '"' ->
      in_token := true;
      Buffer.add_char buf '"';
      incr i;
      let closed = ref false in
      while (not !closed) && !i < len && Option.is_none !err do
        match s.[!i] with
        | '"' ->
          Buffer.add_char buf '"';
          incr i;
          closed := true
        | '\\' when !i + 1 < len ->
          Buffer.add_char buf s.[!i + 1];
          i := !i + 2
        | ch ->
          Buffer.add_char buf ch;
          incr i
      done;
      if not !closed then err := Some "Unterminated quoted value"
    | _ ->
      in_token := true;
      Buffer.add_char buf c;
      incr i
  done;
  flush ();
  match !err with
  | Some msg -> Error msg
  | None -> Ok (List.rev !tokens)
;;

(** Strip the surrounding double quotes from a token produced by
    {!tokenize}. Bare tokens are returned unchanged. *)
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

    Returns [Error] if a quoted value is unterminated, there are multiple
    ids, or an item is unrecognised.
*)
let of_string_or_error (s : string) : (t, Error.t) result =
  let err_msg = ref None in
  match tokenize s with
  | Error msg -> Error (Error.of_string msg)
  | Ok items ->
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
    (match !err_msg with
     | Some msg -> Error (Error.of_string msg)
     | None -> Ok { id; classes; kvs })
;;

let of_string_exn (s : string) : t =
  match of_string_or_error s with
  | Ok t -> t
  | Error e -> raise (Error.to_exn e)
;;

(** Serialize a value for [key=...] output. Bare (unquoted) iff the value
    contains only ASCII alphanumerics, underscore, colon, or hyphen.
    Otherwise emit double-quoted, with backslashes and quotes inside escaped. *)
let serialize_value (v : string) : string =
  let bare_ok c =
    Char.is_alphanum c || Char.equal c '_' || Char.equal c ':' || Char.equal c '-'
  in
  if (not (String.is_empty v)) && String.for_all v ~f:bare_ok
  then v
  else (
    let buf = Buffer.create (String.length v + 2) in
    Buffer.add_char buf '"';
    String.iter v ~f:(fun c ->
      match c with
      | '\\' | '"' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
      | _ -> Buffer.add_char buf c);
    Buffer.add_char buf '"';
    Buffer.contents buf)
;;

(** Serialize an [Oy_attribute.t] back to brace-content syntax (without
    surrounding [{}]). Order: id, classes, kvs — matching how most
    rendered output reads. *)
let to_string (t : t) : string =
  let parts = [] in
  let parts =
    match t.id with
    | Some id -> id :: parts
    | None -> parts
  in
  let parts = parts @ t.classes in
  let parts =
    parts @ List.map t.kvs ~f:(fun (k, v) -> k ^ "=" ^ serialize_value v)
  in
  String.concat ~sep:" " parts
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
      parse "#foo\n.bar .baz key=val";
      [%expect {| (Ok ((id (#foo)) (classes (.bar .baz)) (kvs ((key val))))) |}]
    ;;

    let%expect_test "quoted value with spaces" =
      parse {|key="my value"|};
      [%expect {| (Ok ((id ()) (classes ()) (kvs ((key "my value"))))) |}]
    ;;

    let%expect_test "quoted value with backslash escapes" =
      parse {|key="a \"quoted\" \\ word"|};
      [%expect {| (Ok ((id ()) (classes ()) (kvs ((key "a \"quoted\" \\ word"))))) |}]
    ;;

    let%expect_test "unterminated quoted value" =
      parse {|key="oops|};
      [%expect {| (Error "Unterminated quoted value") |}]
    ;;

    let%expect_test "quoted value mixed with other items" =
      parse {|#myid .cls key="hello world" k2=bare|};
      [%expect
        {| (Ok ((id (#myid)) (classes (.cls)) (kvs ((key "hello world") (k2 bare))))) |}]
    ;;
  end)
;;
