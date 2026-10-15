open Core

type comparison =
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | In
  | Out
[@@deriving sexp, equal, compare]

type t =
  | Compare of
      { selector : string
      ; comparison : comparison
      ; arguments : string list
      }
  | And of t list
  | Or of t list
[@@deriving sexp, equal, compare]

(* Syntax
   ====== *)

exception Syntax of string

let is_reserved = function
  | '"' | '\'' | '(' | ')' | ';' | ',' | '=' | '!' | '~' | '<' | '>' -> true
  | c -> Char.is_whitespace c
;;

let of_string (s : string) : (t, string) result =
  let len = String.length s in
  let pos = ref 0 in
  let fail fmt = ksprintf (fun message -> raise (Syntax message)) fmt in
  let peek () = if !pos < len then Some s.[!pos] else None in
  let skip_blanks () =
    while !pos < len && Char.is_whitespace s.[!pos] do
      incr pos
    done
  in
  let found () =
    match peek () with
    | None -> "the end of the query"
    | Some c -> sprintf "`%c`" c
  in
  let unreserved () =
    let start = !pos in
    while !pos < len && not (is_reserved s.[!pos]) do
      incr pos
    done;
    String.sub s ~pos:start ~len:(!pos - start)
  in
  let selector () =
    skip_blanks ();
    match unreserved () with
    | "" -> fail "expected a field name, found %s" (found ())
    | name -> name
  in
  let comparison selector =
    skip_blanks ();
    let rest = String.drop_prefix s !pos in
    let symbolic = [ "==", Eq; "!=", Neq; "<=", Le; ">=", Ge; "<", Lt; ">", Gt ] in
    match List.find symbolic ~f:(fun (op, _) -> String.is_prefix rest ~prefix:op) with
    | Some (op, comparison) ->
      pos := !pos + String.length op;
      comparison
    | None ->
      if not (Option.equal Char.equal (peek ()) (Some '='))
      then fail "expected a comparison operator after `%s`, found %s" selector (found ());
      incr pos;
      let start = !pos in
      while !pos < len && Char.is_alpha s.[!pos] do
        incr pos
      done;
      let name = String.sub s ~pos:start ~len:(!pos - start) in
      if String.is_empty name || not (Option.equal Char.equal (peek ()) (Some '='))
      then fail "expected a comparison operator after `%s`" selector;
      incr pos;
      (match name with
       | "lt" -> Lt
       | "le" -> Le
       | "gt" -> Gt
       | "ge" -> Ge
       | "in" -> In
       | "out" -> Out
       | _ -> fail "unknown comparison operator `=%s=`" name)
  in
  let value () =
    skip_blanks ();
    match peek () with
    | Some (('"' | '\'') as quote) ->
      incr pos;
      let buf = Buffer.create 16 in
      let rec loop () =
        match peek () with
        | None ->
          fail
            "unterminated quoted value (a query value cannot contain `#` or `|`, even \
             quoted)"
        | Some c when Char.equal c quote -> incr pos
        | Some '\\' when !pos + 1 < len ->
          Buffer.add_char buf s.[!pos + 1];
          pos := !pos + 2;
          loop ()
        | Some c ->
          Buffer.add_char buf c;
          incr pos;
          loop ()
      in
      loop ();
      Buffer.contents buf
    | _ ->
      (match unreserved () with
       | "" -> fail "expected a value, found %s" (found ())
       | v -> v)
  in
  let arguments comparison =
    skip_blanks ();
    match peek () with
    | Some '(' ->
      incr pos;
      let rec loop acc =
        skip_blanks ();
        match peek () with
        | Some ',' ->
          incr pos;
          loop (value () :: acc)
        | Some ')' ->
          incr pos;
          List.rev acc
        | _ -> fail "expected `,` or `)`, found %s" (found ())
      in
      let arguments = loop [ value () ] in
      (match comparison, arguments with
       | (In | Out), _ | _, [ _ ] -> arguments
       | _ -> fail "only `=in=` and `=out=` take a list of values")
    | _ -> [ value () ]
  in
  (* [operands separator operand] is [operand (separator operand)*]. *)
  let operands separator operand =
    let rec loop acc =
      skip_blanks ();
      if Option.equal Char.equal (peek ()) (Some separator)
      then (
        incr pos;
        loop (operand () :: acc))
      else List.rev acc
    in
    loop [ operand () ]
  in
  let rec disjunction () =
    match operands ',' conjunction with
    | [ t ] -> t
    | ts -> Or ts
  and conjunction () =
    match operands ';' constraint_ with
    | [ t ] -> t
    | ts -> And ts
  and constraint_ () =
    skip_blanks ();
    match peek () with
    | Some '(' ->
      incr pos;
      let t = disjunction () in
      skip_blanks ();
      if not (Option.equal Char.equal (peek ()) (Some ')'))
      then fail "expected `)`, found %s" (found ());
      incr pos;
      t
    | _ ->
      let selector = selector () in
      let comparison = comparison selector in
      let arguments = arguments comparison in
      Compare { selector; comparison; arguments }
  in
  match
    skip_blanks ();
    if !pos >= len then fail "empty query";
    let t = disjunction () in
    skip_blanks ();
    if !pos < len then fail "unexpected %s" (found ());
    t
  with
  | t -> Ok t
  | exception Syntax message -> Error message
;;

(* Matching
   ======== *)

(** The values [selector] names: a sequence's items, or the one scalar. *)
let values (frontmatter : Yaml.value option) (selector : string) : Yaml.value list =
  match frontmatter with
  | Some (`O fields) ->
    (match List.Assoc.find fields selector ~equal:String.equal with
     | Some (`A items) -> items
     | Some v -> [ v ]
     | None -> [])
  | _ -> []
;;

(** A YAML scalar as written. YAML has a single number type, so an authored
    [2026] arrives as a float and is rendered without the [.]. *)
let scalar_text : Yaml.value -> string option = function
  | `String s -> Some s
  | `Float f ->
    Some
      (if Float.equal f (Float.round_down f) && Float.( < ) (Float.abs f) 1e16
       then Int.to_string (Float.to_int f)
       else Float.to_string f)
  | `Bool b -> Some (Bool.to_string b)
  | `Null | `A _ | `O _ -> None
;;

let compare_value (v : Yaml.value) (argument : string) : int option =
  match v, Float.of_string_opt argument with
  | `Float f, Some g -> Some (Float.compare f g)
  | _ -> Option.map (scalar_text v) ~f:(fun text -> String.compare text argument)
;;

let rec matches (t : t) (frontmatter : Yaml.value option) : bool =
  match t with
  | And ts -> List.for_all ts ~f:(fun t -> matches t frontmatter)
  | Or ts -> List.exists ts ~f:(fun t -> matches t frontmatter)
  | Compare { selector; comparison; arguments } ->
    let values = values frontmatter selector in
    let some_value argument ~f =
      List.exists values ~f:(fun v -> Option.exists (compare_value v argument) ~f)
    in
    let equal_some () =
      List.exists arguments ~f:(fun argument -> some_value argument ~f:(( = ) 0))
    in
    let ordered f = some_value (List.hd_exn arguments) ~f in
    (match comparison with
     | Eq | In -> equal_some ()
     | Neq | Out -> not (equal_some ())
     | Lt -> ordered (fun c -> c < 0)
     | Le -> ordered (fun c -> c <= 0)
     | Gt -> ordered (fun c -> c > 0)
     | Ge -> ordered (fun c -> c >= 0))
;;
