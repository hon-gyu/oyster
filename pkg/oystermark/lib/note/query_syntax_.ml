(* Text is read in two passes:
   - {!parse} builds a generic [expr] of calls, lists and atoms,
   and elaboration checks each call's name and arguments.
   - Printing builds the same [expr] and writes it out, so the two directions share one
   shape. *)

open Core
open Query_common_

type expr =
  | Atom of
      { text : string
      ; quoted : bool
      }
  | List of expr list
  | Call of
      { name : string
      ; args : (string option * expr) list (** A label for [key=value]. *)
      }

let rec expr_to_string : expr -> string = function
  | Atom { text; quoted } -> if quoted then sprintf "%S" text else text
  | List items ->
    sprintf "[%s]" (String.concat ~sep:", " (List.map items ~f:expr_to_string))
  | Call { name; args = [] } -> name
  | Call { name; args } ->
    let arg (label, expr) =
      match label with
      | None -> expr_to_string expr
      | Some label -> label ^ "=" ^ expr_to_string expr
    in
    sprintf "%s(%s)" name (String.concat ~sep:", " (List.map args ~f:arg))
;;

let cmp_to_string : cmp -> string = function
  | Eq -> "="
  | Ne -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
;;

(* Printing
   ======== *)

let word (text : string) : expr = Atom { text; quoted = false }

(** A string, quoted when it would read as a number, a boolean, or several
    tokens. *)
let string (s : string) : expr =
  let quoted =
    String.is_empty s
    || String.exists s ~f:(fun c -> Char.is_whitespace c || String.mem "()[],=\"" c)
    || Option.is_some (Int.of_string_opt s)
    || String.equal s "true"
    || String.equal s "false"
  in
  Atom { text = s; quoted }
;;

let call name args = Call { name; args = List.map args ~f:(fun arg -> None, arg) }

let rec pred_to_expr : pred -> expr = function
  | Prop ("kind", Eq, String kind) -> call "Is" [ string kind ]
  | Prop (name, cmp, value) ->
    let value =
      match value with
      | Int n -> word (Int.to_string n)
      | Bool b -> word (Bool.to_string b)
      | String s -> string s
    in
    call "Prop" [ string name; word (cmp_to_string cmp); value ]
  | Has name -> call "Has" [ string name ]
  | Not pred -> call "Not" [ pred_to_expr pred ]
  | And preds -> call "And" [ List (List.map preds ~f:pred_to_expr) ]
  | Or preds -> call "Or" [ List (List.map preds ~f:pred_to_expr) ]
  | Exists steps -> call "Exists" [ steps_to_expr steps ]
  | Count (steps, cmp, n) ->
    call "Count" [ steps_to_expr steps; word (cmp_to_string cmp); word (Int.to_string n) ]

and step_to_expr ({ axis; where; nth } : step) : expr =
  let name, args =
    match axis with
    | Self -> "Self", []
    | Child -> "Child", []
    | Descendant -> "Descendant", []
    | Field key -> "Field", [ None, string key ]
    | Section { path; exact } ->
      ( "Section"
      , (None, List (List.map path ~f:string))
        :: (if exact then [] else [ Some "exact", word "false" ]) )
  in
  let where =
    if List.is_empty where
    then []
    else [ Some "where", List (List.map where ~f:pred_to_expr) ]
  in
  let nth =
    Option.to_list (Option.map nth ~f:(fun n -> Some "nth", word (Int.to_string n)))
  in
  Call { name; args = args @ where @ nth }

and steps_to_expr (steps : steps) : expr = List (List.map steps ~f:step_to_expr)

let to_string (steps : t) : string = expr_to_string (steps_to_expr steps)
let step_to_string (step : step) : string = expr_to_string (step_to_expr step)
let pred_to_string (pred : pred) : string = expr_to_string (pred_to_expr pred)

(* Reading
   ======= *)

exception Parse_error of string

let fail fmt = ksprintf (fun message -> raise (Parse_error message)) fmt

(** A word runs up to whitespace or punctuation, except that [<=], [>=] and
    [!=] are one word. *)
let parse (text : string) : expr =
  let length = String.length text in
  let pos = ref 0 in
  let peek () =
    while !pos < length && Char.is_whitespace text.[!pos] do
      Int.incr pos
    done;
    if !pos < length then Some text.[!pos] else None
  in
  let at c = Option.equal Char.equal (peek ()) (Some c) in
  let describe = function
    | Some c -> sprintf "%c at %d" c !pos
    | None -> "the end"
  in
  let read_word () =
    let start = !pos in
    while
      !pos < length
      && not (Char.is_whitespace text.[!pos] || String.mem "()[],=\"" text.[!pos])
    do
      Int.incr pos
    done;
    if !pos = start + 1 && at '=' && String.mem "<>!" text.[start] then Int.incr pos;
    String.sub text ~pos:start ~len:(!pos - start)
  in
  (* Items separated by commas, after the opening bracket, up to [close]. *)
  let sequence close item =
    let rec more acc =
      let acc = item () :: acc in
      if at ','
      then (
        Int.incr pos;
        more acc)
      else if at close
      then (
        Int.incr pos;
        List.rev acc)
      else fail "expected , or %c, got %s" close (describe (peek ()))
    in
    if at close
    then (
      Int.incr pos;
      [])
    else more []
  in
  let rec expr () =
    match peek () with
    | Some '[' ->
      Int.incr pos;
      List (sequence ']' expr)
    | Some '"' ->
      let s, consumed =
        try
          Stdlib.Scanf.sscanf (String.drop_prefix text !pos) "%S%n" (fun s n -> s, n)
        with
        | _ -> fail "unterminated string at %d" !pos
      in
      pos := !pos + consumed;
      Atom { text = s; quoted = true }
    (* The operator [=]; a label's [=] is taken by [arg]. *)
    | Some '=' ->
      Int.incr pos;
      word "="
    | Some c when not (String.mem "()],\"" c) ->
      let name = read_word () in
      if at '('
      then (
        Int.incr pos;
        Call { name; args = sequence ')' arg })
      else Atom { text = name; quoted = false }
    | next -> fail "unexpected %s" (describe next)
  and arg () =
    match expr () with
    | Atom { text; quoted = false } when at '=' ->
      Int.incr pos;
      Some text, expr ()
    | expr -> None, expr
  in
  let query = expr () in
  if Option.is_some (peek ())
  then fail "unexpected %s after the query" (describe (peek ()));
  query
;;

(* Elaboration
   =========== *)

let expected (what : string) (expr : expr) =
  fail "expected %s, got %s" what (expr_to_string expr)
;;

let text_of : expr -> string = function
  | Atom { text; _ } -> text
  | expr -> expected "a word or a string" expr
;;

let list_of : expr -> expr list = function
  | List items -> items
  | expr -> expected "a list [...]" expr
;;

let value_of : expr -> Node.value option = function
  | Atom { text; quoted = true } -> Some (String text)
  | Atom { text; quoted = false } ->
    (match Int.of_string_opt text, text with
     | Some n, _ -> Some (Int n)
     | None, ("true" | "false") -> Some (Bool (Bool.of_string text))
     | None, _ -> Some (String text))
  | List _ | Call _ -> None
;;

let int_of (expr : expr) : int =
  match value_of expr with
  | Some (Int n) -> n
  | _ -> expected "a number" expr
;;

let bool_of (expr : expr) : bool =
  match value_of expr with
  | Some (Bool b) -> b
  | _ -> expected "true or false" expr
;;

let cmp_of (expr : expr) : cmp =
  match
    List.find [ Eq; Ne; Lt; Le; Gt; Ge ] ~f:(fun cmp ->
      match expr with
      | Atom { text; quoted = false } -> String.equal text (cmp_to_string cmp)
      | _ -> false)
  with
  | Some cmp -> cmp
  | None -> expected "one of = != < <= > >=" expr
;;

(** A call's name, positional arguments and labelled arguments, rejecting any
    label not in [labels]. A bare word is a call without arguments. *)
let call_of (expr : expr) ~(labels : string list)
  : string * expr list * (string -> expr option)
  =
  match expr with
  | Atom { text = name; quoted = false } -> name, [], fun _ -> None
  | Call { name; args } ->
    let positional, labelled =
      List.partition_map args ~f:(function
        | None, arg -> First arg
        | Some label, arg -> Second (label, arg))
    in
    ignore
      (List.fold labelled ~init:[] ~f:(fun seen (label, _) ->
         if List.mem seen label ~equal:String.equal
         then fail "%s has duplicate argument %s" name label;
         label :: seen));
    List.iter labelled ~f:(fun (label, _) ->
      if not (List.mem labels label ~equal:String.equal)
      then fail "%s takes no argument %s" name label);
    name, positional, List.Assoc.find labelled ~equal:String.equal
  | expr -> expected "a call such as Child" expr
;;

(** The error for a call of a known name with the wrong arguments, or of an
    unknown one. [forms] is each name with how it is called. *)
let misused ~(forms : (string * string) list) (name : string) (expr : expr) =
  match List.Assoc.find forms name ~equal:String.equal with
  | Some form -> expected form expr
  | None ->
    fail "unknown %s; one of %s" name (String.concat ~sep:", " (List.map forms ~f:fst))
;;

let rec pred_of (expr : expr) : pred =
  match call_of expr ~labels:[] with
  | "Is", [ kind ], _ -> is (text_of kind)
  | "Prop", [ name; cmp; value ], _ ->
    Prop
      ( text_of name
      , cmp_of cmp
      , Option.value_or_thunk (value_of value) ~default:(fun () ->
          expected "a value" value) )
  | "Has", [ name ], _ -> Has (text_of name)
  | "Not", [ pred ], _ -> Not (pred_of pred)
  | "And", [ preds ], _ -> And (List.map (list_of preds) ~f:pred_of)
  | "Or", [ preds ], _ -> Or (List.map (list_of preds) ~f:pred_of)
  | "Exists", [ steps ], _ -> Exists (steps_of steps)
  | "Count", [ steps; cmp; n ], _ -> Count (steps_of steps, cmp_of cmp, int_of n)
  | name, _, _ ->
    misused
      name
      expr
      ~forms:
        [ "Is", "Is(KIND)"
        ; "Prop", "Prop(NAME, OP, VALUE)"
        ; "Has", "Has(NAME)"
        ; "Not", "Not(PRED)"
        ; "And", "And([PRED, ...])"
        ; "Or", "Or([PRED, ...])"
        ; "Exists", "Exists([STEP, ...])"
        ; "Count", "Count([STEP, ...], OP, INT)"
        ]

and step_of (expr : expr) : step =
  let name, positional, label = call_of expr ~labels:[ "where"; "nth"; "exact" ] in
  let axis =
    match name, positional, label "exact" with
    | "Self", [], None -> Self
    | "Child", [], None -> Child
    | "Descendant", [], None -> Descendant
    | "Field", [ key ], None -> Field (text_of key)
    | "Section", [ path ], exact ->
      Section
        { path = List.map (list_of path) ~f:text_of
        ; exact = Option.value_map exact ~default:true ~f:bool_of
        }
    | name, _, _ ->
      misused
        name
        expr
        ~forms:
          [ "Self", "Self(where=..., nth=...)"
          ; "Child", "Child(where=..., nth=...)"
          ; "Descendant", "Descendant(where=..., nth=...)"
          ; "Field", "Field(KEY, where=..., nth=...)"
          ; "Section", "Section([NAME, ...], exact=BOOL, where=..., nth=...)"
          ]
  in
  { axis
  ; where =
      Option.value_map (label "where") ~default:[] ~f:(fun preds ->
        List.map (list_of preds) ~f:pred_of)
  ; nth = Option.map (label "nth") ~f:int_of
  }

and steps_of (expr : expr) : steps = List.map (list_of expr) ~f:step_of

let of_string (text : string) : (t, string) Result.t =
  try Ok (steps_of (parse text)) with
  | Parse_error message -> Error message
;;
