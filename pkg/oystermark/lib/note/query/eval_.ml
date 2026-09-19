open Core
open Common_
module B = Cmarkit.Block
module Cursor = Cursor_

(* Axes
   ==== *)

let is_list (cursor : Cursor.t) : bool =
  match cursor.view with
  | V_block block ->
    (match Cursor.unwrap_attributes block with
     | B.List _ -> true
     | _ -> false)
  | V_root | V_item _ | V_section _ -> false
;;

(** The key a keyed node holds, from the keyed syntax: the label of a keyed
    paragraph, or the one a list item starts with. *)
let key_of (cursor : Cursor.t) : string option =
  match Cursor.shallow_node cursor with
  | Node.Keyed_paragraph { key } -> Some key
  | Node.List_item { key; _ } -> key
  | _ -> None
;;

(** The keyed nodes under [cursor] holding [key]: its keyed children, and the
    keyed items of [cursor] when it is a list. *)
let keyed_under ~(key : string) (cursor : Cursor.t) : Cursor.t list =
  let matching k = Option.value_map k ~default:false ~f:(String.equal key) in
  if is_list cursor
  then
    List.concat_map (Cursor.children cursor) ~f:(fun item ->
      if matching (key_of item)
      then
        List.filter (Cursor.children item) ~f:(fun block -> Option.is_some (key_of block))
      else [])
  else List.filter (Cursor.children cursor) ~f:(fun child -> matching (key_of child))
;;

(** The value of [key] on [cursor]: the blocks of the keyed node holding it.
    The fields of a list are its keyed items; those of any other node are its
    keyed children, and, under a list item or a keyed node, the keyed items of
    a list it holds — there a list describes the node above it, whereas under a
    section or a div a list is content of its own. *)
let field_axis ~(key : string) (cursor : Cursor.t) : Cursor.t list =
  let describing =
    match Cursor.shallow_node cursor with
    | Node.List_item _ | Node.Keyed_paragraph _ ->
      List.filter (Cursor.children cursor) ~f:is_list
      |> List.concat_map ~f:(keyed_under ~key)
    | _ -> []
  in
  keyed_under ~key cursor @ describing |> List.concat_map ~f:Cursor.children
;;

let section_id (cursor : Cursor.t) : string option =
  match Cursor.shallow_node cursor with
  | Node.Section { heading = Node.Heading { id; _ }; _ } -> Some id
  | _ -> None
;;

(** The sections under [cursor] matching [path], compared by heading
    identifier. A name is read as one, so both [Setup] and [setup] match the
    heading [## Setup]. This axis walks sections only: a heading inside a
    container is reached through the child or field axis first. *)
let section_axis ~(path : string list) ~(exact : bool) (cursor : Cursor.t) : Cursor.t list
  =
  let wanted = List.map path ~f:Parse.Common.heading_id_of_text in
  let sections cursor =
    List.filter (Cursor.children cursor) ~f:(fun child ->
      Option.is_some (section_id child))
  in
  let matches name section =
    Option.value_map (section_id section) ~default:false ~f:(String.equal name)
  in
  let rec exactly cursor remaining =
    match remaining with
    | [] -> [ cursor ]
    | name :: rest ->
      List.concat_map (sections cursor) ~f:(fun section ->
        if matches name section then exactly section rest else [])
  in
  let rec anywhere cursor remaining =
    List.concat_map (sections cursor) ~f:(fun section ->
      let here =
        match remaining with
        | name :: rest when matches name section ->
          if List.is_empty rest then [ section ] else anywhere section rest
        | _ -> []
      in
      here @ anywhere section remaining)
  in
  match wanted with
  | [] -> [ cursor ]
  | wanted -> if exact then exactly cursor wanted else anywhere cursor wanted
;;

(* Running a step
   ============== *)

let holds_cmp (cmp : cmp) (order : int) : bool =
  match cmp with
  | Eq -> order = 0
  | Ne -> order <> 0
  | Lt -> order < 0
  | Le -> order <= 0
  | Gt -> order > 0
  | Ge -> order >= 0
;;

let compare_values (a : Node.value) (b : Node.value) : int option =
  match a, b with
  | Int a, Int b -> Some (Int.compare a b)
  | String a, String b -> Some (String.compare a b)
  | Bool a, Bool b -> Some (Bool.compare a b)
  | (Int _ | String _ | Bool _), _ -> None
;;

let rec holds (pred : pred) (cursor : Cursor.t) : bool =
  match pred with
  | Prop (name, cmp, wanted) ->
    List.exists (Cursor.prop_values name cursor) ~f:(fun actual ->
      Option.value_map (compare_values actual wanted) ~default:false ~f:(holds_cmp cmp))
  | Has name -> not (List.is_empty (Cursor.prop_values name cursor))
  | Exists steps -> not (List.is_empty (eval steps [ cursor ]))
  | Count (steps, cmp, n) ->
    holds_cmp cmp (Int.compare (List.length (eval steps [ cursor ])) n)
  | And preds -> List.for_all preds ~f:(fun pred -> holds pred cursor)
  | Or preds -> List.exists preds ~f:(fun pred -> holds pred cursor)
  | Not pred -> not (holds pred cursor)

and move (axis : axis) (cursor : Cursor.t) : Cursor.t list =
  match axis with
  | Self -> [ cursor ]
  | Child -> Cursor.children cursor
  | Descendant -> Cursor.descendants cursor
  | Field key -> field_axis ~key cursor
  | Section { path; exact } -> section_axis ~path ~exact cursor

(** The axis, then the filter, then the index: [candidates], [kept] and the
    step's output. The two before the output are what {!no_match} reports.
    Candidates come in document order, without duplicates: a path is the node's
    index at each level, so ordering paths orders the nodes. *)
and apply (step : step) (input : Cursor.t list)
  : Cursor.t list * Cursor.t list * Cursor.t list
  =
  let candidates =
    List.concat_map input ~f:(move step.axis)
    |> List.dedup_and_sort ~compare:(fun a b ->
      List.compare Int.compare (Cursor.path a) (Cursor.path b))
  in
  let kept =
    List.filter candidates ~f:(fun cursor ->
      List.for_all step.where ~f:(fun pred -> holds pred cursor))
  in
  let output =
    match step.nth with
    | None -> kept
    | Some n ->
      let length = List.length kept in
      Option.to_list (List.nth kept (if n < 0 then length + n else n))
  in
  candidates, kept, output

and eval (steps : steps) (input : Cursor.t list) : Cursor.t list =
  List.fold steps ~init:input ~f:(fun input step ->
    let _, _, output = apply step input in
    output)
;;

(* Run
   === *)

type stage =
  | No_candidate
  | Filtered_out of Node.t list
  | Out_of_range of { length : int }

type no_match =
  { index : int
  ; step : step
  ; reached : Node.t list
  ; stage : stage
  }

type result =
  { matches : Node.found_t list
  ; why_empty : no_match option
  }

let run (query : t) (doc : Cmarkit.Doc.t) : result =
  let rec go index input failure steps =
    match steps with
    | [] -> input, failure
    | step :: rest ->
      let candidates, kept, output = apply step input in
      let failure =
        match failure with
        | Some _ -> failure
        | None ->
          if List.is_empty output
          then
            Some
              { index
              ; step
              ; reached = List.map input ~f:Cursor.node
              ; stage =
                  (if List.is_empty candidates
                   then No_candidate
                   else if List.is_empty kept
                   then Filtered_out (List.map candidates ~f:Cursor.node)
                   else Out_of_range { length = List.length kept })
              }
          else None
      in
      go (index + 1) output failure rest
  in
  let final, failure = go 0 [ Cursor.root doc ] None query in
  { matches = List.map final ~f:Cursor.found
  ; why_empty = (if List.is_empty final then failure else None)
  }
;;

let no_match_to_string ({ index; step; reached; stage } : no_match) : string =
  let listing values =
    match List.stable_dedup values ~compare:String.compare with
    | [] -> "none"
    | values -> String.concat values ~sep:", "
  in
  let kinds_of nodes = listing (List.map nodes ~f:Node.kind) in
  let where = sprintf "step %d (%s)" index (Syntax_.step_to_string step) in
  match stage with
  | No_candidate -> sprintf "%s: nothing to move to from %s" where (kinds_of reached)
  | Filtered_out candidates ->
    let values =
      match step.where with
      | Prop (name, _, _) :: _ ->
        (match List.filter_map candidates ~f:(Node.prop name) with
         | [] -> sprintf "none of them has %s" name
         | values ->
           sprintf "%s here: %s" name (listing (List.map values ~f:Node.value_to_string)))
      | _ -> sprintf "kinds here: %s" (kinds_of candidates)
    in
    sprintf "%s: %d nodes, none kept; %s" where (List.length candidates) values
  | Out_of_range { length } -> sprintf "%s: only %d nodes to index" where length
;;
