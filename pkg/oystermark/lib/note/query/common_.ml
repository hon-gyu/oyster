(* The query language itself: what a query is, and how one is built. Reading,
   printing and running it are {!Syntax_} and {!Eval_}. The
   documentation of these types is {!Query}. *)

open Core

type cmp =
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type pred =
  | Prop of string * cmp * Node.value
  | Has of string
  | Exists of steps
  | Count of steps * cmp * int
  | And of pred list
  | Or of pred list
  | Not of pred

and axis =
  | Self
  | Child
  | Descendant
  | Field of string
  | Section of
      { path : string list
      ; exact : bool
      }

and step =
  { axis : axis
  ; where : pred list
  ; nth : int option
  }

and steps = step list

type t = steps

let is (kind : string) : pred = Prop ("kind", Eq, String kind)

(* Combinators
   ----------- *)

let empty : steps = []

let add (steps : steps) (axis : axis) ~where ~nth : steps =
  steps @ [ { axis; where; nth } ]
;;

let self ?(where = []) ?nth (steps : steps) : steps = add steps Self ~where ~nth
let child ?(where = []) ?nth (steps : steps) : steps = add steps Child ~where ~nth
let descend ?(where = []) ?nth (steps : steps) : steps = add steps Descendant ~where ~nth

let field ?(where = []) ?nth (key : string) (steps : steps) : steps =
  add steps (Field key) ~where ~nth
;;

let section ?(exact = true) ?(where = []) ?nth (path : string list) (steps : steps)
  : steps
  =
  add steps (Section { path; exact }) ~where ~nth
;;
