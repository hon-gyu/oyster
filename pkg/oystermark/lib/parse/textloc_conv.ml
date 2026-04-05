(** Sexp and compare conversions for [Cmarkit.Textloc.t]. *)

open Core

let sexp_of_line_pos ((line, byte) : int * int) : Sexp.t =
  Sexp.List [ Atom (Int.to_string line); Atom (Int.to_string byte) ]
;;

let line_pos_of_sexp (sexp : Sexp.t) : int * int =
  match sexp with
  | Sexp.List [ Atom l; Atom b ] -> Int.of_string l, Int.of_string b
  | _ -> of_sexp_error "expected (line byte)" sexp
;;

let sexp_of_t (tl : Cmarkit.Textloc.t) : Sexp.t =
  Sexp.List
    [ Sexp.List
        [ Atom "first_byte"; Atom (Int.to_string (Cmarkit.Textloc.first_byte tl)) ]
    ; Sexp.List [ Atom "last_byte"; Atom (Int.to_string (Cmarkit.Textloc.last_byte tl)) ]
    ; Sexp.List [ Atom "first_line"; sexp_of_line_pos (Cmarkit.Textloc.first_line tl) ]
    ; Sexp.List [ Atom "last_line"; sexp_of_line_pos (Cmarkit.Textloc.last_line tl) ]
    ]
;;

let t_of_sexp (sexp : Sexp.t) : Cmarkit.Textloc.t =
  match sexp with
  | Sexp.List fields ->
    let get name =
      List.find_map_exn fields ~f:(fun field ->
        match field with
        | Sexp.List [ Atom n; v ] when String.equal n name -> Some v
        | _ -> None)
    in
    let first_byte =
      match get "first_byte" with
      | Atom s -> Int.of_string s
      | s -> of_sexp_error "expected int" s
    in
    let last_byte =
      match get "last_byte" with
      | Atom s -> Int.of_string s
      | s -> of_sexp_error "expected int" s
    in
    let first_line = line_pos_of_sexp (get "first_line") in
    let last_line = line_pos_of_sexp (get "last_line") in
    Cmarkit.Textloc.v
      ~file:Cmarkit.Textloc.file_none
      ~first_byte
      ~last_byte
      ~first_line
      ~last_line
  | _ -> of_sexp_error "expected record" sexp
;;

let compare (a : Cmarkit.Textloc.t) (b : Cmarkit.Textloc.t) : int =
  let c = Int.compare (Cmarkit.Textloc.first_byte a) (Cmarkit.Textloc.first_byte b) in
  if c <> 0
  then c
  else Int.compare (Cmarkit.Textloc.last_byte a) (Cmarkit.Textloc.last_byte b)
;;
