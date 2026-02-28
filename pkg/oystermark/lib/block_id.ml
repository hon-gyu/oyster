open Core

type t = string

let meta_key : t Cmarkit.Meta.key = Cmarkit.Meta.key ()

let is_valid_block_id s =
  String.length s > 0
  && String.for_all s ~f:(fun c -> Char.is_alphanum c || Char.equal c '-')
  && Char.is_alphanum (String.get s 0)
;;

let extract_trailing s =
  let s = String.rstrip s in
  (* Find the last '^' that is preceded by whitespace or is at start *)
  let rec find_caret i =
    if i < 0
    then None
    else if Char.equal (String.get s i) '^'
    then (
      let preceded_by_space = i = 0 || Char.is_whitespace (String.get s (i - 1)) in
      if preceded_by_space
      then (
        let candidate = String.drop_prefix s (i + 1) in
        if is_valid_block_id candidate
        then (
          let before = String.rstrip (String.prefix s i) in
          Some (before, candidate))
        else find_caret (i - 1))
      else find_caret (i - 1))
    else find_caret (i - 1)
  in
  find_caret (String.length s - 1)
;;
