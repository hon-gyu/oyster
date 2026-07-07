open Extend_protocol
module Protocol_reader = Extend_protocol.Reader

let identifier_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let ident_at_text ~file text (pos : Lexing.position) =
  let n = String.length text in
  let cnum = max 0 (min pos.pos_cnum n) in
  let left = ref cnum in
  while !left > 0 && identifier_char text.[!left - 1] do
    decr left
  done;
  let right = ref cnum in
  while !right < n && identifier_char text.[!right] do
    incr right
  done;
  if !left = !right then [] else
  let txt = String.sub text !left (!right - !left) in
  let loc_start =
    { pos with
      pos_fname = file;
      pos_cnum = !left;
    }
  in
  let loc_end = { loc_start with pos_cnum = !right } in
  [ { Location.txt; loc = { Location.loc_start; loc_end; loc_ghost = false } } ]

module Reader = struct
  type t = Protocol_reader.buffer

  let load buffer = buffer

  let parse t =
    Protocol_reader.Structure (Mlmdx_codegen.Codegen.of_string ~file:t.Protocol_reader.path t.Protocol_reader.text)

  let for_completion t _pos =
    { Protocol_reader.complete_labels = true }, parse t

  let parse_line t pos line =
    let lb = Lexing.from_string line in
    Lexing.set_filename lb t.Protocol_reader.path;
    Lexing.set_position lb pos;
    Protocol_reader.Structure (Parse.implementation lb)

  let ident_at t pos = ident_at_text ~file:t.Protocol_reader.path t.Protocol_reader.text pos

  let print_outcome ppf outcome =
    Extend_helper.print_outcome_using_oprint ppf outcome

  let pretty_print ppf = function
    | Protocol_reader.Pretty_toplevel_phrase x -> Pprintast.toplevel_phrase ppf x
    | Pretty_expression x -> Pprintast.expression ppf x
    | Pretty_core_type x -> Pprintast.core_type ppf x
    | Pretty_pattern x -> Pprintast.pattern ppf x
    | Pretty_signature x -> Pprintast.signature ppf x
    | Pretty_structure x -> Pprintast.structure ppf x
    | Pretty_case_list _ -> Format.pp_print_string ppf "<cases>"
end

let () =
  Extend_main.extension_main
    ~reader:(Extend_main.Reader.make_v0 (module Reader))
    (Extend_main.Description.make_v0 ~name:"mlmdx" ~version:"0.1")
