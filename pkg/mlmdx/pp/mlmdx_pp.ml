(* mlmdx dialect preprocessor.

   Reads a .mlmdx file and writes the generated OCaml [Parsetree.structure] using
   the classic -pp binary-AST protocol (same as mlx-pp): the compiler's
   [ast_impl_magic_number] followed by a marshaled [(filename, structure)] pair.
   The compiler / ppxlib reads this and, per the consuming library's
   [(preprocess (pps html_of_jsx.ppx))], lowers any [@JSX] before typing. *)

let read_file file =
  let ic = open_in_bin file in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let () =
  let file = Sys.argv.(1) in
  let structure = Mlmdx_codegen.Codegen.of_string ~file (read_file file) in
  set_binary_mode_out stdout true;
  output_string stdout Config.ast_impl_magic_number;
  output_value stdout file;
  output_value stdout structure
