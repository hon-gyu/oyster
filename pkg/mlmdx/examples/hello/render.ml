(* Plain OCaml driving a .mlmdx page: proves the full chain
   .mlmdx -> mlmdx-pp -> html_of_jsx.ppx -> JSX.render -> HTML string. *)
let () = print_endline (JSX.render (Hello.make ()))
