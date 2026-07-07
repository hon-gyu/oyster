(* Plain OCaml driving a .mlmdx page: proves the full chain
   .mlmdx -> mlmdx-pp -> html_of_jsx.ppx -> JSX.render -> HTML string. *)

(* Default: no components table supplied, so every markdown element renders as a
   vanilla HTML node. *)
let () = print_endline (JSX.render (Hello.make ()))

(* Overridden: swap the h1 renderer page-wide via the components table. Only the
   markdown heading (# ...) is affected; literal JSX and <Component/> calls in
   the page are untouched. This is what makes mlmdx MDX, not markdown-to-HTML. *)
let () =
  let components =
    { Mlmdx.Components.default with
      h1 = (fun ~children -> JSX.node "h1" [ "class", `String "title" ] children)
    }
  in
  print_endline (JSX.render (Hello.make ~components ()))
