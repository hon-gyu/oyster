(** The overridable components table.

    Markdown-structural elements in a [.mlmdx] page (headings, paragraphs,
    emphasis, ...) route through this record rather than hardcoded [JSX.node]
    calls, so a consumer can restyle or swap any element page-wide:

    {[
      JSX.render
        (Page.make
           ~components:{ Components.default with
                         h1 = (fun ~children -> JSX.node "h1" [ "class", `String "title" ] children) }
           ())
    ]}

    This is what makes mlmdx {e MDX}, not markdown-to-HTML. Each [default]
    renderer reproduces the vanilla HTML element, so a page rendered without a
    custom table (the common case) is byte-for-byte identical to plain
    markdown-to-HTML — the table is always present, its default is the identity.

    Literal JSX and component calls written in the page ([ <div> ], [ <Foo/> ])
    do {e not} route through this table: those are explicit author intent, like
    raw JSX in MDX. Only Markdown syntax dispatches through the components. *)

type elt = children:JSX.element list -> JSX.element

type t =
  { h1 : elt
  ; h2 : elt
  ; h3 : elt
  ; h4 : elt
  ; h5 : elt
  ; h6 : elt
  ; p : elt
  ; em : elt
  ; strong : elt
  ; code : elt
  ; blockquote : elt
  }

let host tag ~children = JSX.node tag [] children

let default =
  { h1 = host "h1"
  ; h2 = host "h2"
  ; h3 = host "h3"
  ; h4 = host "h4"
  ; h5 = host "h5"
  ; h6 = host "h6"
  ; p = host "p"
  ; em = host "em"
  ; strong = host "strong"
  ; code = host "code"
  ; blockquote = host "blockquote"
  }
