(** Read odoc documentation into an OysterMark vault.

    odoc has already parsed the doc comments, resolved every cross-reference,
    rendered the signatures and assigned an anchor to each declaration by the
    time it writes a [.odocl] file. {!Notes.of_odocl} asks the active [odoc]
    executable to cross that internal boundary and emit embeddable JSON. The
    reader consumes that textual output, so its command can read documentation
    built by another OCaml switch.

    See {!Generated_html} for that boundary and {!Render} for the direct,
    in-process renderer. *)

module Address = Address
module Generated_html = Generated_html
module Notes = Notes
module Render = Render
