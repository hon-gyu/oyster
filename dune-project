(lang dune 3.22)

(name oystermark)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

; The mlx dialect: .mlx files are OCaml + JSX syntax, lowered to plain OCaml
; parsetree (carrying [@JSX]) by mlx-pp, then [@JSX] is lowered per-library by
; (preprocess (pps html_of_jsx.ppx)). merlin_reader points at ocamlmerlin-mlx.
; mlmdx (pkg/mlmdx) reuses this chain to render .mlx components embedded in pages.
(dialect
 (name mlx)
 (implementation
  (extension mlx)
  (merlin_reader mlx)
  (preprocess
   (run mlx-pp %{input-file}))))

; The mlmdx dialect: .mlmdx files are prose-first Markdown pages. mlmdx-pp emits
; a plain-OCaml parsetree (structural nodes as JSX.node calls; embedded {expr}
; parsed with the real OCaml parser). ocamlmerlin-mlmdx shares the same codegen.
(dialect
 (name mlmdx)
 (implementation
  (extension mlmdx)
  (merlin_reader mlmdx)
  (preprocess
   (run mlmdx-pp %{input-file}))))

(package
 (name oystermark-core)
 (synopsis "Oystermark parser: CommonMark and various extensions")
 (description
  "Pre-resolution, file-level parsing of Oystermark documents.")
 (depends
  ocaml
  (dune
   (>= 3.22))
  oymarkit
  core
  yaml
  yojson
  ppx_jane
  ppx_string
  ppx_quick_test
  ppx_yojson_conv
  (ppx_expect :with-test)))

(package
 (name oystermark)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  (dune
   (>= 3.22))
  oystermark-core
  oymarkit
  merlin-extend
  core
  core_unix
  tyxml
  ocamlgraph
  yaml
  yojson
  ppx_jane
  ppx_string
  ppx_blob
  ppx_quick_test
  trace
  opentelemetry
  opentelemetry-client-cohttp-eio
  linol-eio
  eio_main
  (textutils :with-test)
  (ppx_expect :with-test)
  (ppx_trace :with-test)
  (shexp :with-doc)
  (odoc :with-doc))
 (tags
  ("add topics" "to describe" your project)))

(package
  (name spec)
  (allow_empty)
  (synopsis "")
  (description "")
  (depends
  ocaml
  (dune
    (>= 3.22))
  core
  ppx_jane
  opentelemetry
))

(package
  (name trace-collect)
  (synopsis "A short synopsis")
  (description "A longer description")
  (depends
  ocaml
  (dune
    (>= 3.22))
  core
  opentelemetry
  trace
  ambient-context
))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
