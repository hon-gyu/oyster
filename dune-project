(lang dune 3.22)

(name oystermark)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name oystermark-core)
 (synopsis "Oystermark parser: CommonMark and various extensions")
 (description
  "Pre-resolution, file-level parsing of Oystermark documents.")
 (depends
  ocaml
  (dune
   (>= 3.21))
  cmarkit
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
   (>= 3.21))
  oystermark-core
  cmarkit
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
  (name trace-collect)
  (synopsis "A short synopsis")
  (description "A longer description")
  (depends
  ocaml
  (dune
    (>= 3.21))
  core
  opentelemetry
  trace
  ambient-context
))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
