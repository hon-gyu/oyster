(lang dune 3.22)

(name oystermark)

; The version reported by [oyster -version] and by the LSP [serverInfo].
(version 0.1.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name oystermark)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  (dune
   (>= 3.22))
  djot
  core
  core_unix
  tyxml
  ocamlgraph
  yaml
  yojson
  base64
  ppx_jane
  ppx_string
  ppx_blob
  ppx_quick_test
  ppx_yojson_conv
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
 (name oyster-odoc-reader)
 (synopsis "Read odoc documentation into an OysterMark vault")
 (description
  "Renders the pages odoc has already compiled and linked -- .odocl files -- into OysterMark notes, one note per odoc page.")
 (depends
  ocaml
  (dune
   (>= 3.22))
  odoc
  fpath
  lambdasoup
  yojson))

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
