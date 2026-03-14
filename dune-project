(lang dune 3.21)

(name oystermark)

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
   (>= 3.21))
  cmarkit
  core
  core_unix
  tyxml
  yaml
  ppx_jane
  ppx_string
  ppx_blob
  ppx_quick_test
  (textutils :with-test)
  (ppx_expect :with-test)
  (shexp :with-doc)
  (odoc :with-doc))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
