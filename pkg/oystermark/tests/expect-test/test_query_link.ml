(* Tests for {!Oystermark.Note.Query} and query-link resolution in
   {!Oystermark.Vault.Index.resolve}. Spec: {!page-"query-link"}. *)

open! Core
open Oystermark
module Query = Note.Query
module Index = Vault.Index

(* Syntax
   ====== *)

let parse s =
  match Query.of_string s with
  | Ok t -> print_s [%sexp (t : Query.t)]
  | Error message -> print_endline ("error: " ^ message)
;;

let%expect_test "operators" =
  List.iter
    [ "id==foo"
    ; "id!=foo"
    ; "n=lt=1"
    ; "n<1"
    ; "n=le=1"
    ; "n<=1"
    ; "n=gt=1"
    ; "n>1"
    ; "n=ge=1"
    ; "n>=1"
    ; "id=in=(a,b)"
    ; "id=out=(a)"
    ]
    ~f:parse;
  [%expect
    {|
    (Compare (selector id) (comparison Eq) (arguments (foo)))
    (Compare (selector id) (comparison Neq) (arguments (foo)))
    (Compare (selector n) (comparison Lt) (arguments (1)))
    (Compare (selector n) (comparison Lt) (arguments (1)))
    (Compare (selector n) (comparison Le) (arguments (1)))
    (Compare (selector n) (comparison Le) (arguments (1)))
    (Compare (selector n) (comparison Gt) (arguments (1)))
    (Compare (selector n) (comparison Gt) (arguments (1)))
    (Compare (selector n) (comparison Ge) (arguments (1)))
    (Compare (selector n) (comparison Ge) (arguments (1)))
    (Compare (selector id) (comparison In) (arguments (a b)))
    (Compare (selector id) (comparison Out) (arguments (a)))
    |}]
;;

let%expect_test "[;] binds tighter than [,], and groups override it" =
  parse "a==1;b==2,c==3";
  parse "a==1;(b==2,c==3)";
  parse " a == 1 ; b == 2 ";
  [%expect
    {|
    (Or
     ((And
       ((Compare (selector a) (comparison Eq) (arguments (1)))
        (Compare (selector b) (comparison Eq) (arguments (2)))))
      (Compare (selector c) (comparison Eq) (arguments (3)))))
    (And
     ((Compare (selector a) (comparison Eq) (arguments (1)))
      (Or
       ((Compare (selector b) (comparison Eq) (arguments (2)))
        (Compare (selector c) (comparison Eq) (arguments (3)))))))
    (And
     ((Compare (selector a) (comparison Eq) (arguments (1)))
      (Compare (selector b) (comparison Eq) (arguments (2)))))
    |}]
;;

let%expect_test "quoted values" =
  parse {|title=="two words"|};
  parse {|title=='it\'s'|};
  parse {|title=="a;b,c(d)"|};
  [%expect
    {|
    (Compare (selector title) (comparison Eq) (arguments ("two words")))
    (Compare (selector title) (comparison Eq) (arguments (it's)))
    (Compare (selector title) (comparison Eq) (arguments ("a;b,c(d)")))
    |}]
;;

let%expect_test "errors" =
  List.iter
    [ ""
    ; "id"
    ; "id="
    ; "id=foo=bar"
    ; "id=="
    ; "id==(a,b)"
    ; "id=in=(a,b"
    ; "title=='a"
    ; "(id==a"
    ; "id==a)"
    ; "==a"
    ]
    ~f:parse;
  [%expect
    {|
    error: empty query
    error: expected a comparison operator after `id`, found the end of the query
    error: expected a comparison operator after `id`
    error: unknown comparison operator `=foo=`
    error: expected a value, found the end of the query
    error: only `=in=` and `=out=` take a list of values
    error: expected `,` or `)`, found the end of the query
    error: unterminated quoted value (a query value cannot contain `#` or `|`, even quoted)
    error: expected `)`, found the end of the query
    error: unexpected `)`
    error: expected a field name, found `=`
    |}]
;;

(* Matching
   ======== *)

let frontmatter =
  Yaml.of_string_exn
    "id: foo\n\
     draft: false\n\
     quoted: \"false\"\n\
     year: 2026\n\
     created: 2026-03-01\n\
     tags: [essay, draft]\n\
     nested: {a: 1}\n\
     nothing: null\n"
;;

let%expect_test "matching" =
  List.iter
    [ "id==foo"
    ; "id==bar"
    ; "draft==false"
    ; "quoted==false"
    ; "year==2026"
    ; "year==2026.0"
    ; "year=gt=2025;year<2027"
    ; "created=lt=2026-12-31"
    ; "created=ge=2027-01-01"
    ; "tags==essay"
    ; "tags!=essay"
    ; "tags=in=(x,draft)"
    ; "tags=out=(x,y)"
    ; "absent==x"
    ; "absent!=x"
    ; "nested==x"
    ; "nothing==null"
    ; "id==bar,tags==essay"
    ]
    ~f:(fun s ->
      let t = Result.ok_or_failwith (Query.of_string s) in
      printf "%-24s %b\n" s (Query.matches t (Some frontmatter)));
  [%expect
    {|
    id==foo                  true
    id==bar                  false
    draft==false             true
    quoted==false            true
    year==2026               true
    year==2026.0             true
    year=gt=2025;year<2027   true
    created=lt=2026-12-31    true
    created=ge=2027-01-01    false
    tags==essay              true
    tags!=essay              false
    tags=in=(x,draft)        true
    tags=out=(x,y)           true
    absent==x                false
    absent!=x                true
    nested==x                false
    nothing==null            false
    id==bar,tags==essay      true
    |}]
;;

let%expect_test "frontmatter that is not a mapping has no fields" =
  let t = Result.ok_or_failwith (Query.of_string "id!=foo") in
  printf "%b %b\n" (Query.matches t None) (Query.matches t (Some (`String "id: foo")));
  [%expect {| true true |}]
;;

(* Resolution
   ========== *)

let index =
  Vault.build_index
    ~md_docs:
      (List.map
         [ "a.md", "---\nid: a\ngroup: x\n---\n# A\n\n## Intro\n"
         ; "d/b.md", "---\nid: b\ngroup: x\n---\n# B\n"
         ; "c.md", "no frontmatter\n"
         ]
         ~f:(fun (path, content) -> path, Parse.of_string ~locs:true content))
    ~other_files:[ "image.png" ]
    ()
;;

let%expect_test "resolution" =
  List.iter
    [ "?id==a"
    ; "?id==b"
    ; "?id==a#Intro"
    ; "?id==a#Nope"
    ; "?group==x"
    ; "?id==z"
    ; "?id="
    ; "  ?id==b  "
    ]
    ~f:(fun target ->
      let reference = Note.Link.Ref.of_wikilink_target target in
      printf
        "%-14S %s\n"
        target
        (match Index.resolve index "c.md" reference with
         | Ok target -> Sexp.to_string [%sexp (target : Index.target)]
         | Error e -> Index.string_of_resolution_error e));
  [%expect
    {|
    "?id==a"       (Note a.md)
    "?id==b"       (Note d/b.md)
    "?id==a#Intro" (Anchor(note_path a.md)(anchor((definition(Heading((text Intro)(level 2)(slug Intro))))(loc((first_byte 28)(last_byte 35)(first_line(7 28))(last_line(7 28)))))))
    "?id==a#Nope"  no such heading or id in a.md
    "?group==x"    the query matches 2 notes: a.md, d/b.md
    "?id==z"       no note matches the query
    "?id="         invalid query: expected a comparison operator after `id`
    "  ?id==b  "   (Note d/b.md)
    |}]
;;

let%expect_test "a markdown destination starting with [?] is not a vault link" =
  print_s [%sexp (Note.Link.Ref.of_destination "?id==a" : Note.Link.Ref.t option)];
  [%expect {| () |}]
;;
