(** What a vault looks like as a Jinja template context.
    Impl: {!Oystermark.Context}. Spec: the [template-context] page.

    The vaults here are built with {!Oystermark.Vault.of_files}, which does no
    IO, so [modified] is always [null]: the filesystem mtime is the one field
    these tests cannot pin down. Creation dates come from frontmatter and are
    therefore stable. *)

open Core
module Context = Oystermark.Context

let vault (md_files : (string * string) list) ?(other_files = []) () =
  Oystermark.Vault.of_files ~vault_root:"/vault" ~md_files ~other_files
;;

(** Print one field of every note, keyed by path, to keep each expectation
    focused on the fact under test rather than the whole context. *)
let print_note_field (json : Yojson.Safe.t) (field : string) =
  match json with
  | `Assoc top ->
    (match List.Assoc.find_exn top "notes" ~equal:String.equal with
     | `List notes ->
       List.iter notes ~f:(fun note ->
         match note with
         | `Assoc fields ->
           let get k = List.Assoc.find_exn fields k ~equal:String.equal in
           let path =
             match get "path" with
             | `String s -> s
             | _ -> "?"
           in
           printf "%-22s %s\n" path (Yojson.Safe.to_string (get field))
         | _ -> ())
     | _ -> ())
  | _ -> ()
;;

let print_field (json : Yojson.Safe.t) (field : string) =
  match json with
  | `Assoc top ->
    print_endline
      (Yojson.Safe.pretty_to_string (List.Assoc.find_exn top field ~equal:String.equal))
  | _ -> ()
;;

(** {1 Titles}

    See {!Oystermark.Vault.Index.Entry.title}. *)

let%expect_test "title falls back frontmatter, then first h1, then basename" =
  let json =
    Context.of_vault
      (vault
         [ "explicit.md", "---\ntitle: From Frontmatter\n---\n# From Heading\n"
         ; "heading.md", "# From Heading\n"
         ; "deep-heading.md", "## Level Two First\n# Level One\n"
         ; "bare.md", "Just a paragraph.\n"
         ; "blank-title.md", "---\ntitle: '   '\n---\n# Fallback Wins\n"
         ]
         ())
  in
  print_note_field json "title";
  [%expect
    {|
    bare.md                "bare"
    blank-title.md         "Fallback Wins"
    deep-heading.md        "Level One"
    explicit.md            "From Frontmatter"
    heading.md             "From Heading"
    |}]
;;

(** {1 Tags}

    The parser has no inline [#tag] syntax, so tags are exactly the frontmatter
    [tags] key. Both the sequence and the inline comma forms are accepted. *)

let%expect_test "tags accept sequence, comma string, and bare scalar" =
  let json =
    Context.of_vault
      (vault
         [ "seq.md", "---\ntags:\n  - alpha\n  - beta\n---\n"
         ; "inline-seq.md", "---\ntags: [alpha, beta]\n---\n"
         ; "comma.md", "---\ntags: alpha, beta\n---\n"
         ; "scalar.md", "---\ntags: alpha\n---\n"
         ; "duplicate.md", "---\ntags: [alpha, beta, alpha]\n---\n"
         ; "none.md", "---\ntitle: No Tags\n---\n"
         ]
         ())
  in
  print_note_field json "tags";
  [%expect
    {|
    comma.md               ["alpha","beta"]
    duplicate.md           ["alpha","beta"]
    inline-seq.md          ["alpha","beta"]
    none.md                []
    scalar.md              ["alpha"]
    seq.md                 ["alpha","beta"]
    |}]
;;

let%expect_test "the tag index groups paths by tag" =
  let json =
    Context.of_vault
      (vault
         [ "b.md", "---\ntags: [ocaml, active]\n---\n"
         ; "a.md", "---\ntags: [ocaml]\n---\n"
         ; "c.md", "# Untagged\n"
         ]
         ())
  in
  print_field json "tags";
  [%expect
    {|
    { "active": [ "b.md" ], "ocaml": [ "a.md", "b.md" ] }
    |}]
;;

(** {1 Dates}

    [created] is authored, not stat'd: see {!Oystermark.Vault.Index.Entry.created}
    for why. [modified] is [null] here because these vaults never touch disk. *)

let%expect_test "created reads frontmatter created, then date" =
  let json =
    Context.of_vault
      (vault
         [ "created.md", "---\ncreated: 2026-01-15\n---\n"
         ; "slashes.md", "---\ncreated: 2025/11/02\n---\n"
         ; "with-time.md", "---\ncreated: 2024-03-04 09:30:00\n---\n"
         ; "iso-t.md", "---\ncreated: 2024-03-05T09:30:00Z\n---\n"
         ; "date-key.md", "---\ndate: 2023-07-08\n---\n"
         ; "both-keys.md", "---\ncreated: 2022-01-01\ndate: 2099-01-01\n---\n"
         ; "unparseable.md", "---\ncreated: sometime last spring\n---\n"
         ; "absent.md", "# No Dates\n"
         ]
         ())
  in
  print_note_field json "created";
  [%expect
    {|
    absent.md              null
    both-keys.md           "2022-01-01"
    created.md             "2026-01-15"
    date-key.md            "2023-07-08"
    iso-t.md               "2024-03-05"
    slashes.md             "2025-11-02"
    unparseable.md         null
    with-time.md           "2024-03-04"
    |}]
;;

let%expect_test "modified prefers an authored updated key over the filesystem" =
  let json =
    Context.of_vault
      (vault
         [ "updated.md", "---\nupdated: 2026-05-06\n---\n"; "plain.md", "# Plain\n" ]
         ())
  in
  print_note_field json "modified";
  [%expect
    {|
    plain.md               null
    updated.md             "2026-05-06"
    |}]
;;

(** {1 Frontmatter passthrough}

    Keys this library has no opinion about still reach the template. *)

let%expect_test "frontmatter is passed through as JSON, integers unwidened" =
  let json =
    Context.of_vault
      (vault
         [ ( "note.md"
           , "---\n\
              title: Note\n\
              weight: 3\n\
              ratio: 1.5\n\
              draft: true\n\
              authors: [ada, grace]\n\
              nested:\n\
             \  key: value\n\
              ---\n" )
         ]
         ())
  in
  print_note_field json "frontmatter";
  [%expect
    {|
    note.md                {"title":"Note","weight":3,"ratio":1.5,"draft":true,"authors":["ada","grace"],"nested":{"key":"value"}}
    |}]
;;

let%expect_test "a non-mapping frontmatter has no keys to index" =
  let json = Context.of_vault (vault [ "list.md", "---\n- a\n- b\n---\n" ] ()) in
  print_note_field json "frontmatter";
  [%expect
    {|
    list.md                {}
    |}]
;;

(** {1 Links and backlinks}

    Backlinks include links aimed at an anchor the note owns, so a heading link
    counts as a reference to its note. *)

let%expect_test "backlinks carry the source note's title and line" =
  let json =
    Context.of_vault
      (vault
         [ "target.md", "# Target\n## Section\n"
         ; "linker.md", "---\ntitle: The Linker\n---\nSee [[target]].\n"
         ; "anchor-linker.md", "Jump to [[target#Section]].\n"
         ]
         ())
  in
  print_note_field json "backlinks";
  [%expect
    {|
    anchor-linker.md       []
    linker.md              []
    target.md              [{"source":"anchor-linker.md","title":"anchor-linker","kind":"link","line":1},{"source":"linker.md","title":"The Linker","kind":"link","line":4}]
    |}]
;;

let%expect_test "an unresolved link is reported with its reason and line" =
  let json =
    Context.of_vault
      (vault
         [ "source.md", "# Source\nGood [[target]], bad [[nowhere]].\n"
         ; "target.md", "# Target\n"
         ]
         ())
  in
  print_field json "unresolved";
  [%expect
    {|
    [
      {
        "source": "source.md",
        "target": "nowhere",
        "fragment": null,
        "kind": "link",
        "reason": "missing_path",
        "line": 2
      }
    ]
    |}]
;;

(** {1 The whole shape}

    One small vault printed in full, so the exposed namespace is visible in one
    place and any change to it shows up as a diff here. *)

let%expect_test "the complete context of a two-note vault" =
  let json =
    Context.of_vault
      (vault
         [ ( "notes/target.md"
           , "---\n\
              title: Target\n\
              tags: [demo]\n\
              created: 2026-02-03\n\
              ---\n\
              # Target\n\
              ## Section\n" )
         ; "source.md", "# Source\nSee [[notes/target#Section]] and ![[logo.png]].\n"
         ]
         ~other_files:[ "logo.png" ]
         ())
  in
  print_endline (Yojson.Safe.pretty_to_string json);
  [%expect
    {|
    {
      "vault": {
        "root": "/vault",
        "note_count": 2,
        "asset_count": 1,
        "link_count": 2,
        "orphan_count": 1,
        "unresolved_count": 0
      },
      "notes": [
        {
          "path": "notes/target.md",
          "dir": "notes",
          "segments": [ "notes" ],
          "name": "target.md",
          "stem": "target",
          "title": "Target",
          "tags": [ "demo" ],
          "frontmatter": {
            "title": "Target",
            "tags": [ "demo" ],
            "created": "2026-02-03"
          },
          "created": "2026-02-03",
          "modified": null,
          "headings": [
            { "text": "Target", "level": 1, "slug": "target", "line": 6 },
            { "text": "Section", "level": 2, "slug": "section", "line": 7 }
          ],
          "links": [],
          "link_count": 0,
          "backlinks": [
            {
              "source": "source.md",
              "title": "Source",
              "kind": "link",
              "line": 2
            }
          ],
          "backlink_count": 1,
          "is_orphan": false
        },
        {
          "path": "source.md",
          "dir": "",
          "segments": [],
          "name": "source.md",
          "stem": "source",
          "title": "Source",
          "tags": [],
          "frontmatter": {},
          "created": null,
          "modified": null,
          "headings": [
            { "text": "Source", "level": 1, "slug": "source", "line": 1 }
          ],
          "links": [
            {
              "kind": "link",
              "target": "notes/target",
              "fragment": "#Section",
              "resolved": "notes/target.md",
              "broken": false,
              "reason": null,
              "line": 2
            },
            {
              "kind": "embed",
              "target": "logo.png",
              "fragment": null,
              "resolved": "logo.png",
              "broken": false,
              "reason": null,
              "line": 2
            }
          ],
          "link_count": 2,
          "backlinks": [],
          "backlink_count": 0,
          "is_orphan": true
        }
      ],
      "notes_by_path": {
        "notes/target.md": {
          "path": "notes/target.md",
          "dir": "notes",
          "segments": [ "notes" ],
          "name": "target.md",
          "stem": "target",
          "title": "Target",
          "tags": [ "demo" ],
          "frontmatter": {
            "title": "Target",
            "tags": [ "demo" ],
            "created": "2026-02-03"
          },
          "created": "2026-02-03",
          "modified": null,
          "headings": [
            { "text": "Target", "level": 1, "slug": "target", "line": 6 },
            { "text": "Section", "level": 2, "slug": "section", "line": 7 }
          ],
          "links": [],
          "link_count": 0,
          "backlinks": [
            {
              "source": "source.md",
              "title": "Source",
              "kind": "link",
              "line": 2
            }
          ],
          "backlink_count": 1,
          "is_orphan": false
        },
        "source.md": {
          "path": "source.md",
          "dir": "",
          "segments": [],
          "name": "source.md",
          "stem": "source",
          "title": "Source",
          "tags": [],
          "frontmatter": {},
          "created": null,
          "modified": null,
          "headings": [
            { "text": "Source", "level": 1, "slug": "source", "line": 1 }
          ],
          "links": [
            {
              "kind": "link",
              "target": "notes/target",
              "fragment": "#Section",
              "resolved": "notes/target.md",
              "broken": false,
              "reason": null,
              "line": 2
            },
            {
              "kind": "embed",
              "target": "logo.png",
              "fragment": null,
              "resolved": "logo.png",
              "broken": false,
              "reason": null,
              "line": 2
            }
          ],
          "link_count": 2,
          "backlinks": [],
          "backlink_count": 0,
          "is_orphan": true
        }
      },
      "assets": [
        {
          "path": "logo.png",
          "dir": "",
          "segments": [],
          "name": "logo.png",
          "stem": "logo"
        }
      ],
      "tags": { "demo": [ "notes/target.md" ] },
      "orphans": [ "source.md" ],
      "unresolved": []
    }
    |}]
;;
