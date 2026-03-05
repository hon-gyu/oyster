(** Link ref extraction tests: end-to-end from raw markdown text to Link_ref.t. *)

open! Core
open Oystermark

(* Extract all Link_ref.t values from a parsed document, in order. *)
let extract_link_refs (doc : Cmarkit.Doc.t) : Link_ref.t list =
  let folder =
    Cmarkit.Folder.make
      ~inline:(fun _f acc i ->
        match i with
        | Cmarkit.Inline.Link (link, _meta) ->
          let ref_ = Cmarkit.Inline.Link.reference link in
          (match Link_ref.of_cmark_reference ref_ with
           | Some lr -> Cmarkit.Folder.ret (acc @ [ lr ])
           | None -> Cmarkit.Folder.default)
        | _ -> Cmarkit.Folder.default)
      ~inline_ext_default:(fun _f acc i ->
        match i with
        | Wikilink.Ext_wikilink (w, _meta) ->
          let lr = Link_ref.of_wikilink w in
          acc @ [ lr ]
        | _ -> acc)
      ~block_ext_default:(fun _f acc _b -> acc)
      ()
  in
  Cmarkit.Folder.fold_doc folder [] doc
;;

let pp_link_ref (lr : Link_ref.t) =
  let t = Option.value ~default:"-" lr.target in
  let f =
    match lr.fragment with
    | None -> "-"
    | Some (Heading hs) -> "H[" ^ String.concat ~sep:"; " hs ^ "]"
    | Some (Block_ref s) -> "B[" ^ s ^ "]"
  in
  sprintf "(%s, %s)" t f
;;

let pp_link_ref (lr : Link_ref.t) = Link_ref.sexp_of_t lr |> Sexp.to_string_hum

(** Parse a single inline markdown snippet and return the first Link_ref extracted. *)
let link_ref_of (md : string) : string =
  let doc = Oystermark_base.of_string md in
  match extract_link_refs doc with
  | [ lr ] -> pp_link_ref lr
  | [] -> "<none>"
  | lrs -> String.concat ~sep:" | " (List.map lrs ~f:pp_link_ref)
;;

let print_cases cases =
  let rows = List.map cases ~f:(fun (name, input) -> name, input, link_ref_of input) in
  let cols =
    [ Ascii_table.Column.create "name" Tuple3.get1
    ; Ascii_table.Column.create "input" Tuple3.get2
    ; Ascii_table.Column.create "link_ref" Tuple3.get3
    ]
  in
  print_string (Ascii_table.to_string_noattr cols rows ~limit_width_to:150)
;;

let%expect_test "wikilink_link_refs" =
  let cases =
    [ "basic note", "[[Three laws of motion]]"
    ; "with ext", "[[Three laws of motion.md]]"
    ; "with pipe", "[[Note 2 | Note two]]"
    ; "self heading", "[[#Level 3 title]]"
    ; "cross heading", "[[Note 2#Some level 2 title]]"
    ; "nested heading", "[[Note 2#Some level 2 title#Level 3 title]]"
    ; "block ref", "[[Note 2#^blockid]]"
    ; "empty [[]]", "[[]]"
    ; "empty heading [[#]]", "[[#]]"
    ; "empty heading other", "[[Note 2##]]"
    ; "hash collapse", "[[###L2#L4]]"
    ; "hash collapse 2", "[[##L2######L4]]"
    ; "hash collapse invalid", "[[##L2#####L4#L3]]"
    ; "pipe + heading", "[[#L2 | #L4]]"
    ; "multi pipe", "[[Note 2 | 2 | 3]]"
    ; "asset jpg", "[[Figure1.jpg]]"
    ; "asset with hash", "[[Figure1.jpg#2]]"
    ; "asset .md suffix", "[[Figure1.jpg.md]]"
    ; "asset hash in name", "[[Figure1#2.jpg]]"
    ; "embed", "![[Figure1.jpg]]"
    ; "asset block ref", "[[Figure1^2.jpg]]"
    ]
  in
  print_cases cases;
  [%expect
    {|
    ┌───────────────────────┬─────────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────┐
    │ name                  │ input                                       │ link_ref                                                            │
    ├───────────────────────┼─────────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────┤
    │ basic note            │ [[Three laws of motion]]                    │ ((target ("Three laws of motion")) (fragment ()))                   │
    │ with ext              │ [[Three laws of motion.md]]                 │ ((target ("Three laws of motion.md")) (fragment ()))                │
    │ with pipe             │ [[Note 2 | Note two]]                       │ ((target ("Note 2 ")) (fragment ()))                                │
    │ self heading          │ [[#Level 3 title]]                          │ ((target ()) (fragment ((Heading ("Level 3 title")))))              │
    │ cross heading         │ [[Note 2#Some level 2 title]]               │ ((target ("Note 2")) (fragment ((Heading ("Some level 2 title"))))) │
    │ nested heading        │ [[Note 2#Some level 2 title#Level 3 title]] │ ((target ("Note 2"))                                                │
    │                       │                                             │  (fragment ((Heading ("Some level 2 title" "Level 3 title")))))     │
    │ block ref             │ [[Note 2#^blockid]]                         │ ((target ("Note 2")) (fragment ((Block_ref blockid))))              │
    │ empty [[]]            │ [[]]                                        │ ((target ()) (fragment ()))                                         │
    │ empty heading [[#]]   │ [[#]]                                       │ ((target ()) (fragment ()))                                         │
    │ empty heading other   │ [[Note 2##]]                                │ ((target ("Note 2")) (fragment ()))                                 │
    │ hash collapse         │ [[###L2#L4]]                                │ ((target ()) (fragment ((Heading (L2 L4)))))                        │
    │ hash collapse 2       │ [[##L2######L4]]                            │ ((target ()) (fragment ((Heading (L2 L4)))))                        │
    │ hash collapse invalid │ [[##L2#####L4#L3]]                          │ ((target ()) (fragment ((Heading (L2 L4 L3)))))                     │
    │ pipe + heading        │ [[#L2 | #L4]]                               │ ((target ()) (fragment ((Heading ("L2 ")))))                        │
    │ multi pipe            │ [[Note 2 | 2 | 3]]                          │ ((target ("Note 2 ")) (fragment ()))                                │
    │ asset jpg             │ [[Figure1.jpg]]                             │ ((target (Figure1.jpg)) (fragment ()))                              │
    │ asset with hash       │ [[Figure1.jpg#2]]                           │ ((target (Figure1.jpg)) (fragment ((Heading (2)))))                 │
    │ asset .md suffix      │ [[Figure1.jpg.md]]                          │ ((target (Figure1.jpg.md)) (fragment ()))                           │
    │ asset hash in name    │ [[Figure1#2.jpg]]                           │ ((target (Figure1)) (fragment ((Heading (2.jpg)))))                 │
    │ embed                 │ ![[Figure1.jpg]]                            │ ((target (Figure1.jpg)) (fragment ()))                              │
    │ asset block ref       │ [[Figure1^2.jpg]]                           │ ((target (Figure1^2.jpg)) (fragment ()))                            │
    └───────────────────────┴─────────────────────────────────────────────┴─────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "markdown_link_link_refs" =
  let cases =
    [ "percent encoded spaces", "[x](Three%20laws%20of%20motion.md)"
    ; "same file heading", "[x](#Level%203%20title)"
    ; "cross file heading", "[x](Note%202#Some%20level%202%20title)"
    ; "just target", "[x](ww)"
    ; "hash in heading", "[x](##L2######L4)"
    ; "hash collapse", "[x](##L2#####L4#L3)"
    ; "external https", "[x](https://example.com)"
    ; "external http", "[x](http://example.com)"
    ; "external mailto", "[x](mailto:a@b.com)"
    ; "empty dest [www]()", "[www]()"
    ]
  in
  print_cases cases;
  [%expect
    {|
    ┌────────────────────────┬────────────────────────────────────────┬─────────────────────────────────────────────────────────────────────┐
    │ name                   │ input                                  │ link_ref                                                            │
    ├────────────────────────┼────────────────────────────────────────┼─────────────────────────────────────────────────────────────────────┤
    │ percent encoded spaces │ [x](Three%20laws%20of%20motion.md)     │ ((target ("Three laws of motion.md")) (fragment ()))                │
    │ same file heading      │ [x](#Level%203%20title)                │ ((target ()) (fragment ((Heading ("Level 3 title")))))              │
    │ cross file heading     │ [x](Note%202#Some%20level%202%20title) │ ((target ("Note 2")) (fragment ((Heading ("Some level 2 title"))))) │
    │ just target            │ [x](ww)                                │ ((target (ww)) (fragment ()))                                       │
    │ hash in heading        │ [x](##L2######L4)                      │ ((target ()) (fragment ((Heading (L2 L4)))))                        │
    │ hash collapse          │ [x](##L2#####L4#L3)                    │ ((target ()) (fragment ((Heading (L2 L4 L3)))))                     │
    │ external https         │ [x](https://example.com)               │ <none>                                                              │
    │ external http          │ [x](http://example.com)                │ <none>                                                              │
    │ external mailto        │ [x](mailto:a@b.com)                    │ <none>                                                              │
    │ empty dest [www]()     │ [www]()                                │ ((target ()) (fragment ()))                                         │
    └────────────────────────┴────────────────────────────────────────┴─────────────────────────────────────────────────────────────────────┘
    |}]
;;
