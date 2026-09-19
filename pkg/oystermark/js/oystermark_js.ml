(** JavaScript boundary for Oystermark parsing and vault indexing. *)

open Core
open Js_of_ocaml
module Vault = Oystermark.Vault
module Index = Vault.Index
module Json = Yojson.Safe

let option f = function
  | None -> `Null
  | Some value -> f value
;;

let json_of_fragment = function
  | Oystermark.Note.Link.Ref.Hash_path path ->
    `Assoc
      [ "kind", `String "hash-path"
      ; "path", `List (List.map path ~f:(fun s -> `String s))
      ]
  | Caret_id id -> `Assoc [ "kind", `String "caret-id"; "id", `String id ]
;;

let json_of_reference (reference : Oystermark.Note.Link.Ref.t) =
  `Assoc
    [ "target", option (fun s -> `String s) reference.target
    ; "fragment", option json_of_fragment reference.fragment
    ]
;;

let json_of_loc loc =
  `Assoc
    [ "firstByte", `Int (Cmarkit.Textloc.first_byte loc)
    ; "lastByte", `Int (Cmarkit.Textloc.last_byte loc)
    ]
;;

let json_of_anchor_definition = function
  | Index.Heading { text; level; slug } ->
    `Assoc
      [ "kind", `String "heading"
      ; "text", `String text
      ; "level", `Int level
      ; "slug", `String slug
      ]
  | Caret id ->
    `Assoc [ "kind", `String "block"; "id", `String id; "syntax", `String "caret" ]
  | Attr { id; inline = false } ->
    `Assoc [ "kind", `String "block"; "id", `String id; "syntax", `String "attribute" ]
  | Attr { id; inline = true } -> `Assoc [ "kind", `String "inline"; "id", `String id ]
;;

let json_of_anchor (anchor : Index.Anchor.t) =
  `Assoc
    [ "value", json_of_anchor_definition anchor.definition
    ; "location", json_of_loc anchor.loc
    ]
;;

let json_of_resolution = function
  | Ok (Index.Note path) -> `Assoc [ "kind", `String "note"; "path", `String path ]
  | Ok (Asset path) -> `Assoc [ "kind", `String "asset"; "path", `String path ]
  | Ok (Anchor { note_path; anchor }) ->
    `Assoc
      [ "kind", `String "anchor"
      ; "path", `String note_path
      ; "anchor", json_of_anchor anchor
      ]
  | Error Index.Missing_path -> `Assoc [ "kind", `String "missing-path" ]
  | Error (Index.Missing_anchor path) ->
    `Assoc [ "kind", `String "missing-anchor"; "path", `String path ]
;;

let json_of_link index source (link : Index.Link.t) =
  `Assoc
    [ "reference", json_of_reference link.reference
    ; ( "kind"
      , `String
          (match link.kind with
           | Index.Link.Link -> "link"
           | Embed -> "embed") )
    ; "location", json_of_loc link.loc
    ; "resolution", json_of_resolution (Index.resolve index source link.reference)
    ]
;;

let json_of_note vault (note : Index.Entry.t) =
  let path = Index.Entry.path note in
  let doc = Option.value_exn (Vault.find_doc vault path) in
  `Assoc
    [ "path", `String path
    ; "mdast", Json.from_string (Cmarkit_mdast.of_doc ~strip_block_id:false doc)
    ; "anchors", `List (List.map (Index.Entry.anchors note) ~f:json_of_anchor)
    ; ( "links"
      , `List (List.map (Index.Entry.links note) ~f:(json_of_link vault.index path)) )
    ]
;;

let member_string name json = Json.Util.member name json |> Json.Util.to_string

let index request =
  let json = Json.from_string request in
  let vault_root = member_string "vaultRoot" json in
  let md_files =
    Json.Util.member "markdownFiles" json
    |> Json.Util.to_list
    |> List.map ~f:(fun file -> member_string "path" file, member_string "content" file)
  in
  let other_files =
    Json.Util.member "otherFiles" json
    |> Json.Util.to_list
    |> List.map ~f:Json.Util.to_string
  in
  let vault = Vault.of_files ~vault_root ~md_files ~other_files in
  `Assoc
    [ "notes", `List (List.map (Index.notes vault.index) ~f:(json_of_note vault))
    ; ( "assets"
      , `List
          (List.map (Index.assets vault.index) ~f:(fun a -> `String (Index.Asset.path a)))
      )
    ]
  |> Json.to_string
;;

let parse markdown =
  Oystermark.Parse.of_string ~locs:true markdown
  |> Cmarkit_mdast.of_doc ~strip_block_id:false
;;

let expose name f =
  Js.Unsafe.set
    Js.Unsafe.global
    (Js.string name)
    (Js.wrap_callback (fun value -> Js.string (f (Js.to_string value))))
;;

let () =
  expose "oystermarkParse" parse;
  expose "oystermarkIndex" index
;;
