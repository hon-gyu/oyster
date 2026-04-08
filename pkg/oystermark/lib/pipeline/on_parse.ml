open Core
open Common

(** Exclude files with [draft: true] frontmatter. Apply on parse stage. *)
let exclude_drafts : t =
  make
    ~on_parse:(fun path doc ->
      match Parse.Frontmatter.of_doc doc with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "draft" with
         | Some (`Bool true) -> []
         | _ -> [ path, doc ])
      | _ -> [ path, doc ])
    ()
;;

(** Exclude files without [publish: true] frontmatter. Apply on parse stage. *)
let exclude_unpublish : t =
  make
    ~on_parse:(fun path doc ->
      match Parse.Frontmatter.of_doc doc with
      | Some (`O fields) ->
        (match List.Assoc.find fields ~equal:String.equal "publish" with
         | Some (`Bool true) -> [ path, doc ]
         | _ -> [])
      | _ -> [])
    ()
;;

let drop_keys_in_frontmatter (keys : string list) : t =
  let yaml_f : Yaml.value -> Yaml.value option = function
    | `O fields ->
      Some
        (`O
            (List.filter_map fields ~f:(fun (k, v) ->
               if List.mem keys k ~equal:String.equal then None else Some (k, v))))
    | other -> Some other
  in
  make
    ~on_parse:(fun path doc ->
      let b_mapper = Parse.Frontmatter.make_block_mapper yaml_f in
      let mapper =
        Cmarkit.Mapper.make
          ~inline_ext_default:(fun _m i -> Some i)
          ~block_ext_default:(fun _m b -> Some b)
          ~block:b_mapper
          ()
      in
      [ path, Cmarkit.Mapper.map_doc mapper doc ])
    ()
;;

let drop_emtpy_frontmatter : t =
  let yaml_f : Yaml.value -> Yaml.value option = function
    | `O fields as v -> if List.is_empty fields then None else Some v
    | `Null -> None
    | other -> Some other
  in
  make
    ~on_parse:(fun path doc ->
      let b_mapper = Parse.Frontmatter.make_block_mapper yaml_f in
      let mapper =
        Cmarkit.Mapper.make
          ~inline_ext_default:(fun _m i -> Some i)
          ~block_ext_default:(fun _m b -> Some b)
          ~block:b_mapper
          ()
      in
      [ path, Cmarkit.Mapper.map_doc mapper doc ])
    ()
;;
