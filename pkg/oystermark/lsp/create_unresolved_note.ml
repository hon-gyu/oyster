(** Quick-fix calculation for creating an unresolved linked note.

    Spec: {!page-"feature-codeaction-create-unresolved-link"}. *)

open Core

type action =
  { rel_path : string
  ; title : string
  }
[@@deriving sexp, equal, compare]

let safe_note_path target =
  let target = String.strip target in
  let components = String.split target ~on:'/' in
  let safe_component component =
    (not (String.is_empty component))
    && (not (String.mem component '\\'))
    && not (String.equal component "." || String.equal component "..")
  in
  let extension = snd (Filename.split_extension target) in
  if
    String.is_empty target
    || Filename.is_absolute target
    || (not (List.for_all components ~f:safe_component))
    || Option.exists extension ~f:(fun ext -> not (String.equal ext ".md"))
  then None
  else Some (if Option.is_some extension then target else target ^ ".md")
;;

let title_of_path path =
  let basename = Filename.basename path in
  Option.value (String.chop_suffix basename ~suffix:".md") ~default:basename
;;

let action_at_range ~index ~rel_path ~content ~first_byte ~last_byte =
  let doc = Lsp_util.parse_doc content in
  Link_collect.collect_links doc
  |> List.find ~f:(fun link ->
    link.first_byte <= last_byte && first_byte <= link.last_byte)
  |> Option.bind ~f:(fun link ->
    match link.kind, link.link_ref.target, link.link_ref.fragment with
    | Link_collect.Link, Some target, None ->
      (match Oystermark.Vault.Resolve.resolve link.link_ref rel_path index with
       | Unresolved ->
         safe_note_path target
         |> Option.map ~f:(fun rel_path -> { rel_path; title = title_of_path rel_path })
       | _ -> None)
    | _ -> None)
;;

module For_test = struct
  let action_at_range = action_at_range
end
