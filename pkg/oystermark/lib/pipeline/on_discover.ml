open Core
open Common

(** Validate that a [.md] file does not conflict with a same-named directory
    that has note in it.
    e.g. [note1.md] and [note1/] would both produce [note1/index.html].
    if [note1/] has any note in it.
    *)
let validate_no_duplicates : t =
  let on_discover (path : string) (paths : string list) : bool =
    match String.chop_suffix path ~suffix:".md" with
    | None -> true
    | Some stem ->
      let dir_prefix = stem ^ "/" in
      let dir_has_notes : bool =
        List.exists paths ~f:(fun p ->
          String.is_prefix p ~prefix:dir_prefix && String.is_suffix p ~suffix:".md")
      in
      if dir_has_notes
      then
        failwith
          (Printf.sprintf
             "Conflict: %s and %s/ both produce %s/index.html"
             path
             stem
             stem)
      else true
  in
  make ~on_discover ()
;;

(** Exclude notes that has [.draft] in stem. Apply on discover stage. *)
let exclude_draft_by_note_name : t =
  make
    ~on_discover:(fun path _paths -> not (String.is_suffix ~suffix:".draft.md" path))
    ()
;;
