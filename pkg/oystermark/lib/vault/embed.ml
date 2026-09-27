open Core

let expand_docs ?(max_depth = 5) ~(index : Index.t) (docs : (string * Note.t) list)
  : (string * Note.t) list
  =
  let docs_tbl = Hashtbl.of_alist_exn (module String) docs in
  let env ~from (link_ref : Note.Link.Ref.t) : Note.source option =
    match Index.resolve index from link_ref with
    | Error _ | Ok (Index.Asset _) -> None
    | Ok target ->
      let path = Index.target_path target in
      Hashtbl.find docs_tbl path
      |> Option.map ~f:(fun (note : Note.t) : Note.source ->
        match target with
        | Index.Anchor { anchor = { definition; _ }; _ } ->
          { path
          ; fragment = Some (Note.Transclusion.fragment definition)
          ; note =
              { note with
                doc =
                  Parse.doc_of_blocks
                    (Note.Private.Address_utils.find
                       note.doc
                       (Note.Anchor.address definition))
              }
          }
        | Note _ | Asset _ -> { path; fragment = None; note })
  in
  List.map docs ~f:(fun (path, note) -> path, Note.expand ~max_depth ~env ~path note)
;;
