let usage = "oyster-odoc-reader OUT_DIR FILE.odocl..."

let () =
  match Array.to_list Sys.argv with
  | _ :: dir :: (_ :: _ as files) ->
    List.iter
      (fun file ->
         match Oyster_odoc_reader.Notes.of_odocl (Fpath.v file) with
         | Error message -> prerr_endline message
         | Ok notes ->
           List.iter
             (fun (note : Oyster_odoc_reader.Notes.note) ->
                Oyster_odoc_reader.Notes.write ~dir note;
                print_endline note.path)
             notes)
      files
  | _ ->
    prerr_endline usage;
    exit 1
;;
