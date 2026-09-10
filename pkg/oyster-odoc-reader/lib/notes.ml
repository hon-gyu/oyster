type note =
  { path : string
  ; body : string
  }

let of_odocl file =
  match Generated_html.generate file with
  | Error _ as error -> error
  | Ok notes ->
    Ok
      (List.map
         (fun (note : Generated_html.note) -> { path = note.path; body = note.body })
         notes)
;;

let write ~dir note =
  let path = Filename.concat dir (note.path ^ ".md") in
  let rec mkdirs d =
    if not (Sys.file_exists d)
    then (
      mkdirs (Filename.dirname d);
      Sys.mkdir d 0o755)
  in
  mkdirs (Filename.dirname path);
  let out = open_out path in
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out note.body)
;;
