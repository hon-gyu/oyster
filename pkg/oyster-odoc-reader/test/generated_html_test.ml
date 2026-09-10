let expected =
  {|{#demo}
# Demo

See [[demo/Module#val-run|Module.run]].

{#val-use}
```ocaml
val use : Module.t
```

Refs: [[demo/Module#type-t]]

Use a value.

|}
;;

let () =
  let fixture = Sys.argv.(1) in
  let root = Filename.dirname (Filename.dirname fixture) in
  match Oyster_odoc_reader.Generated_html.of_directory root with
  | Error message -> failwith message
  | Ok [ note ] ->
    if not (String.equal note.path "demo/page")
    then failwith (Printf.sprintf "unexpected path: %S" note.path);
    if not (String.equal note.body expected)
    then failwith (Printf.sprintf "expected:\n%s\nactual:\n%s" expected note.body)
  | Ok notes -> failwith (Printf.sprintf "expected one note, got %d" (List.length notes))
;;
