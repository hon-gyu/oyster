open Core
module Index = Oystermark.Index

let render_file ~(index : Index.t) ~(vault_root : string) ~(rel_path : string) : string =
  let full_path = Filename.concat vault_root rel_path in
  let content = In_channel.read_all full_path in
  let doc = Oystermark.resolve ~index ~curr_file:rel_path content in
  Oystermark.Html.of_doc ~safe:true doc
;;

let file_cmd : Command.t =
  Command.basic
    ~summary:"Render a single markdown file to stdout"
    (let%map_open.Command vault_root = anon ("vault-root" %: string)
     and file = anon ("file" %: string) in
     fun () ->
       let index = Index.build vault_root in
       let rel_path =
         match String.chop_prefix file ~prefix:(vault_root ^ "/") with
         | Some rel -> rel
         | None -> file
       in
       print_string (render_file ~index ~vault_root ~rel_path))
;;

let vault_cmd : Command.t =
  Command.basic
    ~summary:"Render all markdown files in a vault to HTML"
    (let%map_open.Command (vault_root : string) = anon ("vault-root" %: string)
     and (output_dir : string option) = anon (maybe ("output-dir" %: string)) in
     fun () ->
       let output_dir : string =
         match output_dir with
         | Some d -> d
         | None -> vault_root ^ "/_site"
       in
       let (index : Index.t) = Index.build vault_root in
       List.iter index.files ~f:(fun (entry : Index.file_entry) ->
         if String.is_suffix entry.rel_path ~suffix:".md"
         then (
           let html = render_file ~index ~vault_root ~rel_path:entry.rel_path in
           let out_rel = String.chop_suffix_exn entry.rel_path ~suffix:".md" ^ ".html" in
           let out_path = Filename.concat output_dir out_rel in
           let out_dir = Filename.dirname out_path in
           Core_unix.mkdir_p out_dir;
           Out_channel.write_all out_path ~data:html;
           printf "  %s -> %s\n" entry.rel_path out_rel)))
;;

let () =
  Command.group ~summary:"Oystermark renderer" [ "file", file_cmd; "vault", vault_cmd ]
  |> Command_unix.run
;;
