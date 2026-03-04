open Core

let render_file ~index ~vault_root ~rel_path =
  let full_path = Filename.concat vault_root rel_path in
  let content = In_channel.read_all full_path in
  let doc = Oystermark.of_string_resolved ~index ~curr_file:rel_path content in
  Oystermark.Html.of_doc ~safe:true doc
;;

let render_single ~vault_root ~file =
  let index = Oystermark.Index.build vault_root in
  let rel_path =
    match String.chop_prefix file ~prefix:(vault_root ^ "/") with
    | Some rel -> rel
    | None -> file
  in
  print_string (render_file ~index ~vault_root ~rel_path)
;;

let render_vault ~vault_root ~output_dir =
  let index = Oystermark.Index.build vault_root in
  List.iter index.files ~f:(fun (entry : Oystermark.Index.file_entry) ->
    if String.is_suffix entry.rel_path ~suffix:".md"
    then (
      let html = render_file ~index ~vault_root ~rel_path:entry.rel_path in
      let out_rel =
        String.chop_suffix_exn entry.rel_path ~suffix:".md" ^ ".html"
      in
      let out_path = Filename.concat output_dir out_rel in
      let out_dir = Filename.dirname out_path in
      Core_unix.mkdir_p out_dir;
      Out_channel.write_all out_path ~data:html;
      printf "  %s -> %s\n" entry.rel_path out_rel))
;;

let () =
  match Sys.get_argv () |> Array.to_list |> List.tl_exn with
  | [ vault_root; file ] when String.is_suffix file ~suffix:".md" ->
    render_single ~vault_root ~file
  | [ vault_root; output_dir ] ->
    render_vault ~vault_root ~output_dir
  | [ vault_root ] ->
    render_vault ~vault_root ~output_dir:(vault_root ^ "/_site")
  | _ ->
    eprintf "Usage:\n";
    eprintf "  oystermark <vault-dir>                 Render vault to <vault-dir>/_site/\n";
    eprintf "  oystermark <vault-dir> <output-dir>    Render vault to output dir\n";
    eprintf "  oystermark <vault-dir> <file.md>       Render single file to stdout\n";
    exit 1
;;
