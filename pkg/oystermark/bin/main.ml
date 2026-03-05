open Core

(* NOTE:

  let%map_open.Command combines two things:

  1. %map — it's a ppx that desugars to Command.Param.map. It takes a command parameter
   spec and maps a function over its result. So let%map x = param in body becomes
  Command.Param.map param ~f:(fun x -> body).
  2. open — it opens Command.Param locally, so you can write anon, flag, string, etc.
  directly instead of Command.Param.anon, Command.Param.flag, etc.

  Combined with and, multiple parameters are collected in parallel (using
  Command.Param.both under the hood), then the function body receives all of them.
*)

let render_doc
      ~(index : Oystermark.Vault.Index.t)
      ~(curr_file : string)
      (parsed : Oystermark.Parse.doc)
  : string
  =
  let resolved = Oystermark.resolve ~index ~curr_file parsed.doc in
  Oystermark.Html.of_doc ~safe:true ~frontmatter:parsed.frontmatter resolved
;;

let file_cmd : Command.t =
  Command.basic
    ~summary:"Render a single markdown file to stdout"
    (let%map_open.Command vault_root = anon ("vault-root" %: string)
     and file = anon ("file" %: string) in
     fun () ->
       let index, docs = Oystermark.Vault.build vault_root in
       let rel_path =
         match String.chop_prefix file ~prefix:(vault_root ^ "/") with
         | Some rel -> rel
         | None -> file
       in
       let doc =
         List.Assoc.find docs ~equal:String.equal rel_path
         |> Option.value_exn ~message:(sprintf "File %s not found in vault" rel_path)
       in
       print_string (render_doc ~index ~curr_file:rel_path doc))
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
       let index, docs = Oystermark.Vault.build vault_root in
       List.iter docs ~f:(fun (rel_path, doc) ->
         let html = render_doc ~index ~curr_file:rel_path doc in
         let out_rel = String.chop_suffix_exn rel_path ~suffix:".md" ^ ".html" in
         let out_path = Filename.concat output_dir out_rel in
         let out_dir = Filename.dirname out_path in
         Core_unix.mkdir_p out_dir;
         Out_channel.write_all out_path ~data:html;
         printf "  %s -> %s\n" rel_path out_rel))
;;

let () =
  Command.group ~summary:"Oystermark renderer" [ "file", file_cmd; "vault", vault_cmd ]
  |> Command_unix.run
;;
