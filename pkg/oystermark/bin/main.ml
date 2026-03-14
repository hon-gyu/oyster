open Core
open Oystermark

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

let file_cmd : Command.t =
  Command.basic
    ~summary:"Render a single markdown file to stdout"
    (let%map_open.Command vault_root = anon ("vault-root" %: string)
     and file = anon ("file" %: string)
     and output_dir = anon (maybe ("output-dir" %: string)) in
     fun () ->
       let rel_path =
         match String.chop_prefix file ~prefix:(vault_root ^ "/") with
         | Some rel -> rel
         | None -> file
       in
       let vault = Oystermark.Vault.of_root_path vault_root in
       let doc =
         List.Assoc.find vault.docs ~equal:String.equal rel_path
         |> Option.value_exn ~message:(sprintf "File %s not found in vault" rel_path)
       in
       if Option.is_none output_dir
       then
         Out_channel.write_all
           (Filename.concat (Option.value_exn output_dir) "index.html")
           ~data:(Oystermark.Html.of_doc ~backend_blocks:true ~safe:false doc)
       else print_string (Oystermark.Html.of_doc ~backend_blocks:true ~safe:false doc))
;;

let theme_of_string (s : string) : Oystermark.Theme.t =
  Theme.of_name (Config.theme_of_string s)
;;

let vault_cmd : Command.t =
  Command.basic
    ~summary:"Render all markdown files in a vault to HTML"
    (let%map_open.Command (vault_root : string) = anon ("vault-root" %: string)
     and (output_dir : string option) = anon (maybe ("output-dir" %: string))
     and (verbose : bool) = flag "--verbose" no_arg ~doc:"Print progress messages"
     and (theme : string option) =
       flag
         "--theme"
         (optional string)
         ~doc:
           "NAME Theme to use (tokyonight, gruvbox, atom-one-dark, atom-one-light, \
            bluloco-dark, bluloco-light, none). Default: gruvbox"
     and (css_snippets : string list) = anon (sequence ("css-snippet" %: string))
     and (config_file : string option) = anon (maybe ("config-file" %: string))
     in
     fun () ->
       let theme : Oystermark.Theme.t =
         match theme with
         | Some name -> theme_of_string name
         | None -> Oystermark.Theme.default
       in
       let output_dir : string =
         match output_dir with
         | Some d -> d
         | None ->
           let curr_dir = Sys_unix.getcwd () in
           curr_dir ^ "/_site"
       in
       let results =
         Oystermark.render_vault ~theme ~backend_blocks:true ~safe:false vault_root
       in
       List.iteri results ~f:(fun i (out_rel, html) ->
         let out_path = Filename.concat output_dir out_rel in
         let out_dir = Filename.dirname out_path in
         Core_unix.mkdir_p out_dir;
         Out_channel.write_all out_path ~data:html;
         if verbose
         then printf "  %s\n" out_rel
         else (
           let print_char c = Out_channel.output_char Out_channel.stdout c in
           if i mod 60 = 0 && i > 0 then print_char '\n';
           print_char '.';
           Out_channel.flush Out_channel.stdout));
       (* Copy non-markdown assets (images, etc.) to the output directory *)
       let all_entries = Oystermark.Vault.list_entries vault_root in
       let is_asset (p : string) : bool =
         (not (String.is_suffix p ~suffix:".md")) && not (String.is_suffix p ~suffix:"/")
       in
       List.iter all_entries ~f:(fun rel_path ->
         if is_asset rel_path
         then (
           let src = Filename.concat vault_root rel_path in
           let dst = Filename.concat output_dir rel_path in
           let dst_dir = Filename.dirname dst in
           Core_unix.mkdir_p dst_dir;
           let content = In_channel.read_all src in
           Out_channel.write_all dst ~data:content)))
;;

let () =
  Command.group ~summary:"Oystermark renderer" [ "file", file_cmd; "vault", vault_cmd ]
  |> Command_unix.run
;;
