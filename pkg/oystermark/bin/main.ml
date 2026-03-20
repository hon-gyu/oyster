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

(** Documentation-only specification  *)
module type Spec = sig
  (** - For all configs in {!Config.t}, there should be an individual flag.
      - User can provide config using a config file or a config string.
      - If either config file or config string is provided, individual flags are ignored.
      - If both config file and config string are provided, an error is raised.
      - If no config is provided, the default config is used and individual flags applied on top.
      - It's normal that for a specific config, the format used by the flag is
        different from the format used in the yaml.
  *)
  val config : unit
end

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
       let vault = Vault.of_root_path vault_root in
       let doc =
         List.Assoc.find vault.docs ~equal:String.equal rel_path
         |> Option.value_exn ~message:(sprintf "File %s not found in vault" rel_path)
       in
       if Option.is_none output_dir
       then
         Out_channel.write_all
           (Filename.concat (Option.value_exn output_dir) "index.html")
           ~data:(Html.of_doc ~backend_blocks:true ~safe:false doc)
       else print_string (Html.of_doc ~backend_blocks:true ~safe:false doc))
;;

let theme_of_string (s : string) : Theme.t = Theme.of_name (Config.theme_of_string s)

let pipeline_of_profile ~(cache : Code_executor.cache) (p : Config.pipeline_profile)
  : Pipeline.t
  =
  match p with
  | Default -> Pipeline.default ~cache ()
  | Basic -> Pipeline.basic
  | None_profile -> Pipeline.id
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
            bluloco-dark, bluloco-light, none). Default: bluloco-dark"
     and (css_snippets : string list) =
       flag "--css-snippet" (listed string) ~doc:"PATH CSS snippet file to include"
     and (config_file : string option) =
       flag "--config" (optional string) ~doc:"PATH Path to a YAML config file"
     and (config_yaml : string option) =
       flag "--config-yaml" (optional string) ~doc:"YAML Inline YAML config string"
     and (pipeline_profile : string option) =
       flag
         "--pipeline"
         (optional string)
         ~doc:"NAME Pipeline profile (default, basic, none). Default: default"
     in
     fun () ->
       (* ::: config-resolving *)
       let config : Config.t =
         match config_file, config_yaml with
         | Some _, Some _ -> failwith "Cannot provide both --config and --config-yaml"
         | Some path, None -> Config.of_file path
         | None, Some yaml -> Config.of_yaml_string yaml
         | None, None ->
           { theme =
               Option.value_map
                 theme
                 ~default:Config.default.theme
                 ~f:Config.theme_of_string
           ; css_snippets =
               (match css_snippets with
                | [] -> Config.default.css_snippets
                | snippets -> snippets)
           ; pipeline_profile =
               Option.value_map
                 pipeline_profile
                 ~default:Config.default.pipeline_profile
                 ~f:Config.pipeline_profile_of_string
           }
       in
       let css_snippet_contents : string list =
         List.map config.css_snippets ~f:In_channel.read_all
       in
       let theme : Theme.t =
         Theme.of_name ~css_snippets:css_snippet_contents config.theme
       in
       (* ::: *)
       let output_dir : string =
         match output_dir with
         | Some d -> d
         | None ->
           let curr_dir = Sys_unix.getcwd () in
           curr_dir ^ "/_site"
       in
       (* Load cache and pass it to the pipeline builder *)
       let cache = Code_executor.load_cache ~dir:output_dir in
       let pipeline : Pipeline.t = pipeline_of_profile ~cache config.pipeline_profile in
       let results =
         render_vault ~pipeline ~theme ~backend_blocks:true ~safe:false vault_root
       in
       Code_executor.save_cache cache ~dir:output_dir;
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
       let all_entries = Vault.list_entries vault_root in
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
  Command.group ~summary:"Oystermark renderer" ~version:"0.1.0"
    [ "file", file_cmd; "vault", vault_cmd ]
  |> Command_unix.run
;;
