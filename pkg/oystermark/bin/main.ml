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
       let pipeline = Oystermark.default_pipeline in
       let all_files = Oystermark.Vault.list_files vault_root in
       let rel_path =
         match String.chop_prefix file ~prefix:(vault_root ^ "/") with
         | Some rel -> rel
         | None -> file
       in
       (* Build vault with pipeline to get index *)
       let other_files =
         List.filter all_files ~f:(fun p -> not (String.is_suffix p ~suffix:".md"))
       in
       (* Read + parse all files for index *)
       let parsed =
         List.filter_map all_files ~f:(fun rp ->
           if not (String.is_suffix rp ~suffix:".md")
           then None
           else (
             let full_path = Filename.concat vault_root rp in
             let content = In_channel.read_all full_path in
             let { Parse.Frontmatter.yaml; body } = Parse.Frontmatter.of_string content in
             match pipeline.on_frontmatter rp yaml with
             | None -> None
             | Some yaml' ->
               let cmarkit_doc = Cmarkit.Doc.of_string ~strict:false body in
               let doc = Cmarkit.Mapper.map_doc Parse.mapper cmarkit_doc in
               let pdoc : Parse.doc =
                 { doc; frontmatter = yaml'; meta = Cmarkit.Meta.none }
               in
               (match pipeline.on_parse rp pdoc with
                | None -> None
                | Some pdoc' -> Some (rp, pdoc'))))
       in
       let index = Oystermark.Vault.build_index ~md_docs:parsed ~other_files in
       let vault_ctx : Oystermark.Pipeline.vault_ctx =
         { vault_root; index; docs = parsed; vault_meta = Cmarkit.Meta.none }
       in
       let target_doc =
         List.Assoc.find parsed ~equal:String.equal rel_path
         |> Option.value_exn ~message:(sprintf "File %s not found in vault" rel_path)
       in
       match pipeline.on_index vault_ctx rel_path target_doc with
       | None -> eprintf "File %s is a draft, skipping.\n" rel_path
       | Some final ->
         print_string
           (Oystermark.Html.of_doc ~safe:true ~frontmatter:final.frontmatter final.doc))
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
       let results = Oystermark.render_vault ~safe:true vault_root in
       List.iter results ~f:(fun (rel_path, html) ->
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
