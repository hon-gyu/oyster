open Core

let () =
  let md = Sys.get_argv () |> Array.last in
  let title =
    String.chop_suffix_exn md ~suffix:".md"
    |> String.split ~on:'/'
    |> List.last_exn
    |> List.return
    |> List.concat_map ~f:(fun s -> String.split s ~on:'_')
    |> List.concat_map ~f:(fun s -> String.split s ~on:'-')
    |> List.map ~f:(fun s -> String.capitalize s)
    |> String.concat ~sep:" "
  in
  let open Shexp_process in
  let open Shexp_process.Infix in
  eval (printf "{0 %s}\n" title >> printf "{v\n" >> run "cat" [ md ] >> printf "v}\n")
;;
