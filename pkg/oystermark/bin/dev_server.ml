open Core

(** Content-type lookup for static file serving. *)
let content_type_of_path (path : string) : string =
  match Filename.split_extension path |> snd with
  | Some "html" -> "text/html; charset=utf-8"
  | Some "css" -> "text/css; charset=utf-8"
  | Some "js" -> "application/javascript"
  | Some "json" -> "application/json"
  | Some "png" -> "image/png"
  | Some ("jpg" | "jpeg") -> "image/jpeg"
  | Some "gif" -> "image/gif"
  | Some "svg" -> "image/svg+xml"
  | Some "ico" -> "image/x-icon"
  | Some "woff" -> "font/woff"
  | Some "woff2" -> "font/woff2"
  | Some "ttf" -> "font/ttf"
  | Some "pdf" -> "application/pdf"
  | _ -> "application/octet-stream"
;;

(** Serve static files from [dir] on [port] using cohttp-eio. *)
let serve ~(env : Eio_unix.Stdenv.base) ~port ~dir =
  let callback _conn (req : Http.Request.t) _body =
    let resource = req.resource in
    let path =
      if String.is_suffix resource ~suffix:"/" then resource ^ "index.html" else resource
    in
    (* Prevent path traversal *)
    let path = String.substr_replace_all path ~pattern:".." ~with_:"" in
    let file_path = dir ^ path in
    match Sys_unix.file_exists file_path with
    | `Yes when Sys_unix.is_directory_exn file_path ->
      (* Redirect to path with trailing slash *)
      let headers = Http.Header.of_list [ "location", resource ^ "/" ] in
      Cohttp_eio.Server.respond
        ~headers
        ~status:`Moved_permanently
        ~body:(Cohttp_eio.Body.of_string "")
        ()
    | `Yes ->
      let content = In_channel.read_all file_path in
      let ct = content_type_of_path file_path in
      let headers = Http.Header.of_list [ "content-type", ct ] in
      Cohttp_eio.Server.respond
        ~headers
        ~status:`OK
        ~body:(Cohttp_eio.Body.of_string content)
        ()
    | _ ->
      Cohttp_eio.Server.respond
        ~status:`Not_found
        ~body:(Cohttp_eio.Body.of_string "Not Found")
        ()
  in
  let server = Cohttp_eio.Server.make ~callback () in
  Eio.Switch.run
  @@ fun sw ->
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket =
    Eio.Net.listen (Eio.Stdenv.net env) ~sw ~backlog:128 ~reuse_addr:true addr
  in
  printf "Serving %s on http://localhost:%d\n%!" dir port;
  Cohttp_eio.Server.run
    ~on_error:(fun exn -> eprintf "Server error: %s\n%!" (Exn.to_string exn))
    socket
    server
;;

(** Collect mtime map for all files under [dir], recursively. *)
let rec scan_mtimes (dir : string) : (string * float) list =
  match Sys_unix.ls_dir dir with
  | entries ->
    List.concat_map entries ~f:(fun entry ->
      let path = Filename.concat dir entry in
      match Sys_unix.is_directory path with
      | `Yes -> scan_mtimes path
      | _ ->
        (match Core_unix.stat path with
         | stat -> [ path, stat.st_mtime ]
         | exception _ -> [])
      | exception _ -> [])
  | exception _ -> []
;;

(** Watch [watch_dir] for changes and call [on_change] when detected. *)
let watch ~(env : Eio_unix.Stdenv.base) ~watch_dir ~on_change =
  let prev = ref (scan_mtimes watch_dir) in
  while true do
    Eio.Time.sleep (Eio.Stdenv.clock env) 1.0;
    let curr = scan_mtimes watch_dir in
    if
      not
        (List.equal
           (fun (p1, t1) (p2, t2) -> String.equal p1 p2 && Float.equal t1 t2)
           curr
           !prev)
    then (
      prev := curr;
      printf "\nChange detected, re-rendering...\n%!";
      on_change ();
      printf "Done.\n%!")
  done
;;
