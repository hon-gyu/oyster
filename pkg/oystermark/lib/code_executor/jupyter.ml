open Core

(** Build a minimal [.ipynb] JSON with a Python 3 kernelspec.
    Each element of [cells] becomes one code cell; source is stored as a plain
    string (Jupyter's [multiline_string] format accepts both a bare string and
    an array of lines). *)
let make_notebook (cells : string list) =
  let make_cell source =
    `Assoc
      [ "cell_type", `String "code"
      ; "source", `String source
      ; "metadata", `Assoc []
      ; "outputs", `List []
      ; "execution_count", `Null
      ]
  in
  `Assoc
    [ "nbformat", `Int 4
    ; "nbformat_minor", `Int 5
    ; ( "metadata"
      , `Assoc
          [ ( "kernelspec"
            , `Assoc
                [ "display_name", `String "Python 3"
                ; "language", `String "python"
                ; "name", `String "python3"
                ] )
          ] )
    ; "cells", `List (List.map cells ~f:make_cell)
    ]
;;

(** Execute a notebook JSON via [uv run ... jupyter nbconvert].
    Dependencies in [uv_config] are passed as [--with <dep>] arguments so uv
    provisions an ephemeral virtual environment — no persistent venv needed.

    Implementation notes:
    - [JUPYTER_CONFIG_DIR=/dev/null] prevents local Jupyter config (e.g.
      contrib extensions) from breaking the clean uv environment.
    - [jupyter nbconvert --output] takes a base name and appends [.ipynb]
      itself, so we strip the extension from the temp output path and
      reconstruct it after the command. *)
let run_notebook
      ~(python_version : float)
      ~(with_args : string list)
      ~(nb_json : Yojson.Basic.t)
  : (Yojson.Basic.t, string) result
  =
  let tmp_in = Filename_unix.temp_file "nb_in" ".ipynb" in
  (* jupyter appends .ipynb to --output, so omit the extension here *)
  let tmp_out_base = Filename_unix.temp_file "nb_out" "" in
  let tmp_out = tmp_out_base ^ ".ipynb" in
  Yojson.Basic.to_file tmp_in nb_json;
  let with_str =
    "jupyter" :: with_args
    |> List.map ~f:(fun dep -> sprintf "--with %s" dep)
    |> String.concat ~sep:" "
  in
  let cmd =
    sprintf
      "JUPYTER_CONFIG_DIR=/dev/null uv run --python %g %s jupyter nbconvert --to \
       notebook --execute --allow-errors %s --output %s 2>/dev/null"
      python_version
      with_str
      tmp_in
      tmp_out_base
  in
  match Core_unix.system cmd with
  | Ok () ->
    let result = Yojson.Basic.from_file tmp_out in
    Sys_unix.remove tmp_in;
    Sys_unix.remove tmp_out_base;
    Sys_unix.remove tmp_out;
    Ok result
  | Error _ -> Error "nbconvert failed"
;;

(** Jupyter multiline_string: either a plain string or an array of strings *)
let multiline_string (j : Yojson.Basic.t) : string =
  let open Yojson.Basic.Util in
  match j with
  | `String s -> s
  | `List _ -> j |> to_list |> List.map ~f:to_string |> String.concat ~sep:""
  | _ -> ""
;;

(** Extract text from each output entry of a Jupyter code cell.
    Handles the four output types defined by nbformat:
    - [stream]: stdout/stderr text
    - [execute_result] / [display_data]: [text/plain] from the MIME bundle
    - [error]: formatted as ["ExcType: message"]
    Other output types are silently ignored. *)
let cell_outputs (cell : Yojson.Basic.t) : string list =
  let open Yojson.Basic.Util in
  cell
  |> member "outputs"
  |> to_list
  |> List.filter_map ~f:(fun output ->
    match output |> member "output_type" |> to_string with
    | "stream" -> Some (output |> member "text" |> multiline_string)
    | "execute_result" | "display_data" ->
      Some (output |> member "data" |> member "text/plain" |> multiline_string)
    | "error" ->
      let ename = output |> member "ename" |> to_string in
      let evalue = output |> member "evalue" |> to_string in
      Some (sprintf "%s: %s" ename evalue)
    | _ -> None)
;;

(** Return the outputs of every code cell in an executed notebook, in order.
    Each element of the returned list corresponds to one code cell and is
    itself a list of output strings (one per output entry). *)
let notebook_outputs (nb_json : Yojson.Basic.t) : string list list =
  let open Yojson.Basic.Util in
  nb_json
  |> member "cells"
  |> to_list
  |> List.filter ~f:(fun c -> String.equal (c |> member "cell_type" |> to_string) "code")
  |> List.map ~f:cell_outputs
;;
