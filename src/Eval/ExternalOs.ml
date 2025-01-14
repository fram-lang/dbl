(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value
open ExternalHelpers

let getEnv s =
  Option.map (fun x -> VStr x) (Sys.getenv_opt s)
  |> of_option

let str_fun_try f = VFn (fun v cont ->
  match v with
  | VStr s ->
    cont (try f s
    with Sys_error s ->
      runtime_error s)
  | _ -> runtime_error "Not a string")

let getArgv () =
  let argv = Array.of_list !DblConfig.prog_args in
  VArray (Array.map (fun s -> VStr s) argv)

let extern_os_seq =
  [ "dbl_argv",        unit_fun getArgv;
    "dbl_getEnv",      str_fun getEnv;
    "dbl_fileExists",  str_fun (fun s -> of_bool (Sys.file_exists s));
    "dbl_isDirectory", str_fun_try (fun s -> of_bool (Sys.is_directory s));
    "dbl_isRegularFile",   str_fun_try (fun s -> of_bool (Sys.is_regular_file s));
    "dbl_removeFile",      str_fun (fun s -> Sys.remove s; v_unit);
    "dbl_renameFile",      str_fun (fun s1 ->
      str_fun (fun s2 -> Sys.rename s1 s2; v_unit));
    "dbl_changeDirectory", str_fun (fun s -> Sys.chdir s; v_unit);
    "dbl_removeDirectory", str_fun (fun s -> Sys.rmdir s; v_unit);
    "dbl_makeDirectory", str_fun (fun s ->
      int_fun (fun i -> Sys.mkdir s i; v_unit));
    "dbl_readDirectory", str_fun (fun s ->
      VArray (Sys.readdir s |> Array.map (fun s -> VStr s)));
    "dbl_getCurrentDirectory", unit_fun (fun () -> VStr (Sys.getcwd ()));
    (* TODO: remove of_float after adding floats *)
    "dbl_getUnixTime", unit_fun (fun () -> VNum (Unix.time () |> Int.of_float))
  ] |> List.to_seq

