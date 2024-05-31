(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Global configuration of the interpreter *)

let src_extension = ".fram"

let stdlib_path =
  match Sys.getenv_opt "DBL_LIB" with
  | Some path -> path
  | None      ->
    begin match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
    | Some path ->
      List.fold_left Filename.concat path [ "lib"; "dbl"; "stdlib" ]
    | None      -> Filename.concat Filename.current_dir_name "lib"
    end

let local_mod_prefix = "Main"

let lib_search_dirs   : string list ref = ref [ ]
let local_search_dirs : string list ref = ref [ ]
