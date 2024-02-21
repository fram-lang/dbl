(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Global configuration of the interpreter *)

(* 2024: Patrycja Balik: initial implementation *)

let prelude_path =
  let prelude_filename = "Prelude.dbl" in
  match Sys.getenv_opt "DBL_LIB" with
  | Some path -> Filename.concat path prelude_filename
  | None      ->
    begin match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
    | Some path ->
      List.fold_left
        Filename.concat
        path [ "lib"; "dbl"; "stdlib"; prelude_filename ]
    | None      -> "./lib"
    end
