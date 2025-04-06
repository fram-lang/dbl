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

let display_error_context = ref true

let print_colors_auto () =
  Unix.isatty Unix.stdout

let display_colors = ref (print_colors_auto ())

let print_colors_of_string = function
  | "always" -> display_colors := true
  | "never"  -> display_colors := false
  | "auto"   -> display_colors := print_colors_auto ()
  | _ -> assert false


let compile_glob = Dune_glob.V1.of_string

let test_globs : Dune_glob.V1.t list ref = ref [ ]
let test_tagless = ref false

let test_active (tags : string list) =
  match tags with
  | [] -> !test_tagless
  | _ -> List.exists (fun s -> List.exists (fun g -> Dune_glob.V1.test g s) !test_globs) tags
