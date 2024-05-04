(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pipeline of the compiler *)

let dump_core = ref false

let use_prelude = ref true

let use_stdlib = ref true

let dump_sexpr flag to_sexpr p =
  if flag then
    SExpr.pretty_stdout (to_sexpr p);
  p

let check_invariant check inv p =
  if check
  then inv p;
  p

let set_module_dirs ?fname () =
  if !use_stdlib then
    Config.lib_search_dirs := Config.stdlib_path :: !Config.lib_search_dirs;
  let cur_dir =
    match fname with
    | Some fname -> Filename.dirname fname
    | None       -> Filename.current_dir_name
  in
  Config.local_search_dirs := cur_dir :: !Config.local_search_dirs

let common_pipeline repl_mode prog =
  prog
  |> TypeInference.Main.tr_program
  |> ToCore.Main.tr_program ~repl_mode
  |> dump_sexpr !dump_core Lang.Core.to_sexpr
  |> check_invariant (not repl_mode) Lang.Core.check_well_typed
  |> TypeErase.tr_program
  |> Eval.eval_program

let run_repl () =
  set_module_dirs ();
  Parser.Main.repl ~use_prelude:!use_prelude
  |> common_pipeline true

let run_file fname =
  set_module_dirs ~fname ();
  Parser.Main.parse_file ~use_prelude:!use_prelude fname
  |> common_pipeline false
