(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pipeline of the compiler *)

let dump_cone = ref false
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
    DblConfig.lib_search_dirs := DblConfig.stdlib_path :: !DblConfig.lib_search_dirs;
  let cur_dir =
    match fname with
    | Some fname -> Filename.dirname fname
    | None       -> Filename.current_dir_name
  in
  DblConfig.local_search_dirs := cur_dir :: !DblConfig.local_search_dirs

let core_pipeline prog =
  prog
  |> TypeInference.Main.tr_program
  |> EffectInference.Main.tr_program ~solve_all:true
  |> dump_sexpr !dump_cone Lang.ConE.to_sexpr
  |> ToCore.Main.tr_program
  |> dump_sexpr !dump_core Lang.Core.to_sexpr
  |> check_invariant true Lang.Core.check_well_typed
  |> CoreTypeErase.tr_program
  |> Eval.eval_program

let nocore_pipeline prog =
  prog
  |> TypeInference.Main.tr_program
  |> EffectInference.Main.tr_program ~solve_all:false
  |> dump_sexpr !dump_cone Lang.ConE.to_sexpr
  |> ConETypeErase.tr_program
  |> Eval.eval_program

let run_repl () =
  set_module_dirs ();
  DblParser.Main.repl ~use_prelude:!use_prelude
  |> nocore_pipeline

let run_file fname =
  set_module_dirs ~fname ();
  DblParser.Main.parse_file ~use_prelude:!use_prelude fname
  |> core_pipeline
