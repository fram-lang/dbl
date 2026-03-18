(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pipeline of the compiler *)

let dump_cone = ref false
let dump_core = ref false

let use_prelude = ref true

let use_stdlib = ref true

let timings = ref false

let dump_sexpr flag to_sexpr p =
  if flag then
    SExpr.pretty_stdout (to_sexpr p);
  p

let print_timing label f arg =
  if not !timings then f arg
  else
    let start = Unix.gettimeofday () in
    let at_end () =
      Printf.eprintf "[timing] %s:  %.3fs\n%!" label
        (Unix.gettimeofday () -. start)
    in
    match f arg with
    | r -> at_end (); r
    | exception ex -> at_end (); raise ex

let check_invariant check inv p =
  if check
  then inv p;
  p

let set_module_dirs ?fname () =
  if !use_stdlib then
    DblConfig.lib_search_dirs :=
      DblConfig.stdlib_path :: !DblConfig.lib_search_dirs;
  let cur_dir =
    match fname with
    | Some fname -> Filename.dirname fname
    | None       -> Filename.current_dir_name
  in
  DblConfig.local_search_dirs := cur_dir :: !DblConfig.local_search_dirs

let core_pipeline prog =
  prog
  |> print_timing "type inference" TypeInference.Main.tr_program
  |> print_timing "effect inference"
       (EffectInference.Main.tr_program ~solve_all:true)
  |> dump_sexpr !dump_cone Lang.ConE.to_sexpr
  |> print_timing "to core" ToCore.Main.tr_program
  |> dump_sexpr !dump_core Lang.Core.to_sexpr
  |> check_invariant true
       (print_timing "second type check" Lang.Core.check_well_typed)
  |> print_timing "type erasure" CoreTypeErase.tr_program
  |> print_timing "evaluation" Eval.eval_program

let nocore_pipeline prog =
  prog
  |> print_timing "type inference" TypeInference.Main.tr_program
  |> print_timing "effect inference"
       (EffectInference.Main.tr_program ~solve_all:false)
  |> dump_sexpr !dump_cone Lang.ConE.to_sexpr
  |> print_timing "type erasure" ConETypeErase.tr_program
  |> print_timing "evaluation" Eval.eval_program

let run_repl () =
  set_module_dirs ();
  DblParser.Main.repl ~use_prelude:!use_prelude
  |> nocore_pipeline

let run_file fname =
  set_module_dirs ~fname ();
  DblParser.Main.parse_file ~use_prelude:!use_prelude fname
  |> core_pipeline
