(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pipeline of the compiler *)

(* Author: Piotr Polesiuk, 2023 *)

let dump_core = ref false

let dump_sexpr flag to_sexpr p =
  if flag then
    SExpr.pretty_stdout (to_sexpr p);
  p

let check_invariant inv p =
  inv p;
  p

let common_pipeline prog =
  prog
  |> TypeInference.Main.tr_program
  |> ToCore.Main.tr_program
  |> dump_sexpr !dump_core Lang.Core.to_sexpr
  |> check_invariant Lang.Core.check_well_typed
  |> TypeErase.tr_program
  |> Eval.eval_program

let run_repl () =
  Parser.Main.repl
  |> common_pipeline

let run_file fname =
  Parser.Main.parse_file fname
  |> common_pipeline
