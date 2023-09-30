(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pipeline of the compiler *)

(* Author: Piotr Polesiuk, 2023 *)

let common_pipeline prog =
  prog
  |> TypeInference.Main.tr_program
  (* TODO: not implemented  *)
  |> ignore

let run_repl () =
  (* TODO: not implemented *)
  ()

let run_file fname =
  Parser.Main.parse_file fname
  |> common_pipeline
