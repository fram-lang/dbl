(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to type-inference. *)

(* Author: Piotr Polesiuk, 2023 *)

type t = unit

let fatal () =
  InterpLib.Error.incr_error_counter ();
  raise InterpLib.Error.Fatal_error

let report () =
  InterpLib.Error.incr_error_counter ()

let unbound_var ~pos x =
  Printf.eprintf "%s: error: Unbound variable `%s'\n"
    (Position.to_string pos) x

let expr_type_mismatch ~pos ~env tp1 tp2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Type mismatch\n"
    (Position.to_string pos)

let expr_effect_mismatch ~pos ~env eff1 eff2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Effect mismatch\n"
    (Position.to_string pos)

let func_effect_mismatch ~pos ~env eff1 eff2 =
  (* TODO: better message *)
  Printf.eprintf "%s: error: Function effect mismatch\n"
    (Position.to_string pos)

let func_not_pure ~pos =
  Printf.eprintf
    "%s: error: Cannot ensure that this function is pure and always terminates."
    (Position.to_string pos)

let expr_not_function ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: This expression is not a function and cannot be applied.\n"
    (Position.to_string pos)

let expr_not_function_ctx ~pos ~env tp =
  (* TODO: better message *)
  Printf.eprintf 
    "%s: error: This expresion should not be a function.\n"
    (* the expected type is tp *)
    (Position.to_string pos)

let type_escapes_its_scope ~pos ~env x =
  (* TODO: better message *)
  Printf.eprintf
    "%s: error: Something escapes its scope here.\n"
    (Position.to_string pos)
