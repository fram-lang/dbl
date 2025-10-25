(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions related to the REPL. *)

open Common
open BiDirectional
open TypeCheckFix

(** Build an expression that evaluates to the function converting a value
  of given type to its string representation. It tries to use [toString]
  method if available; otherwise, it falls back to default conversion,
  relying on an external function. *)
val to_string_expr : tcfix:tcfix -> pos:Position.t ->
  'st Env.t -> T.typ -> infer expr_result
