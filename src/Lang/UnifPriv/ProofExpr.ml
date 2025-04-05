(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on proof expressions. *)

open Syntax

let subst sub (e : proof_expr) =
  match e with
  | PE_Unit        -> PE_Unit
  | PE_Option tp   -> PE_Option (Subst.in_type sub tp)
  | PE_Var(x, tps) -> PE_Var(x, List.map (Subst.in_type sub) tps)
