(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of ADT-shape proof expressions. *)

open Common

(** Translate a proof expression to a computationally irrelevant expression.
  Returns the translated expression, the type that is ADT, the list of its
  constructors, and the effect of pattern-matching on this ADT. *)
val tr_proof_expr :
  Env.t -> S.proof_expr -> T.expr * T.typ * T.ctor_decl list * T.ceffect
