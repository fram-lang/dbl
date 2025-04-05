(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of expressions *)

open Common

(** Infer type of the expression. Effect-checking is in the bidirectional
  style. *)
val infer_type :
  Env.t -> S.expr -> (T.ceffect, 'ed) request ->
    T.expr * T.typ * (T.ceffect, 'ed) response

(** Check the type of the expression. Effect-checking is in the bidirectional
  style. *)
val check_type :
  Env.t -> S.expr -> T.typ -> (T.ceffect, 'ed) request ->
    T.expr * (T.ceffect, 'ed) response
