(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of pattern-matching *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Translate a pattern-matching of a single value. Clauses are represented
  as pairs of pattern and translating function that produces expression for
  given environment. Function takes also the type and the effect of the whole
  pattern-matching expression. *)
val tr_single_match :
  pos:Position.t -> env:Env.t ->
  T.value -> (S.pattern * (Env.t -> T.expr)) list ->
  T.ttype -> T.effect -> T.expr
