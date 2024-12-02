(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of pattern-matching *)
(*
open Common

(** Translate a pattern-matching of a single value.
  This function takes also translation function from Unif to Core, the type,
  and the effect of the whole pattern-matching expression. *)
val tr_single_match :
  pos:Position.t -> env:Env.t ->
  tr_expr:(Env.t -> S.expr -> T.expr) ->
  T.value -> S.match_clause list -> T.ttype -> T.effect -> T.expr
*)
