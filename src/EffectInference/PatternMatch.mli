(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of deep pattern-matching *)

open Common

(** Clause of a pattern-matching. *)
type clause = {
  cl_pos   : Position.t;
    (** Position of the clause *)

  cl_pat   : Pattern.t;
    (** Pattern of the clause *)

  cl_body  : T.expr;
    (** Body of the clause. This expression is a function that should be
      applied to [cl_tvars], [cl_vars], and the unit value. *)

  cl_tvars : T.tvar list;
    (** Type variables bound by the pattern *)

  cl_vars  : T.var list
    (** Regular variables bound by the pattern *)
}

(** Translate a pattern-matching, assuming that the the result of the
  translation has type [tp] and effect [eff]. *)
val tr_match :
  pos:Position.t -> T.expr ->
    tp:T.typ -> eff:T.ceffect -> clause list -> T.expr
