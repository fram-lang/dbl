(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Positive formulas *)

type t

(** Always true formula *)
val top : t

(* Always false formula *)
val bot : t

(** Propositional variable as a formula *)
val var : PropVar.t -> t

(** Formula build from a single fresh propositional variable *)
val fresh_var : unit -> t

(** Conjunction of two formuals *)
val conj : t -> t -> t

(** Disjunction of two formulas *)
val disj : t -> t -> t

(** Check if formula is trivially true *)
val is_true : t -> bool

(** Check if formula is trivially false *)
val is_false : t -> bool

(** Check if one formula trivially implies the other *)
val implies : t -> t -> bool

(** Set some propositional variables in order to fix its value. Return the
  value of the formula *)
val fix : t -> bool

(** Convert implication to CNF, i.e, conjunction of disjunctions of litarals.
  The boolean flag at each literal describes polarity: [false] means that
  variable is negated. *)
val imp_to_cnf : t -> t -> (PropVar.t * bool) list list

(** Pretty-print formula as S-expression *)
val to_sexpr : t -> SExpr.t
