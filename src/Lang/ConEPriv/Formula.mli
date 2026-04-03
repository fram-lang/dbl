(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of formulas of a logic enriched with effect
  modes.

This module contains almost all of the logic related to effect modes. In
general, the original effect inference algorithm described in the "Deciding
not to decide" paper scales well to effect modes: it is enough to replace the
underlying propositional logic (with positive formulas only) with a logic
enriched with some additional constants (representing effect modes). Such
a logic can be easily encoded using standard positive propositional logic
(the [IncrSAT.Formula] module). See the implementation for details. *)

(** The type of formulas. *)
type t

(** Always true formula *)
val top : t

(* Always false formula *)
val bot : t

(** Create a formula that represents a given effect mode. *)
val of_mode : EffectMode.t -> t

(** Formula built from a single fresh propositional variable *)
val fresh_var : unit -> t

(** Conjunction of two formulas *)
val conj : t -> t -> t

(** Disjunction of two formulas *)
val disj : t -> t -> t

(** Check if formula is trivially true *)
val is_true : t -> bool

(** Check if formula is trivially false *)
val is_false : t -> bool

(** Check if one formula trivially implies the other *)
val implies : t -> t -> bool

(** Set values of some propositional variables in order to fix the value
  of the formula. Return the value of the formula *)
val fix : t -> bool

(** Convert implication to CNF, i.e., conjunction of disjunctions of
  literals. The boolean flag at each literal describes polarity: [false]
  means that variable is negated. *)
val imp_to_cnf : t -> t -> (IncrSAT.PropVar.t * bool) list list

(** Pretty-print formula as S-expression *)
val to_sexpr : t -> SExpr.t

(** Convert formula to a list of possible modes, used for pretty-printing.
  Returns a list of pairs of a mode (to which the effect should be projected)
  and a boolean flag describing whether the effect is certainly present
  (i.e., it is not guarded by a formula that is not trivially satisfied). The
  optional [mode] parameter describe the implicit mode of the effect, i.e., if
  the [mode] is [Affine], the function will return [Unrestricted], instead of
  [Affine], because the effect is implicitly projected to [Affine], so there
  is no need of printing it. *)
val to_mode_list : ?mode:EffectMode.t -> t -> (EffectMode.t * bool) list
