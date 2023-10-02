(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Unif language: result of type-inference.
  The main feature of the Unif language is a support for type-unification *)

(* Author: Piotr Polesiuk, 2023 *)

include module type of SyntaxNode.Export

(** Kinds.

  This is an abstract type. Use [Kind.view] to view it. *)
type kind

(** Unification variables *)
type uvar

(** Type variables *)
type tvar

(** Types.

  This is an abstract type. Use [Type.view] to view it. *)
type typ

(** Effects. They are represented as types of effect kind *)
type effect = typ

(** Polymorphic type scheme *)
type scheme = {
  sch_tvars : tvar list;
    (** universally quantified type variables *)
  
  sch_body  : typ
    (** Body of the type scheme *)
}

(** Type substitution *)
type subst

(* ========================================================================= *)
(** Variable *)
type var = Var.t

(** Expression *)
type expr = expr_data node
and expr_data =
  | EUnit
    (** Unit expression *)

  | EVar of var
    (** Variable *)

  | EPureFn of var * typ * expr
    (** Pure lambda-abstraction *)

  | EFn of var * typ * expr
    (** Impure lambda-abstraction *)

  | ETFun of tvar * expr
    (** Type function *)

  | EApp of expr * expr
    (** Function application *)

  | ETApp of expr * typ
    (** Type application *)

  | ELet of var * scheme * expr * expr
    (** Let-definition *)

  | ERepl of (unit -> expr) * effect
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      This constructor stores also an effect of a REPL expression. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Program *)
type program = expr

(* ========================================================================= *)
(** Operations on kinds *)
module Kind : sig
  (** View of kinds *)
  type kind_view =
    | KType
      (** Kind of all types *)

    | KEffrow
      (** Kind of all effect rows *)

  (** Kind of all types *)
  val k_type : kind

  (** Kind of all effect rows. *)
  val k_effrow : kind

  (** Reveal a top-most constructor of a kind *)
  val view : kind -> kind_view
end

(* ========================================================================= *)
(** Operations on type variables *)
module TVar : sig
  (** Kind of a type variable *)
  val kind : tvar -> kind

  (** Create fresh type variable of given kind *)
  val fresh : kind -> tvar

  (** Check type variables for equality *)
  val equal : tvar -> tvar -> bool

  (** Finite map from type variables *)
  module Map : Map.S with type key = tvar
end

(* ========================================================================= *)
(** Operations on unification variables *)
module UVar : sig
  type t = uvar

  (** Check unification variables for equality *)
  val equal : t -> t -> bool

  (** Set the value of a unification variable. It can be done at most once for
    each variable *)
  val set : t -> typ -> unit

  (** Promote unification variable to fresh type variable *)
  val fix : t -> tvar

  (** Set of unification variables *)
  module Set : Set.S with type elt = t
end

(* ========================================================================= *)
(** Operations on types *)
module Type : sig
  (** View of a type *)
  type type_view =
    | TUnit
      (** Unit type *)

    | TRowPure
      (** Pure effect row *)

    | TUVar of uvar
      (** Unification variable *)

    | TVar of tvar
      (** Regular type variable *)

    | TPureArrow of typ * typ
      (** Pure arrow, i.e., type of fuction that doesn't perform any effects
        and always terminate *)

    | TArrow of typ * typ * effect
      (** Impure arrow *)

  (** Unit type *)
  val t_unit : typ

  (** Unification variable *)
  val t_uvar : uvar -> typ

  (** Regular type variable *)
  val t_var : tvar -> typ

  (** Pure arrow type *)
  val t_pure_arrow : typ -> typ -> typ

  (** Arrow type *)
  val t_arrow : typ -> typ -> effect -> typ

  (** Fresh unification variable (packed as type) *)
  val fresh_uvar : kind -> typ

  (** Reveal a top-most constructor of a type *)
  val view : typ -> type_view

  (** Check if given unification variable appears in given type. It is
    equivalent to [UVar.mem u (uvars tp)], but faster. *)
  val contains_uvar : uvar -> typ -> bool

  (** Set of unification variables in given type *)
  val uvars : typ -> UVar.Set.t

  (** Extend given set of unification variables by unification variables
    from given type. Equivalent to [uvars tp UVar.Set.empty] *)
  val collect_uvars : typ -> UVar.Set.t -> UVar.Set.t

  (** Apply substitution to a type *)
  val subst : subst -> typ -> typ
end

(* ========================================================================= *)
(** Operations on effects *)
module Effect : sig
  (** View of effect *)
  type effect_view =
    | EffPure
      (** Pure effect *)

    | EffUVar of uvar
      (** Row unification variable *)

    | EffVar  of tvar
      (** Row variable *)

  (** Pure effect row *)
  val pure_row : effect

  (** Row with single IO effect *)
  val io_row : effect

  (** View effect row *)
  val view : effect -> effect_view
end

(* ========================================================================= *)
(** Type substitutions *)
module Subst : sig
  (** Empty substitution *)
  val empty : subst

  (** Extend substitution. It is rather parallel extension than composition *)
  val add_type : subst -> tvar -> typ -> subst
end

(* ========================================================================= *)
(** Operations on type schemes *)
module Scheme : sig
  (** Set of unification variables in given scheme *)
  val uvars : scheme -> UVar.Set.t

  (** Extend given set of unification variables by unification variables
    from given scheme. Equivalent to [uvars sch UVar.Set.empty] *)
  val collect_uvars : scheme -> UVar.Set.t -> UVar.Set.t
end
