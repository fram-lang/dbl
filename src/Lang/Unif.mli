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

  | EFn of var * typ * expr
    (** Lambda-abstraction *)

  | ETFun of tvar * expr
    (** Type function *)

  | EApp of expr * expr
    (** Function application *)

  | ETApp of expr * typ
    (** Type application *)

  | ELet of var * scheme * expr * expr
    (** Let-definition *)

(** Program *)
type program = expr

(* ========================================================================= *)
(** Operations on kinds *)
module Kind : sig
  (** View of kinds *)
  type kind_view =
    | KType
      (** Kind of all types *)

  (** Kind of all types *)
  val k_type : kind

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

    | TUVar of uvar
      (** Unification variable *)

    | TVar of tvar
      (** Regular type variable *)

    | TArrow of typ * typ
      (** Arrow type *)

  (** Unit type *)
  val t_unit : typ

  (** Regular type variable *)
  val t_var : tvar -> typ

  (** Arrow type *)
  val t_arrow : typ -> typ -> typ

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
