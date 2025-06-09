(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the effect inference *)

open Common

(** Information about variable *)
type var_info =
  | Simple of T.scheme
    (** Variable with a simple polymorphic scheme *)

  | Large  of T.tvar list * T.constr list * T.scheme
    (** Variable with a large scheme, i.e., it abstracts over extra effect
      variables and effect constraints. *)


type adt_info =
  { adt_proof    : T.var;
    adt_args     : T.named_tvar list;
    adt_type     : T.typ; (* Already applied to [adt_args] *)
    adt_ctors    : T.ctor_decl list;
    adt_positive : bool
  }

type t

(** Initial environment that contains built-in types *)
val initial : solve_all:bool -> unit -> t

(** Add a type variable to the environment. It returns the extended environment,
  and the refreshed type variable in ConE representation. *)
val add_tvar : t -> S.tvar -> t * T.tvar

(** Add named type variables to the environment. It returns the extended
  environment, and the list of refreshed type variables in ConE
  representation. *)
val add_named_tvars : t -> S.named_tvar list -> t * T.named_tvar list

(** Add the datatype definition to the environment. *)
val add_data : t -> S.var -> adt_info -> t

(** Add a type alias to the environment. *)
val add_type_alias : t -> S.ty_alias -> T.typ -> t

(** Add simple polymorphic variable to the environment. *)
val add_poly_var : t -> S.var -> T.scheme -> t

(** Add variable with a monomorphic type to the environment. *)
val add_mono_var : t -> S.var -> T.typ -> t

(** Add variable with a large scheme *)
val add_lpoly_var : t -> S.var -> T.tvar list -> T.constr list -> T.scheme -> t

(** Add a recursive variable to the environment. It will be visible only after
  the call to [commit_rec_context]. *)
val add_rec_var : t -> S.var -> T.scheme -> t

(** Lookup a type variable in the environment. We are after the type-inference
  phase, so we should always find the type variable. *)
val lookup_tvar : t -> S.tvar -> T.tvar

(** Lookup a variable in the environment. *)
val lookup_var : t -> S.var -> var_info

(** Lookup an ADT in the environment. *)
val lookup_adt : t -> S.var -> adt_info

(** Lookup a type alias in the environment. *)
val lookup_type_alias : t -> S.ty_alias -> T.typ

(** Enter a new scope. The returned environment has its own set of constraints.
  When the scope is exited, the constraints should be obtained using
  [constraints] function, and (after some processing) should be added to
  the parent environment. *)
val enter_scope : t -> t

(** Enter a new recursive context. In the recursive context it is possible to
  add recursive variables [add_rec_var], but they become visible only after
  the call to [commit_rec_context]. The recursive context should not be
  nested. *)
val enter_rec_context : t -> t

(** Commit the recursive context. The recursive variables become visible in the
  environment. *)
val commit_rec_context : t -> t

(** Add a list of constraints to the environment. *)
val add_constraints : t -> Constr.t list -> unit

(** Get the list of constraints collected in the current scope *)
val constraints : t -> Constr.t list

(** Clear the list of constraints. *)
val clear_constraints : t -> unit

(** Add formula implication constraint to the SAT solver *)
val add_formula_constraint :
  t -> origin:origin -> T.tvar -> T.formula -> T.formula -> unit

(** Get the global SAT-solver *)
val sat_solver : t -> (T.tvar * origin) IncrSAT.Solver.t

(** Check if [solve_all] flag was set during environment initialization. *)
val solve_all_is_set : t -> bool

(** Get the current scope *)
val scope : t -> Scope.t

(** Get the list of all type variables stored in the environment *)
val all_tvars : t -> T.tvar list

(** Generate a fresh generalizable variable and convert it into an effect *)
val fresh_gvar : t -> T.effct
