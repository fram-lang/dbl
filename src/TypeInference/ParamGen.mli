(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Implicit generalization of section and polymorphic parameters. This module
  provides a convenient wrappers around functions from [ParamEnv] module. *)

open Common

(** Build a list of types and named parameters that should be implicitly
  generalized. The second parameter is a set of unification variables that
  appears in the type/scheme of the generalized entity. Additionally, the
  function modifies list of constraints generated in the generalized entity:
  it partially solve them, and for the reamining, it extends their scope with
  unification variables promoted to type variables. This function tries to
  generalize only those parameters that were used. *)
val end_generalize_pure :
  ParamEnv.param_list -> T.UVar.Set.t -> Constr.t list ->
    T.named_tvar list * (Name.t * T.var * T.scheme_expr) list * Constr.t list

(** Ensure, that no named parameters on a given list were used. After calling
  this function, given named parameter become unavailable. The fuction takes
  also a set of unification variables that appears in the entity that was
  created with the environment returned by [begin_generalize]. This set is
  needed to decrease the level of unification variables. *)
val end_generalize_impure : ParamEnv.param_list -> T.UVar.Set.t -> unit

(** Enclose generalization and declare a named parameter. *)
val end_generalize_declare : pos:Position.t ->
  ParamEnv.param_list -> ('st, sec) opn Env.t ->
    S.name -> S.ident -> T.scheme_expr -> ('st, sec) opn Env.t
