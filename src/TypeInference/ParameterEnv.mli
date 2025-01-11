(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Additional environment used in type-checking definition blocks. It stores
  information about declared parameters. *)

open Common

type t

(** List of named that can be implicitly generalized. It can be created by
  [begin_generalize] function and consumed by [end_generalize] functions. *)
type param_list

(** Empty environment *)
val empty : t

(** Prepare environment for generalizing named parameters: it adds parameters
  parameters to the environment and increases the level of the environment. *)
val begin_generalize : Env.t -> t -> Env.t * param_list

(** Build a list of types and named parameters that should be implicitly
  generalized. The second parameter is a set of unification variables that
  appears in the type/scheme of the generalized entity. Additionally, the
  function modifies list of constraints generated in the generalized entity:
  it partially solve them, and for the reamining, it extends their scope with
  unification variables promoted to type variables. This function tries to
  generalize only those parameters that were used. *)
val end_generalize_pure :
  param_list -> T.UVar.Set.t -> Constr.t list ->
    T.named_tvar list * (T.name * T.var * T.scheme_expr) list * Constr.t list

(** Ensure, that no named parameters on a given list were used. After calling
  this function, given named parameter become unavailable. *)
val end_generalize_impure : param_list -> unit

(** Enclose generalization and declare a named parameter. *)
val end_generalize_declare :
  pos:Position.t -> param_list -> t -> S.name -> S.ident -> T.scheme_expr -> t

(** Extend environment with a declaration of type parameter *)
val declare_type : pos:Position.t -> t -> S.tname -> S.tvar -> T.kind -> t

(** Shadow type with given name. Shadowed type is still present on parameter
  list, but are not accessible by name. *)
val shadow_type : t -> T.tname -> t

(** Extend an environment with a type variable. It may shadow some type
  parameters. *)
val add_tvar : pos:Position.t -> public:bool ->
  Env.t -> t -> S.tvar -> T.kind -> Env.t * t * T.tvar

(** Extend an environment with a polymorphic identifier. It may shadow named
  parameters. *)
val add_poly_id : pos:Position.t -> public:bool ->
  Env.t -> t -> S.ident -> T.scheme -> Env.t * t * T.var

(** Extend an environment with a monomorphic identifier. It may shadow named
  parameters. *)
val add_mono_id : pos:Position.t -> public:bool ->
  Env.t -> t -> S.ident -> T.typ -> Env.t * t * T.var

(** Extend an environment with information that given identifier when used
  as function is a method of given name. *)
val add_method_fn : public:bool -> Env.t -> t -> S.var -> S.method_name ->
  Env.t * t

(** Extend an environment with a constructor of given ADT and index. *)
val add_ctor : public:bool ->
  Env.t -> t -> S.ctor_name -> int -> Module.adt_info -> Env.t * t

(** Open given module, shadowing named parameters *)
val open_module : public:bool -> Env.t -> t -> Module.t -> Env.t * t

(** Extend environment with variables bound by given partial environment.
  It may shadow named parameters *)
val add_partial_env : Env.t -> t -> PartialEnv.t -> Env.t * t
