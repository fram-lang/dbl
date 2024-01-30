(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

type t

(** Information about ADT definition *)
type adt_info = {
  adt_proof : T.expr;
    (** A computationally irrelevant expression that give a proof that given
      type is an ADT. It is polymorphic in the type parameters of an ADT. *)

  adt_args  : T.named_tvar list;
    (** Type parameter of an ADT *)

  adt_ctors : T.ctor_decl list;
    (** List of constructors of an ADT *)

  adt_type  : T.typ
    (** The type that is an ADT, already applied to [adt_args] *)
}

(** Empty environment *)
val empty : t

(** Extend an environment with a polymorphic variable *)
val add_poly_var : t -> S.var -> T.scheme -> t * T.var

(** Extend an environment with a monomorphic variable *)
val add_mono_var : t -> S.var -> T.typ -> t * T.var

(** Extend an environment with a polymorphic named implicit.
  The last parameter is a function called on each use of implicit parameter *)
val add_poly_implicit :
  t -> S.iname -> T.scheme -> (Position.t -> unit) -> t * T.var

(** Extend an environment with a monomorphic named implicit.
  The last parameter is a function called on each use of implicit parameter *)
val add_mono_implicit :
  t -> S.iname -> T.typ -> (Position.t -> unit) -> t * T.var

(** Extend an environment with a variable labeled with "label". Such a
  variable always have a label type. All components of the label type should
  be passed as a parameters. *)
val add_the_label : t -> T.effect -> T.typ -> T.effect -> t * T.var

(** Extend an environment with a named type variable *)
val add_tvar : t -> S.tvar -> T.kind -> t * T.tvar

(** Extend an environment with a type variable labeled with "effect". Such
  a type always have [cleffect] kind. *)
val add_the_effect : t -> t * T.tvar

(** Extend an environment with an anonymous type variable *)
val add_anon_tvar : t -> T.kind -> t * T.tvar

(** Assign ADT definition to given type variable. *)
val add_data : t -> T.tvar -> adt_info -> t

(** Add constructor of given name and index to the environment *)
val add_ctor : t -> string -> int -> adt_info -> t

(** Lookup for Unif representation and a scheme of a variable. Returns [None]
  if variable is not bound. *)
val lookup_var : t -> S.var -> (T.var * T.scheme) option

(** Lookup for Unif representation, a scheme, and "on-use" function of a named
  implicit. Returns [None] if implicit is not bound. *)
val lookup_implicit :
  t -> S.var -> (T.var * T.scheme * (Position.t -> unit)) option

(** Lookup for variable labeled with "label". On success, returns the variable
  together with its label-type components *)
val lookup_the_label : t -> (T.var * T.effect * T.typ * T.effect) option

(** Lookup for a constructor of ADT. Returns [None] if there is no constructor
  with given name. On success return the index of the constructor and
  full information about ADT *)
val lookup_ctor : t -> S.ctor_name -> (int * adt_info) option

(** Lookup for Unif representation of a type variable. Returns [None] if
  variable is not bound. *)
val lookup_tvar : t -> S.tvar -> T.tvar option

(** Lookup for ADT definition assigned for given type variable *)
val lookup_adt : t -> T.tvar -> adt_info option

(** Set of unification variables in the environment *)
val uvars : t -> T.UVar.Set.t

(** Get current scope *)
val scope : t -> T.scope

(** Create a fresh unification variable in current scope *)
val fresh_uvar : t -> T.kind -> T.typ

(** Introduce all variables and named parameters bound by given scheme.
  Returns extended environment, list of type variables, list of introduced
  named parameters, and the type of the scheme body. *)
val open_scheme : t -> T.scheme ->
  t * T.named_tvar list * (T.name * T.var * T.scheme) list * T.typ
