(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the type inference *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

type t

(** Empty environment *)
val empty : t

(** Extend an environment with a polymorphic variable *)
val add_poly_var : t -> S.var -> T.scheme -> t * T.var

(** Extend an environment with a monomorphic variable *)
val add_mono_var : t -> S.var -> T.typ -> t * T.var

(** Extend an environment with a polymorphic named implicit.
  The last parameter is a function called on each use of implicit parameter *)
val add_poly_implicit :
  t -> S.name -> T.scheme -> (Position.t -> unit) -> t * T.var

(** Extend an environment with a monomorphic named implicit.
  The last parameter is a function called on each use of implicit parameter *)
val add_mono_implicit :
  t -> S.name -> T.typ -> (Position.t -> unit) -> t * T.var

(** Extend an environment with an anonymous type variable *)
val add_anon_tvar : t -> T.kind -> t * T.tvar

(** Lookup for Unif representation and a scheme of a variable. Returns [None]
  if variable is not bound. *)
val lookup_var : t -> S.var -> (T.var * T.scheme) option

(** Lookup for Unif representation, a scheme, and "on-use" function of a named
  implicit. Returns [None] if implicit is not bound. *)
val lookup_implicit :
  t -> S.var -> (T.var * T.scheme * (Position.t -> unit)) option

(** Set of unification variables in the environment *)
val uvars : t -> T.UVar.Set.t

(** Get current scope *)
val scope : t -> T.scope

(** Create a fresh unification variable in current scope *)
val fresh_uvar : t -> T.kind -> T.typ
