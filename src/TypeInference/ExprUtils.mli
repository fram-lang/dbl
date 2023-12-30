(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Generate a type application to given list of types *)
val make_tapp : T.expr -> T.typ list -> T.expr

(** Generalize type to polymorphic scheme. The second parameter is a list of
  implicit parameters. *)
val generalize : Env.t -> (S.name * T.var * T.typ) list ->
  T.expr -> T.typ -> T.expr * T.scheme

(** Guess types used to instantiate polymorphic function. Returns substitution
  from given type variables to guessed types together with list of these types.
  *)
val guess_types : Env.t -> T.tvar list -> T.subst * T.typ list

(** Instantiate named parameters of polymorphic expression. It takes possibly
  empty list of explicit instantiations. These instantiations are pure, so
  their order doesn't matter. *)
val instantiate_implicits :
  Env.t -> T.expr -> (T.name * T.typ) list -> (S.name * T.expr) list -> T.expr

(** Create a function that represents ADT contructor of given index,
  not applied to any parameters yet, even the type parameters of the ADT. *)
val ctor_func : pos:Position.t -> int -> Env.adt_info -> T.expr
