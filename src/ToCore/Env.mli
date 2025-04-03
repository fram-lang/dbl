(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the translation *)

open Common

type t

(** Initial environment *)
val initial : t

(** Extend the environment with type variable. *)
val add_tvar : t -> S.tvar -> t * T.TVar.ex

(** Extend environment with multiple type variables. *)
val add_tvars : t -> S.tvar list -> t * T.TVar.ex list

(** Extend the environment with a list of named type variables. *)
val add_named_tvars : t -> S.named_tvar list -> t * T.TVar.ex list

(** Lookup for a type variable. It must be present in the environment *)
val lookup_tvar : t -> S.tvar -> T.TVar.ex
