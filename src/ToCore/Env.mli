(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the translation *)
(*
open Common

type t

(** Empty environment *)
val empty : repl_mode:bool -> t

(** Extend environment with type variable. *)
val add_tvar : t -> S.tvar -> t * T.TVar.ex

(** Extend environment with a named type variable. *)
val add_named_tvar : t -> S.named_tvar -> t * T.TVar.ex

(** Extend environment with multiple type variables. *)
val add_tvars : t -> S.tvar list -> t * T.TVar.ex list

(** Extend environment with multiple type variables, freshly generated
  outside the environment *)
val add_tvars' : t -> S.tvar list -> T.TVar.ex list -> t

(** Lookup for a type variable. It must be present in the environment *)
val lookup_tvar : t -> S.tvar -> T.TVar.ex

val in_repl_mode : t -> bool
*)
