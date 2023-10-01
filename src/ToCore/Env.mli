(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the translation *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

type t

(** Empty environment *)
val empty : t

(** Externd environment with type variable. *)
val add_tvar : t -> S.tvar -> t * T.TVar.ex

(** Lookup for a type variable. It must be present in the environment *)
val lookup_tvar : t -> S.tvar -> T.TVar.ex
