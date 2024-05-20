(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Functions allowing the use of some of the types defined in the Prelude. *)

open Common

val mk_Option : env:Env.t -> T.typ -> T.typ

val mk_Some : env:Env.t -> T.typ -> T.expr -> T.expr

val mk_None : env:Env.t -> T.typ -> T.expr