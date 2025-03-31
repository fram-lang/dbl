(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types *)

open Common

(** Translate a computation effect *)
val tr_ceffect : Env.t -> S.ceffect -> T.effect

(** Translate a type *)
val tr_type : Env.t -> S.typ -> T.Type.ex

(** Translate a type of type kind *)
val tr_ttype : Env.t -> S.typ -> T.ttype

(** Translate a type scheme *)
val tr_scheme : Env.t -> S.scheme -> T.ttype

(** Translate a constraint *)
val tr_constr : Env.t -> S.constr -> T.constr
