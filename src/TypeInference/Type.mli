(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kind-checking and translation of type expressions *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Check kind and translate a type expression *)
val check_kind : Env.t -> S.type_expr -> T.kind -> T.typ

(** Check kind and translate a type expression of kind Type *)
val tr_ttype : Env.t -> S.type_expr -> T.typ
