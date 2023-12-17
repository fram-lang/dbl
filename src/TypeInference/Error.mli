(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to type-inference. *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Abstract representation of error *)
type t

(** Report fatal error and abort the compilation *)
val fatal : t -> 'a

(** Report non-fatal error *)
val report : t -> unit

val unbound_var : pos:Position.t -> S.var -> t
val unbound_implicit : pos:Position.t -> S.name -> t

val expr_type_mismatch   : pos:Position.t -> env:Env.t -> T.typ -> T.typ -> t
val expr_effect_mismatch :
  pos:Position.t -> env:Env.t -> T.effect -> T.effect -> t

val func_effect_mismatch :
  pos:Position.t -> env:Env.t -> T.effect -> T.effect -> t

val func_not_pure : pos:Position.t -> t

val expr_not_function     : pos:Position.t -> env:Env.t -> T.typ -> t
val expr_not_function_ctx : pos:Position.t -> env:Env.t -> T.typ -> t

val type_escapes_its_scope : pos:Position.t -> env:Env.t -> T.tvar -> t

val ungeneralizable_implicit : pos:Position.t -> S.name -> t

val looping_implicit : pos:Position.t -> S.name -> t

val implicit_type_mismatch :
  pos:Position.t -> env:Env.t -> S.name -> T.typ -> T.typ -> t
