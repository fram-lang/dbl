(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for definitions *)
(*
open Common
open TypeCheckFix

(** Check type and effect of a single definition. It uses bidirectional
  type checking, and pass the extended environment to the body-generating
  continuation. *)
val check_def : tcfix:tcfix ->
  Env.t -> ImplicitEnv.t -> S.def ->
    (T.typ, 'dir) request -> T.effrow -> def_cont ->
      T.expr * (T.typ, 'dir) response * ret_effect

(** Check type and effect of a block of definitions. It uses bidirectional
  type checking, and pass the extended environment to the body-generating
  continuation. *)
val check_defs : tcfix:tcfix ->
  Env.t -> ImplicitEnv.t -> S.def list ->
    (T.typ, 'dir) request -> T.effrow -> def_cont ->
      T.expr * (T.typ, 'dir) response * ret_effect
*)
