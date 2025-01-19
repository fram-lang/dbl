(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for definitions *)

open Common
open TypeCheckFix

(** Check the type of a single definition. It uses bidirectional type
  checking, and passes the extended environment to the body-generating
  continuation. *)
val check_def : tcfix:tcfix ->
  ('st, sec) opn Env.t -> S.def -> (T.typ, 'dir) request ->
    'st def_cont -> 'dir expr_result

(** Check the type of a block of definitions. It uses bidirectional type
  checking, and passes the extended environment to the body-generating
  continuation. *)
val check_defs : tcfix:tcfix ->
  ('st, sec) opn Env.t -> S.def list -> (T.typ, 'dir) request ->
    'st def_cont -> 'dir expr_result
