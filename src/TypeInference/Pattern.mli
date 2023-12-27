(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Check if given pattern fits in given scope and has given type.
  Returns extended environment and translated pattern. The scope is passed
  separately from environment, since the scope of the pattern flows top-down
  in a complex patterns, while the environment flows left-to-right. *)
val check_type :
  env:Env.t -> scope:T.scope -> S.pattern -> T.typ -> Env.t * T.pattern
