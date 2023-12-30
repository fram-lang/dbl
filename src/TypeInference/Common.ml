(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common definitions of type-checker *)

(* Author: Piotr Polesiuk, 2023 *)

module S = Lang.Surface
module T = Lang.Unif

(** Return information about effect. *)
type ret_effect =
  | Pure
    (** Expression is pure, i.e, it does not perform any effects, and always
      terminates *)

  | Impure
    (** Expression is inpure *)

let ret_effect_join eff1 eff2 =
  match eff1, eff2 with
  | Pure,   eff2   -> eff2
  | eff1,   Pure   -> eff1
  | Impure, Impure -> Impure

