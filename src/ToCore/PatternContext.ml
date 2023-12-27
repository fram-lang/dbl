(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Building counterexample of non-matched value in pattern-matching *)

(* Author: Piotr Polesiuk, 2023 *)

(** Example of not-matched pattern *)
type ex_pattern =
  | ExWildcard
    (** Wild-card: any value *)

(** Zipper context of a not-matched counterexample *)
type ctx =
  | CtxRoot
    (** Root of the tree *)

  | CtxDone of ex_pattern
    (** Pattern without holes *)

(** Plug given example pattern into the context, and focus on the next hole *)
let rec refocus_with ctx ex =
  match ctx with
  | CtxRoot -> CtxDone ex

  | CtxDone _ -> assert false

(** Refocus given context on the next hole *)
let refocus ctx =
  refocus_with ctx ExWildcard
