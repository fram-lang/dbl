(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type variables *)

include UnifCommon.TVar

let fresh_eff ~scope = fresh ~scope UnifCommon.Kind.k_effect

let clone_unif = clone
