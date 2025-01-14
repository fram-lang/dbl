(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraints generated during type inference *)

type t = unit

let solve c = ()

let solve_partial cs = []

let solve_all = List.iter solve

let fix_scopes new_tvars cs = cs
