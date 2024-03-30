(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on scopes *)

let initial = TVar.Set.empty

let add scope x = TVar.Set.add x scope

let add_named scope (_, x) = add scope x

let mem scope x = TVar.Set.mem x scope

let perm p scope = TVar.Set.map (TVar.Perm.apply p) scope
