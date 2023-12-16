(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on scopes *)

(* Author: Piotr Polesiuk, 2023 *)

let initial = TVar.Set.empty

let add scope x = TVar.Set.add x scope

let mem scope x = TVar.Set.mem x scope
