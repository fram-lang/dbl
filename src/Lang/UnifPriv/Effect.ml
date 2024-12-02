(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on effects *)

open TypeBase

let join eff1 eff2 =
  match eff1 with
  | Pure   -> eff2
  | Impure -> Impure

let joins effs = List.fold_left join Pure effs
