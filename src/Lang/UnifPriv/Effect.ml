(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on effects *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

type effect_view =
  | EffPure
  | EffUVar of uvar
  | EffVar  of tvar

let pure_row = t_row_pure

let io_row = t_row_pure (* TODO *)

let view eff =
  match TypeBase.view eff with
  | TRowPure -> EffPure
  | TUVar u  -> EffUVar u
  | TVar x   -> EffVar x

  | TUnit | TPureArrow _ | TArrow _ ->
    failwith "Internal kind error"
