(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on kinds *)

open TypeBase

let equal = kind_equal

type ex = Ex : 'k kind -> ex
