(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on names *)

(* Author: Piotr Polesiuk, 2024 *)

open TypeBase

module Ordered = struct
  type t = TypeBase.name

  let compare n1 n2 =
    match n1, n2 with
    | NLabel, NLabel -> 0
    | NLabel, _      -> -1
    | _,      NLabel -> 1

    | NVar x1, NVar x2 -> String.compare x1 x2
    | NVar _,  _       -> -1
    | _,       NVar _  -> 1

    | NImplicit n1, NImplicit n2 -> String.compare n1 n2
end

let equal n1 n2 =
  match n1, n2 with
  | NLabel, NLabel -> true
  | NLabel, _      -> false

  | NVar x1, NVar x2 -> x1 = x2
  | NVar _, _ -> false

  | NImplicit n1, NImplicit n2 -> n1 = n2
  | NImplicit _,  _ -> false

let assoc n xs =
  List.find_map (fun (m, v) -> if equal n m then Some v else None) xs

let subst = Subst.in_name

module Set = Set.Make(Ordered)
module Map = Map.Make(Ordered)
