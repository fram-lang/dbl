(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on names *)

open TypeBase

module Ordered = struct
  type t = TypeBase.name

  let compare n1 n2 =
    match n1, n2 with
    | NVar x1, NVar x2 -> String.compare x1 x2
    | NVar _,  _       -> -1
    | _,       NVar _  -> 1

    | NOptionalVar x1, NOptionalVar x2 -> String.compare x1 x2
    | NOptionalVar _,  _       -> -1
    | _,       NOptionalVar _  -> 1

    | NMethod n1, NMethod n2 -> String.compare n1 n2
    | NMethod _, _ -> -1
    | _, NMethod _ -> 1

    | NImplicit n1, NImplicit n2 -> String.compare n1 n2
end

let equal n1 n2 =
  match n1, n2 with
  | NVar x1, NVar x2 -> x1 = x2
  | NVar _, _ -> false

  | NOptionalVar x1, NOptionalVar x2 -> x1 = x2
  | NOptionalVar _, _ -> false

  | NMethod n1, NMethod n2 -> n1 = n2
  | NMethod _, _ -> false

  | NImplicit n1, NImplicit n2 -> n1 = n2
  | NImplicit _,  _ -> false

let assoc n xs =
  List.find_map (fun (m, v) -> if equal n m then Some v else None) xs

module Set = Set.Make(Ordered)
module Map = Map.Make(Ordered)
