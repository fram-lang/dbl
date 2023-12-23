(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

(* Author: Piotr Polesiuk, 2023 *)

type kuvar = kind option BRef.t
and kind = kind_view
and kind_view =
  | KType
  | KEffect
  | KClEffect
  | KUVar of kuvar

module KUVar = struct
  let fresh () = BRef.ref None

  let equal x y = x == y

  let set x k =
    match BRef.get x with
    | None -> BRef.set x (Some k)
    | Some _ -> assert false
end

let k_type = KType

let k_effect = KEffect

let k_cleffect = KClEffect

let fresh_uvar () = KUVar (KUVar.fresh ())

let rec view k =
  match k with
  | KType | KEffect | KClEffect -> k
  | KUVar u ->
    begin match BRef.get u with
    | None -> k
    | Some k ->
      let k = view k in
      BRef.set u (Some k);
      k
    end

let rec contains_uvar x k =
  match view k with
  | KType | KEffect | KClEffect -> false
  | KUVar u -> KUVar.equal x u
