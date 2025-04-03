(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

type kuvar = t option BRef.t
and t = kind_view
and kind_view =
  | KType
  | KEffect
  | KUVar  of kuvar
  | KArrow of t * t

let k_type = KType

let k_effect = KEffect

let k_arrow k1 k2 = KArrow(k1, k2)

let k_arrows ks k = List.fold_right k_arrow ks k

let rec view k =
  match k with
  | KType | KEffect | KArrow _ -> k
  | KUVar u ->
    begin match BRef.get u with
    | None -> k
    | Some k ->
      let k = view k in
      BRef.set u (Some k);
      k
    end

module KUVar = struct
  let fresh () = BRef.create None

  let equal x y = x == y

  let set x k =
    match BRef.get x with
    | None   -> BRef.set x (Some k)
    | Some _ -> assert false
end

let fresh_uvar () = KUVar (KUVar.fresh ())

let rec contains_uvar x k =
  match view k with
  | KType | KEffect -> false
  | KUVar u -> KUVar.equal x u
  | KArrow(k1, k2) ->
    contains_uvar x k1 || contains_uvar x k2

let rec to_sexpr k =
  let open SExpr in
  let rec tr_arrow k =
    match k with
    | KArrow(k1, k2) -> to_sexpr k1 :: tr_arrow k2
    | _ -> [ Sym "->"; to_sexpr k ]
  in
  match view k with
  | KType   -> Sym "type"
  | KEffect -> Sym "effect"
  | KArrow _ -> List (tr_arrow k)
  | KUVar  _  -> Sym "?"
