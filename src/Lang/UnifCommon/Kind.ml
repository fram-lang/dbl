(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kinds *)

type kuvar = kuvar_state BRef.t

and kuvar_state =
  | UVar
    (** Unconstrained kind variable *)

  | UNonEffectVar
    (** Kind variable constrained to non-effect kinds *)

  | UKind of t
    (** Kind variable unified with a kind *)

and t = kind_view

and kind_view =
  | KType
  | KEffect
  | KUVar  of kuvar
  | KArrow of t * t

let k_type = KType

let k_effect = KEffect

let rec view k =
  match k with
  | KType | KEffect | KArrow _ -> k
  | KUVar u ->
    begin match BRef.get u with
    | UVar | UNonEffectVar -> k
    | UKind k ->
      let k = view k in
      BRef.set u (UKind k);
      k
    end

(** Constrain [k] to be a non-effect kind.
  Return [true] on success, [false] if the constraint cannot be satisfied. *)
let set_non_effect k =
  match view k with
  | KType | KArrow _ -> true
  | KEffect -> false
  | KUVar u ->
    begin match BRef.get u with
    | UVar          -> BRef.set u UNonEffectVar; true
    | UNonEffectVar -> true
    | UKind _       -> assert false
    end

(** Check whether [k] is a non-effect kind. When it returns [true], [k] is
  guaranteed to be a non-effect kind. Otherwise, [k] may still be a non-effect
  kind after further unifications. *)
let non_effect k =
  match view k with
  | KType | KArrow _ -> true
  | KEffect -> false
  | KUVar u ->
    begin match BRef.get u with
    | UVar          -> false
    | UNonEffectVar -> true
    | UKind _       -> assert false
    end

module KUVar = struct
  let fresh () = BRef.create UVar

  let equal x y = x == y

  let set x k =
    match BRef.get x with
    | UVar          -> BRef.set x (UKind k); true
    | UNonEffectVar ->
      if set_non_effect k then
        (BRef.set x (UKind k); true)
      else
        false
    | UKind _       -> assert false
end

let k_noneff_arrow k1 k2 =
  assert (non_effect k2);
  KArrow(k1, k2)

let k_arrow k1 k2 =
  if set_non_effect k2 then
    Some (KArrow(k1, k2))
  else
    None

let k_noneff_arrows ks k =
  List.fold_right k_noneff_arrow ks k

let k_arrows ks k =
  if List.is_empty ks then Some k
  else if set_non_effect k then
    Some (List.fold_right k_noneff_arrow ks k)
  else
    None

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
