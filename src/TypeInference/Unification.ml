(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Unification and subtyping of types *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

type arrow =
  | Arr_No
  | Arr_UVar
  | Arr_Pure   of T.typ * T.typ
  | Arr_Impure of T.typ * T.typ * T.effect

(** Internal exception *)
exception Error

let unify_with_kuvar x k =
  if T.Kind.contains_uvar x k then
    raise Error
  else
    T.KUVar.set x k

let rec check_kind_equal k1 k2 =
  match T.Kind.view k1, T.Kind.view k2 with
  | KUVar x1, KUVar x2 when T.KUVar.equal x1 x2 -> ()
  | KUVar x, _ -> unify_with_kuvar x k2
  | _, KUVar x -> unify_with_kuvar x k1

  | KType, KType -> ()
  | KType, _ -> raise Error

  | KEffect, KEffect -> ()
  | KEffect, _ -> raise Error

  | KClEffect, KClEffect -> ()
  | KClEffect, _ -> raise Error

  | KArrow(ka1, kv1), KArrow(ka2, kv2) ->
    check_kind_equal ka1 ka2;
    check_kind_equal kv1 kv2
  | KArrow _, _ -> raise Error

let unify_kind k1 k2 =
  (* TODO: create reference backtracking point *)
  match check_kind_equal k1 k2 with
  | ()              -> true
  | exception Error -> false

let kind_to_arrow k =
  match T.Kind.view k with
  | KUVar x ->
    let k1 = T.Kind.fresh_uvar () in
    let k2 = T.Kind.fresh_uvar () in
    T.KUVar.set x (T.Kind.k_arrow k1 k2);
    Some (k1, k2)
  | KArrow(k1, k2) -> Some(k1, k2)

  | KType | KEffect | KClEffect -> None

let set_uvar env u tp =
  let scope = T.UVar.raw_set u tp in
  match T.Type.try_shrink_scope ~scope tp with
  | Ok   () -> ()
  | Error _ -> raise Error

let rec check_effect_mem env x eff =
  match T.Effect.view eff with
  | EffPure | EffVar _ | EffApp _ -> raise Error
  | EffUVar u ->
    set_uvar env u (T.Effect.cons x (Env.fresh_uvar env T.Kind.k_effect))
  | EffCons(y, eff) ->
    if T.TVar.equal x y then ()
    else check_effect_mem env x eff

let rec check_subeffect env eff1 eff2 =
  match T.Effect.view eff1 with
  | EffPure    -> ()
  | EffUVar u1 ->
    if T.Effect.is_pure eff2 then
      set_uvar env u1 T.Effect.pure
    else
      begin match T.Effect.view_end eff2 with
      | EEUVar u2 when T.UVar.equal u1 u2 -> ()
      | _ ->
        (** TODO: add constraint *)
        set_uvar env u1 eff2
      end
  | EffVar x1 ->
    begin match T.Effect.view_end eff2 with
    | EEUVar u ->
      set_uvar env u (T.Type.t_var x1)
    | EEVar x2 when T.TVar.equal x1 x2 -> ()
    | _ -> raise Error
    end
  | EffApp(tp1, tp2) ->
    begin match T.Effect.view_end eff2 with
    | EEUVar u ->
      set_uvar env u (T.Type.t_app tp1 tp2)
    | EEApp(tp1', tp2') ->
      unify env (T.Type.t_app tp1 tp2) (T.Type.t_app tp1' tp2')
    | _ -> raise Error
    end
  | EffCons(x1, eff1) ->
    check_effect_mem env x1 eff2;
    check_subeffect env eff1 eff2

and unify_at_kind env tp1 tp2 k =
  match T.Kind.view k with
  | KEffect ->
    check_subeffect env tp1 tp2;
    check_subeffect env tp2 tp1

  | _ -> unify env tp1 tp2

and unify env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TUVar u1, TUVar u2 when T.UVar.equal u1 u2 -> ()
  | TUVar u, _ ->
    if T.Type.contains_uvar u tp2 then
      raise Error
    else
      set_uvar env u tp2
  | _, TUVar u ->
    if T.Type.contains_uvar u tp1 then
      raise Error
    else
      set_uvar env u tp1

  | TUnit, TUnit -> ()
  | TUnit, (TVar _ | TPureArrow _ | TArrow _ | TApp _) -> raise Error

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, (TUnit | TPureArrow _ | TArrow _ | TApp _) -> raise Error

  | TPureArrow(atp1, vtp1), TPureArrow(atp2, vtp2) ->
    unify env atp1 atp2;
    unify env vtp1 vtp2
  | TPureArrow _, (TUnit | TVar _ | TArrow _ | TApp _) -> raise Error

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    unify env atp1 atp2;
    unify env vtp1 vtp2;
    unify_at_kind env eff1 eff2 T.Kind.k_effect
  | TArrow _, (TUnit | TVar _ | TPureArrow _ | TApp _) -> raise Error

  | TApp(ftp1, atp1), TApp(ftp2, atp2) ->
    let k1 = T.Type.kind atp1 in
    let k2 = T.Type.kind atp2 in
    check_kind_equal k1 k2;
    unify env ftp1 ftp2;
    unify_at_kind env atp1 atp2 k1
  | TApp _, (TUnit | TVar _ | TPureArrow _ | TArrow _) -> raise Error

  | TEffect _, _ | _, TEffect _ ->
    (* To unify types that may possibly be effects, use [unify_at_kind]
      function. *)
    assert false

let rec check_subtype env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TUVar u1, TUVar u2 when T.UVar.equal u1 u2 -> ()
  | TUVar u, _ ->
    if T.Type.contains_uvar u tp2 then
      raise Error
    else
      set_uvar env u (T.Type.open_down ~scope:(Env.scope env) tp2)
  | _, TUVar u ->
    if T.Type.contains_uvar u tp1 then
      raise Error
    else
      set_uvar env u (T.Type.open_up ~scope:(Env.scope env) tp1)

  | TUnit, TUnit -> ()
  | TUnit, (TVar _ | TPureArrow _ | TArrow _ | TApp _) -> raise Error

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, (TUnit | TPureArrow _ | TArrow _ | TApp _) -> raise Error

  | TPureArrow(atp1, vtp1), TPureArrow(atp2, vtp2) ->
    check_subtype env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2
  | TPureArrow(atp1, vtp1), TArrow(atp2, vtp2, _) ->
    check_subtype env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2
  | TPureArrow _, (TUnit | TVar _ | TApp _) -> raise Error

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    check_subtype env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2;
    check_subeffect env eff1 eff2
  | TArrow _, (TUnit | TVar _ | TPureArrow _ | TApp _) -> raise Error

  | TApp _, TApp _ -> unify env tp1 tp2
  | TApp _, (TUnit | TVar _ | TPureArrow _ | TArrow _) -> raise Error

  | TEffect _, _ | _, TEffect _ ->
    failwith "Internal kind error"

let subeffect env eff1 eff2 =
  (* TODO: create reference backtracking point *)
  match check_subeffect env eff1 eff2 with
  | ()              -> true
  | exception Error -> false

let subtype env tp1 tp2 =
  (* TODO: create reference backtracking point *)
  match check_subtype env tp1 tp2 with
  | ()              -> true
  | exception Error -> false

let to_arrow env tp =
  match T.Type.view tp with
  | TUnit | TVar _ | TApp _ -> Arr_No
  | TUVar u ->
    let tp1 = Env.fresh_uvar env T.Kind.k_type in
    let tp2 = Env.fresh_uvar env T.Kind.k_type in
    let eff = Env.fresh_uvar env T.Kind.k_effect in
    set_uvar env u (T.Type.t_arrow tp1 tp2 eff);
    Arr_Impure(tp1, tp2, eff)
  | TPureArrow(tp1, tp2)  -> Arr_Pure(tp1, tp2)
  | TArrow(tp1, tp2, eff) -> Arr_Impure(tp1, tp2, eff)

  | TEffect _ ->
    failwith "Internal kind error"

let from_arrow env tp =
  match T.Type.view tp with
  | TUnit | TVar _ | TApp _ -> Arr_No
  | TUVar _ -> Arr_UVar
  | TPureArrow(tp1, tp2) -> Arr_Pure(tp1, tp2)
  | TArrow(tp1, tp2, eff) -> Arr_Impure(tp1, tp2, eff)

  | TEffect _ ->
    failwith "Internal kind error"
