(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on types *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

(** Get the kind of given type *)
let rec kind : type k. k typ -> k kind =
  function
  | TUnit      -> KType
  | TEffPure   -> KEffect
  | TEffJoin _ -> KEffect
  | TVar     x -> TVar.kind x
  | TArrow   _ -> KType
  | TForall  _ -> KType
  | TData    _ -> KType

(** Substitute one type in another *)
let subst_type x stp tp =
  Subst.in_type (Subst.singleton x stp) tp

(** Check equality of types *)
let rec equal : type k. k typ -> k typ -> bool =
  fun tp1 tp2 ->
  match tp1, tp2 with
  | TEffPure,   _ -> effect_equal tp1 tp2
  | TEffJoin _, _ -> effect_equal tp1 tp2
  | _, TEffPure   -> effect_equal tp1 tp2
  | _, TEffJoin _ -> effect_equal tp1 tp2

  | TUnit, TUnit -> true
  | TUnit, _     -> false

  | TVar x, TVar y -> TVar.equal x y
  | TVar _, _      -> false

  | TArrow(ta1, tb1, eff1), TArrow(ta2, tb2, eff2) ->
    equal ta1 ta2 && equal tb1 tb2 && effect_equal eff1 eff2
  | TArrow _, _ -> false

  | TForall(x1, tp1), TForall(x2, tp2) ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      if TVar.equal x1 x2 then equal tp1 tp2
      else begin
        let x = TVar.fresh (TVar.kind x1) in
        equal (subst_type x1 (TVar x) tp1) (subst_type x2 (TVar x) tp2)
      end
    | NotEqual -> false
    end
  | TForall _, _ -> false

  | TData(tp1, ctors1), TData(tp2, ctors2) ->
    equal tp1 tp2 &&
    List.length ctors1 = List.length ctors2 &&
    List.for_all2 ctor_type_equal ctors1 ctors2
  | TData _, _ -> false

and ctor_type_equal { ctor_name; ctor_arg_types } ctor2 =
  ctor_name = ctor2.ctor_name &&
  List.length ctor_arg_types = List.length ctor2.ctor_arg_types &&
  List.for_all2 equal ctor_arg_types ctor2.ctor_arg_types

(** Check equality of effects *)
and effect_equal : effect -> effect -> bool =
  fun eff1 eff2 -> subeffect eff1 eff2 && subeffect eff2 eff1

(** Check if one effect is a subeffect of another *)
and subeffect eff1 eff2 =
  match eff1 with
  | TEffPure -> true
  | TEffJoin(eff_a, eff_b) ->
    subeffect eff_a eff2 && subeffect eff_b eff2
  | TVar _ -> simple_subeffect eff1 eff2

(** Check if simple effect (different than pure and join) is a subeffect of
  another effect *)
and simple_subeffect eff1 eff2 =
  match eff2 with
  | TEffPure -> false
  | TEffJoin(eff_a, eff_b) ->
    simple_subeffect eff1 eff_a || simple_subeffect eff1 eff_b
  | TVar _ -> equal eff1 eff2

(** Check if one type is a subtype of another *)
let rec subtype tp1 tp2 =
  match tp1, tp2 with
  | TUnit, TUnit  -> true
  | TUnit, (TVar _ | TArrow _ | TForall _ | TData _) -> false

  | TVar x, TVar y -> x == y
  | TVar _, (TUnit | TArrow _ | TForall _ | TData _) -> false

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    subtype atp2 atp1 && subtype vtp1 vtp2 && subeffect eff1 eff2
  | TArrow _, (TUnit | TVar _ | TForall _ | TData _) -> false

  | TForall(x1, tp1), TForall(x2, tp2) ->
    (* TODO: it can be done better than O(n^2) time. *)
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      let x = TVar (TVar.clone x1) in
      subtype (subst_type x1 x tp1) (subst_type x2 x tp2)
    | NotEqual -> false
    end
  | TForall _, (TUnit | TVar _ | TArrow _ | TData _) -> false

  | TData _, TData _ -> equal tp1 tp2
  | TData _, (TUnit | TVar _ | TArrow _ | TForall _) -> false

let rec forall_map f xs =
  match xs with
  | [] -> Some []
  | x :: xs ->
    begin match f x, forall_map f xs with
    | Some y, Some ys -> Some (y :: ys)
    | _ -> None
    end

(** Tries to find an equivalent type that do not contain given type
  variable. *)
let rec type_without : type k1 k2. k1 tvar -> k2 typ -> k2 typ option =
  fun a tp ->
  match tp with
  | TUnit | TEffPure -> Some tp
  | TEffJoin(eff1, eff2) ->
    begin match type_without a eff1, type_without a eff2 with
    | Some eff1, Some eff2 -> Some (TEffJoin(eff1, eff2))
    | _ -> None
    end
  | TVar b ->
    begin match TVar.hequal a b with
    | Equal    -> None
    | NotEqual -> Some tp
    end
  | TArrow(tp1, tp2, eff) ->
    begin match
      type_without a tp1, type_without a tp2, type_without a eff
    with
    | Some tp1, Some tp2, Some eff -> Some (TArrow(tp1, tp2, eff))
    | _ -> None
    end
  | TForall(b, body) ->
    begin match TVar.hequal a b with
    | Equal    -> Some tp
    | NotEqual ->
      begin match type_without a body with
      | Some body -> Some (TForall(b, body))
      | None      -> None
      end
    end
  | TData(tp, ctors) ->
    begin match type_without a tp, forall_map (ctor_type_without a) ctors with
    | Some tp, Some ctors -> Some (TData(tp, ctors))
    | _ -> None
    end

and ctor_type_without : type k. k tvar -> ctor_type -> ctor_type option =
  fun a ctor ->
  match forall_map (type_without a) ctor.ctor_arg_types with
  | Some tps ->
    Some {
      ctor_name      = ctor.ctor_name;
      ctor_arg_types = tps
    }
  | None -> None

(** Tries to find a supereffect of given type that do not contain given type
  variable *)
let supereffect_without : type k. k tvar -> effect -> effect option =
  type_without 

(** Finds a subeffect of given type that do not contain given type
  variable *)
let rec subeffect_without : type k. k tvar -> effect -> effect =
  fun a eff ->
  match eff with
  | TEffPure -> eff
  | TEffJoin(eff1, eff2) ->
    begin match subeffect_without a eff1, subeffect_without a eff2 with
    | TEffPure, eff2 -> eff2
    | eff1, TEffPure -> eff1
    | eff1, eff2     -> TEffJoin(eff1, eff2)
    end
  | TVar b ->
    begin match TVar.hequal a b with
    | Equal    -> TEffPure
    | NotEqual -> eff
    end

(** Tries to find a supertype of given type that do not contain given type
  variable *)
let rec supertype_without : type k. k tvar -> ttype -> ttype option =
  fun a tp ->
  match tp with
  | TUnit  -> Some tp
  | TVar _ | TData _ -> type_without a tp
  | TArrow(tp1, tp2, eff) ->
    begin match
      subtype_without     a tp1,
      supertype_without   a tp2,
      supereffect_without a eff
    with
    | Some tp1, Some tp2, Some eff -> Some (TArrow(tp1, tp2, eff))
    | _ -> None
    end
  | TForall(b, body) ->
    begin match TVar.hequal a b with
    | Equal    -> Some tp
    | NotEqual ->
      begin match supertype_without a body with
      | Some body -> Some (TForall(b, body))
      | None      -> None
      end
    end

(** Tries to find a subtype of given type that do not contain given type
  variable *)
and subtype_without : type k. k tvar -> ttype -> ttype option =
  fun a tp ->
  match tp with
  | TUnit  -> Some tp
  | TVar _ | TData _ -> type_without a tp
  | TArrow(tp1, tp2, eff) ->
    begin match
      supertype_without a tp1, subtype_without a tp2, subeffect_without a eff
    with
    | Some tp1, Some tp2, eff -> Some(TArrow(tp1, tp2, eff))
    | _ -> None
    end
  | TForall(b, body) ->
    begin match TVar.hequal a b with
    | Equal    -> Some tp
    | NotEqual ->
      begin match subtype_without a body with
      | Some body -> Some (TForall(b, body))
      | None      -> None
      end
    end

type ex = Ex : 'k typ -> ex

let t_pure_arrows tps tp =
  List.fold_right (fun tp1 tp2 -> TArrow(tp1, tp2, TEffPure)) tps tp

let t_foralls xs tp =
  List.fold_right (fun (TVar.Ex x) tp -> TForall(x, tp)) xs tp
