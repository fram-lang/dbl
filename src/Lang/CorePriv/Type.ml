(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on types *)

open TypeBase

(** Unit type *)
let t_unit = TVar BuiltinType.tv_unit

(** Get the kind of given type *)
let rec kind : type k. k typ -> k kind =
  function
  | TUVar (_, k) -> k
  | TEffPure     -> KEffect
  | TEffJoin _   -> KEffect
  | TVar     x   -> TVar.kind x
  | TArrow   _   -> KType
  | TForall  _   -> KType
  | TLabel  _    -> KType
  | TData    _   -> KType
  | TApp(tp, _) ->
    begin match kind tp with
    | KArrow(_, k) -> k
    end

(** Substitute one type in another *)
let subst_type x stp tp =
  Subst.in_type (Subst.singleton x stp) tp

(** Check equality of types *)
let rec equal : type k. k typ -> k typ -> bool =
  fun tp1 tp2 ->
  match tp1, tp2 with
  | TUVar(x,_), TUVar(y,_) -> UID.compare x y = 0
  | _, TUVar _
  | TUVar _, _ -> false

  | TEffPure,   _ -> effect_equal tp1 tp2
  | TEffJoin _, _ -> effect_equal tp1 tp2
  | _, TEffPure   -> effect_equal tp1 tp2
  | _, TEffJoin _ -> effect_equal tp1 tp2

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

  | TLabel lbl1, TLabel lbl2 ->
    effect_equal lbl1.effect lbl2.effect &&
    begin match
      tvars_binder_equal ~sub1:Subst.empty ~sub2:Subst.empty
        lbl1.tvars lbl2.tvars
    with
    | None -> false
    | Some(sub1, sub2) ->
      List.length lbl1.val_types = List.length lbl2.val_types &&
      List.for_all2
        (fun tp1 tp2 ->
          equal (Subst.in_type sub1 tp1) (Subst.in_type sub2 tp2))
        lbl1.val_types lbl2.val_types &&
      equal
        (Subst.in_type sub1 lbl1.delim_tp)
        (Subst.in_type sub2 lbl2.delim_tp) &&
      effect_equal
        (Subst.in_type sub1 lbl1.delim_eff)
        (Subst.in_type sub2 lbl2.delim_eff)
    end
  | TLabel _, _ -> false

  | TData(tp1, eff1, ctors1), TData(tp2, eff2, ctors2) ->
    equal tp1 tp2 &&
    equal eff1 eff2 &&
    List.length ctors1 = List.length ctors2 &&
    List.for_all2 ctor_type_equal ctors1 ctors2
  | TData _, _ -> false

  | TApp(tf1, ta1), TApp(tf2, ta2) ->
    begin match Kind.equal (kind ta1) (kind ta2) with
    | Equal    -> equal tf1 tf2 && equal ta1 ta2
    | NotEqual -> false
    end
  | TApp _, _ -> false

and ctor_type_equal { ctor_name; ctor_tvars; ctor_arg_types } ctor2 =
  match
    tvars_binder_equal ~sub1:Subst.empty ~sub2:Subst.empty
      ctor_tvars ctor2.ctor_tvars
  with
  | None -> false
  | Some(sub1, sub2) ->
    ctor_name = ctor2.ctor_name &&
    List.length ctor_arg_types = List.length ctor2.ctor_arg_types &&
    List.for_all2
      (fun tp1 tp2 ->
        equal (Subst.in_type sub1 tp1) (Subst.in_type sub2 tp2))
      ctor_arg_types ctor2.ctor_arg_types

and tvars_binder_equal ~sub1 ~sub2 xs1 xs2 =
  match xs1, xs2 with
  | [], [] -> Some(sub1, sub2)
  | TVar.Ex x1 :: xs1, TVar.Ex x2 :: xs2 ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      let x = TVar.fresh (TVar.kind x1) in
      let sub1 = Subst.add sub1 x1 (TVar x) in
      let sub2 = Subst.add sub2 x2 (TVar x) in
      tvars_binder_equal ~sub1 ~sub2 xs1 xs2
    | NotEqual -> None
    end
  | [], _ :: _ | _ :: _, [] -> None

(** Check equality of effects *)
and effect_equal : effect -> effect -> bool =
  fun eff1 eff2 -> subeffect eff1 eff2 && subeffect eff2 eff1

(** Check if one effect is a subeffect of another *)
and subeffect eff1 eff2 =
  match eff1 with
  | TEffPure -> true
  | TEffJoin(eff_a, eff_b) ->
    subeffect eff_a eff2 && subeffect eff_b eff2
  | TUVar _ | TVar _ | TApp _ -> simple_subeffect eff1 eff2

(** Check if simple effect (different than pure and join) is a subeffect of
  another effect *)
and simple_subeffect eff1 eff2 =
  match eff2 with
  | TEffPure -> false
  | TEffJoin(eff_a, eff_b) ->
    simple_subeffect eff1 eff_a || simple_subeffect eff1 eff_b
  | TUVar _ | TVar _ | TApp _ -> equal eff1 eff2

(** Check if one type is a subtype of another *)
let rec subtype tp1 tp2 =
  match tp1, tp2 with
  | TUVar _, TUVar _ -> equal tp1 tp2
  | TUVar _, (TVar _ | TArrow _ | TForall _ | TLabel _ | TData _ | TApp _) ->
    false

  | TVar x, TVar y -> x == y
  | TVar _, (TUVar _ | TArrow _ | TForall _ | TLabel _ | TData _ | TApp _) ->
    false

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    subtype atp2 atp1 && subtype vtp1 vtp2 && subeffect eff1 eff2
  | TArrow _, (TUVar _ | TVar _ | TForall _ | TLabel _ | TData _ | TApp _) ->
    false

  | TForall(x1, tp1), TForall(x2, tp2) ->
    (* TODO: it can be done better than O(n^2) time. *)
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      let x = TVar (TVar.clone x1) in
      subtype (subst_type x1 x tp1) (subst_type x2 x tp2)
    | NotEqual -> false
    end
  | TForall _, (TUVar _ | TVar _ | TArrow _ | TLabel _ | TData _ | TApp _) ->
    false

  | TLabel _, TLabel _ -> equal tp1 tp2
  | TLabel _, (TUVar _ | TVar _ | TArrow _ | TForall _ | TData _ | TApp _) ->
    false

  | TData(tp1, eff1, ctors1), TData(tp2, eff2, ctors2) ->
    equal tp1 tp2 &&
    subeffect eff1 eff2 &&
    List.length ctors1 = List.length ctors2 &&
    List.for_all2 ctor_type_equal ctors1 ctors2
  | TData _, (TUVar _ | TVar _ | TArrow _ | TForall _ | TLabel _ | TApp _) ->
    false

  | TApp _, TApp _ -> equal tp1 tp2
  | TApp _, (TUVar _ | TVar _ | TArrow _ | TForall _ | TLabel _ | TData _) ->
    false

let rec forall_map f xs =
  match xs with
  | [] -> Some []
  | x :: xs ->
    begin match f x, forall_map f xs with
    | Some y, Some ys -> Some (y :: ys)
    | _ -> None
    end

let add_tvars_to_scope tvs scope =
  List.fold_left
    (fun scope (TVar.Ex a) -> TVar.Set.add a scope)
    scope
    tvs

(** Tries to find an equivalent type, such that all free type variables are
  members of given set ([scope]) *)
let rec type_in_scope : type k. _ -> k typ -> k typ option =
  fun scope tp ->
  match tp with
  | TUVar _ | TEffPure -> Some tp
  | TEffJoin(eff1, eff2) ->
    begin match type_in_scope scope eff1, type_in_scope scope eff2 with
    | Some eff1, Some eff2 -> Some (TEffJoin(eff1, eff2))
    | _ -> None
    end
  | TVar a ->
    if TVar.Set.mem a scope then Some tp
    else None
  | TArrow(tp1, tp2, eff) ->
    begin match
      type_in_scope scope tp1,
      type_in_scope scope tp2,
      type_in_scope scope eff
    with
    | Some tp1, Some tp2, Some eff -> Some (TArrow(tp1, tp2, eff))
    | _ -> None
    end
  | TForall(a, body) ->
    begin match type_in_scope (TVar.Set.add a scope) body with
    | Some body -> Some (TForall(a, body))
    | None      -> None
    end
  | TLabel lbl ->
    let effect = type_in_scope scope lbl.effect in
    let scope  = add_tvars_to_scope lbl.tvars scope in
    begin match
      effect,
      forall_map (type_in_scope scope) lbl.val_types,
      type_in_scope scope lbl.delim_tp,
      type_in_scope scope lbl.delim_eff
    with
    | Some effect, Some val_types, Some delim_tp, Some delim_eff ->
      Some (TLabel
        { effect; tvars = lbl.tvars; val_types; delim_tp; delim_eff })
    | _ -> None
    end
  | TData(tp, eff, ctors) ->
    begin match
      type_in_scope scope tp,
      type_in_scope scope eff,
      forall_map (ctor_type_in_scope scope) ctors
    with
    | Some tp, Some eff, Some ctors -> Some (TData(tp, eff, ctors))
    | _ -> None
    end
  | TApp(tp1, tp2) ->
    begin match type_in_scope scope tp1, type_in_scope scope tp2 with
    | Some tp1, Some tp2 -> Some (TApp(tp1, tp2))
    | _ -> None
    end

and ctor_type_in_scope scope ctor =
  let scope = add_tvars_to_scope ctor.ctor_tvars scope in
  match forall_map (type_in_scope scope) ctor.ctor_arg_types with
  | Some tps ->
    Some {
      ctor_name      = ctor.ctor_name;
      ctor_tvars     = ctor.ctor_tvars;
      ctor_arg_types = tps
    }
  | None -> None

(** Tries to find a supereffect of given type, such that all free type
  variables are members of given set ([scope]) *)
let supereffect_in_scope scope (eff : effect) =
  type_in_scope scope eff

(** Tries to find a subeffect of given type, such that all free type
  variables are members of given set ([scope]) *)
let rec subeffect_in_scope scope (eff : effect) =
  match eff with
  | TUVar _ | TEffPure -> eff
  | TEffJoin(eff1, eff2) ->
    begin match
      subeffect_in_scope scope eff1,
      subeffect_in_scope scope eff2
    with
    | TEffPure, eff2 -> eff2
    | eff1, TEffPure -> eff1
    | eff1, eff2     -> TEffJoin(eff1, eff2)
    end
  | TVar a ->
    if TVar.Set.mem a scope then eff
    else TEffPure
  | TApp _ ->
    begin match type_in_scope scope eff with
    | None     -> TEffPure
    | Some eff -> eff
    end

(** Tries to find a supertype of given type, such that all free type variables
  are members of given set ([scope]) *)
let rec supertype_in_scope scope (tp : ttype) =
  match tp with
  | TUVar _ | TVar _ | TLabel _ | TData _ | TApp _ -> type_in_scope scope tp
  | TArrow(tp1, tp2, eff) ->
    begin match
      subtype_in_scope     scope tp1,
      supertype_in_scope   scope tp2,
      supereffect_in_scope scope eff
    with
    | Some tp1, Some tp2, Some eff -> Some (TArrow(tp1, tp2, eff))
    | _ -> None
    end
  | TForall(a, body) ->
    begin match supertype_in_scope (TVar.Set.add a scope) body with
    | Some body -> Some (TForall(a, body))
    | None      -> None
    end

(** Tries to find a subtype of given type, such that all free type variables
  are members of given set ([scope]) *)
and subtype_in_scope scope (tp : ttype) =
  match tp with
  | TUVar _ | TVar _ | TLabel _ | TApp _ -> type_in_scope scope tp
  | TArrow(tp1, tp2, eff) ->
    begin match
      supertype_in_scope scope tp1,
      subtype_in_scope   scope tp2,
      subeffect_in_scope scope eff
    with
    | Some tp1, Some tp2, eff -> Some(TArrow(tp1, tp2, eff))
    | _ -> None
    end
  | TForall(a, body) ->
    begin match subtype_in_scope (TVar.Set.add a scope) body with
    | Some body -> Some (TForall(a, body))
    | None      -> None
    end
  | TData(tp, eff, ctors) ->
    begin match
      type_in_scope scope tp,
      subeffect_in_scope scope eff,
      forall_map (ctor_type_in_scope scope) ctors
    with
    | Some tp, eff, Some ctors -> Some (TData(tp, eff, ctors))
    | _ -> None
    end

(** Check if all types on non-strictly positive positions fits in given
  scope. *)
let rec strictly_positive : type k. nonrec_scope:_ -> k typ -> bool =
  fun ~nonrec_scope tp ->
  match tp with
  | TUVar _ | TVar _ | TEffPure -> true
  | TLabel _ | TData _ ->
    begin match type_in_scope nonrec_scope tp with
    | Some _ -> true
    | None   -> false
    end

  | TEffJoin(eff1, eff2) ->
    strictly_positive ~nonrec_scope eff1 &&
    strictly_positive ~nonrec_scope eff2

  | TArrow(tp1, tp2, eff) ->
    begin match
      type_in_scope nonrec_scope tp1,
      strictly_positive ~nonrec_scope tp2,
      strictly_positive ~nonrec_scope eff
    with
    | Some _, true, true -> true
    | _ -> false
    end

  | TForall(a, tp) ->
    strictly_positive ~nonrec_scope:(TVar.Set.add a nonrec_scope) tp

  | TApp(tp1, tp2) ->
    begin match
      strictly_positive ~nonrec_scope tp1,
      type_in_scope nonrec_scope tp2
    with
    | true, Some _ -> true
    | _ -> false
    end

(** Check if all types on non-strictly positive positions fits in given
  scope (for ADT constructors) *)
let strictly_positive_ctor ~nonrec_scope ctor =
  let nonrec_scope = add_tvars_to_scope ctor.ctor_tvars nonrec_scope in
  List.for_all (strictly_positive ~nonrec_scope) ctor.ctor_arg_types

(** Check if all types on non-strictly positive positions fits in given
  scope (for list of ADT constructors) *)
let strictly_positive_ctors ~nonrec_scope ctors =
  List.for_all (strictly_positive_ctor ~nonrec_scope) ctors

type ex = Ex : 'k typ -> ex

let t_pure_arrows tps tp =
  List.fold_right (fun tp1 tp2 -> TArrow(tp1, tp2, TEffPure)) tps tp

let t_foralls xs tp =
  List.fold_right (fun (TVar.Ex x) tp -> TForall(x, tp)) xs tp
