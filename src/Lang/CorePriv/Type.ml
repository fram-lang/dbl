(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on types *)

open TypeBase

(** Unit type *)
let t_unit = TVar BuiltinType.tv_unit

(** Option type *)
let t_option arg =
  TApp(TVar BuiltinType.tv_option, arg)

(** Get the kind of given type *)
let rec kind : type k. k typ -> k kind =
  function
  | TEffPure     -> KEffect
  | TEffJoin _   -> KEffect
  | TVar     x   -> TVar.kind x
  | TArrow   _   -> KType
  | TForall  _   -> KType
  | TGuard   _   -> KType
  | TLabel  _    -> KType
  | TData    _   -> KType
  | TApp(tp, _) ->
    begin match kind tp with
    | KArrow(_, k) -> k
    end

(** Substitute one type in another *)
let subst_type x stp tp =
  Subst.in_type (Subst.singleton x stp) tp

(** Check if effect variable appears (syntactically) in an effect *)
let rec effect_mem a (eff : effct) =
  match eff with
  | TEffPure -> false
  | TEffJoin(effa, effb) -> effect_mem a effa || effect_mem a effb
  | TVar b -> TVar.equal a b
  | TApp _ ->
    failwith "Internal error: TApp in effect"

(** Check if one effect is a subeffect of another. In order to avoid infinite
  loops, we keep track of the set of effect variables that we have already
  visited. *)
let rec subeffect_rec cset visited (eff1 : effct) eff2 =
  match eff1 with
  | TEffPure -> true
  | TEffJoin(effa, effb) ->
    subeffect_rec cset visited effa eff2 &&
    subeffect_rec cset visited effb eff2
  | TVar a ->
    if TVar.Set.mem a visited then false
    else
      effect_mem a eff2 ||
      let visited = TVar.Set.add a visited in
      List.exists
        (fun eff -> subeffect_rec cset visited eff eff2)
        (ConstrSet.upper_bounds cset a)
  | TApp _ ->
    failwith "Internal error: TApp in effect"

(** Check if one effect is a subeffect of another *)
let subeffect cset eff1 eff2 =
  subeffect_rec cset TVar.Set.empty eff1 eff2

(** Check equality of effects *)
let effect_equal cset eff1 eff2 =
  subeffect cset eff1 eff2 && subeffect cset eff2 eff1

(** Check if one set of constraints implies another *)
let constrs_imply cset cs1 cs2 =
  let cset = ConstrSet.add_list cset cs1 in
  List.for_all (fun (eff1, eff2) -> subeffect cset eff1 eff2) cs2

(** Check equality of constraint lists *)
let rec constrs_equal cset cs1 cs2 =
  constrs_imply cset cs1 cs2 && constrs_imply cset cs2 cs1

(** Check equality of types *)
let rec equal : type k. ConstrSet.t -> k typ -> k typ -> bool =
  fun cset tp1 tp2 ->
  match tp1, tp2 with
  | TEffPure,   _ -> effect_equal cset tp1 tp2
  | TEffJoin _, _ -> effect_equal cset tp1 tp2
  | _, TEffPure   -> effect_equal cset tp1 tp2
  | _, TEffJoin _ -> effect_equal cset tp1 tp2

  | TVar x, TVar y ->
    begin match TVar.kind x with
    | KEffect -> effect_equal cset tp1 tp2
    | _ -> TVar.equal x y
    end
  | TVar _, _      -> false

  | TArrow(ta1, tb1, eff1), TArrow(ta2, tb2, eff2) ->
    equal cset ta1 ta2 && equal cset tb1 tb2 && effect_equal cset eff1 eff2
  | TArrow _, _ -> false

  | TForall(x1, tp1), TForall(x2, tp2) ->
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      if TVar.equal x1 x2 then equal cset tp1 tp2
      else begin
        let x = TVar.fresh (TVar.kind x1) in
        equal cset (subst_type x1 (TVar x) tp1) (subst_type x2 (TVar x) tp2)
      end
    | NotEqual -> false
    end
  | TForall _, _ -> false

  | TGuard(cs1, tp1), TGuard(cs2, tp2) ->
    constrs_equal cset cs1 cs2 && equal (ConstrSet.add_list cset cs1) tp1 tp2
  | TGuard _, _ -> false

  | TLabel lbl1, TLabel lbl2 ->
    effect_equal cset lbl1.effct lbl2.effct &&
    begin match
      tvars_binder_equal ~sub1:Subst.empty ~sub2:Subst.empty
        lbl1.tvars lbl2.tvars
    with
    | None -> false
    | Some(sub1, sub2) ->
      List.length lbl1.val_types = List.length lbl2.val_types &&
      List.for_all2
        (fun tp1 tp2 ->
          equal cset (Subst.in_type sub1 tp1) (Subst.in_type sub2 tp2))
        lbl1.val_types lbl2.val_types &&
      equal cset
        (Subst.in_type sub1 lbl1.delim_tp)
        (Subst.in_type sub2 lbl2.delim_tp) &&
      effect_equal cset
        (Subst.in_type sub1 lbl1.delim_eff)
        (Subst.in_type sub2 lbl2.delim_eff)
    end
  | TLabel _, _ -> false

  | TData(tp1, eff1, ctors1), TData(tp2, eff2, ctors2) ->
    equal cset tp1 tp2 &&
    equal cset eff1 eff2 &&
    List.length ctors1 = List.length ctors2 &&
    List.for_all2 (ctor_type_equal cset) ctors1 ctors2
  | TData _, _ -> false

  | TApp(tf1, ta1), TApp(tf2, ta2) ->
    begin match Kind.equal (kind ta1) (kind ta2) with
    | Equal    -> equal cset tf1 tf2 && equal cset ta1 ta2
    | NotEqual -> false
    end
  | TApp _, _ -> false

and ctor_type_equal cset { ctor_name; ctor_tvars; ctor_arg_types } ctor2 =
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
        equal cset (Subst.in_type sub1 tp1) (Subst.in_type sub2 tp2))
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

(** Check if one type is a subtype of another *)
let rec subtype cset tp1 tp2 =
  match tp1, tp2 with
  | TVar x, TVar y -> x == y
  | TVar _, _ -> false

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    subtype cset atp2 atp1 && subtype cset vtp1 vtp2 &&
    subeffect cset eff1 eff2
  | TArrow _, _ -> false

  | TForall(x1, tp1), TForall(x2, tp2) ->
    (* TODO: it can be done better than O(n^2) time. *)
    begin match Kind.equal (TVar.kind x1) (TVar.kind x2) with
    | Equal ->
      let x = TVar (TVar.clone x1) in
      subtype cset (subst_type x1 x tp1) (subst_type x2 x tp2)
    | NotEqual -> false
    end
  | TForall _, _ ->
    false

  | TGuard(cs1, tp1), TGuard(cs2, tp2) ->
    constrs_imply cset cs2 cs1 &&
    subtype (ConstrSet.add_list cset cs2) tp1 tp2
  | TGuard _, _ -> false

  | TLabel _, TLabel _ -> equal cset tp1 tp2
  | TLabel _, _ -> false

  | TData(tp1, eff1, ctors1), TData(tp2, eff2, ctors2) ->
    equal cset tp1 tp2 &&
    subeffect cset eff1 eff2 &&
    List.length ctors1 = List.length ctors2 &&
    List.for_all2 (ctor_type_equal cset) ctors1 ctors2
  | TData _, _ -> false

  | TApp _, TApp _ -> equal cset tp1 tp2
  | TApp _, _ -> false

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
  | TEffPure -> Some tp
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
  | TGuard(cs, tp) ->
    begin match
      forall_map (constr_in_scope scope) cs,
      type_in_scope scope tp
    with
    | Some cs, Some tp -> Some (TGuard(cs, tp))
    | _ -> None
    end
  | TLabel lbl ->
    let effct = type_in_scope scope lbl.effct in
    let scope  = add_tvars_to_scope lbl.tvars scope in
    begin match
      effct,
      forall_map (type_in_scope scope) lbl.val_types,
      type_in_scope scope lbl.delim_tp,
      type_in_scope scope lbl.delim_eff
    with
    | Some effct, Some val_types, Some delim_tp, Some delim_eff ->
      Some (TLabel
        { effct; tvars = lbl.tvars; val_types; delim_tp; delim_eff })
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

and constr_in_scope scope (eff1, eff2) =
  match type_in_scope scope eff1, type_in_scope scope eff2 with
  | Some eff1, Some eff2 -> Some (eff1, eff2)
  | _ -> None

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
let supereffect_in_scope scope (eff : effct) =
  type_in_scope scope eff

(** Tries to find a subeffect of given type, such that all free type
  variables are members of given set ([scope]) *)
let rec subeffect_in_scope scope (eff : effct) =
  match eff with
  | TEffPure -> eff
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
    failwith "Internal error: TApp in effect"

(** Tries to find a supertype of given type, such that all free type variables
  are members of given set ([scope]) *)
let rec supertype_in_scope scope (tp : ttype) =
  match tp with
  | TVar _ | TLabel _ | TData _ | TApp _ -> type_in_scope scope tp
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
  | TGuard(cs, tp) ->
    begin match
      forall_map (constr_in_scope scope) cs,
      supertype_in_scope scope tp
    with
    | Some cs, Some tp -> Some (TGuard(cs, tp))
    | _ -> None
    end

(** Tries to find a subtype of given type, such that all free type variables
  are members of given set ([scope]) *)
and subtype_in_scope scope (tp : ttype) =
  match tp with
  | TVar _ | TLabel _ | TApp _ -> type_in_scope scope tp
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
  | TGuard(cs, tp) ->
    begin match
      forall_map (constr_in_scope scope) cs,
      subtype_in_scope scope tp
    with
    | Some cs, Some tp -> Some (TGuard(cs, tp))
    | _ -> None
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

(** Check if all types on non-positive positions fits in given
  scope. *)
let rec positive : type k. nonrec_scope:_ -> k typ -> bool =
  fun ~nonrec_scope tp ->
  match tp with
  | TVar _ | TEffPure -> true
  | TLabel _ | TData _ ->
    begin match type_in_scope nonrec_scope tp with
    | Some _ -> true
    | None   -> false
    end
  
  | TGuard (constraints, tp) ->
    positive ~nonrec_scope tp &&
    List.for_all (fun (lhs, rhs) ->
      positive ~nonrec_scope lhs &&
      negative ~nonrec_scope rhs
    )
    constraints

  | TEffJoin(eff1, eff2) ->
    positive ~nonrec_scope eff1 &&
    positive ~nonrec_scope eff2
    
  | TArrow(tp1, tp2, eff) ->
    negative ~nonrec_scope tp1 &&
    positive ~nonrec_scope tp2 &&
    positive ~nonrec_scope eff

  | TForall(a, tp) ->
    positive ~nonrec_scope:(TVar.Set.add a nonrec_scope) tp

  | TApp(tp1, tp2) ->
    begin match
      positive ~nonrec_scope tp1,
      type_in_scope nonrec_scope tp2
    with
    | true, Some _ -> true
    | _ -> false
    end

(** Check if all types on non-negative positions fits in given
  scope. *)
and negative : type k. nonrec_scope:_ -> k typ -> bool =
  fun ~nonrec_scope tp ->
  match tp with
  | TEffPure -> true
  | TVar _  | TLabel _ | TData _ | TApp _ | TEffJoin _ ->
    begin match type_in_scope nonrec_scope tp with
    | Some _ -> true
    | None   -> false
    end

  | TGuard _  -> positive ~nonrec_scope tp
    
  | TArrow(tp1, tp2, eff) ->
    positive ~nonrec_scope tp1 &&
    negative ~nonrec_scope tp2 &&
    negative ~nonrec_scope eff

  | TForall(a, tp) ->
    negative ~nonrec_scope:(TVar.Set.add a nonrec_scope) tp

(** Check if all types on non-positive positions fits in given
  scope (for ADT constructors) *)
let positive_ctor ~nonrec_scope ctor =
  let nonrec_scope = add_tvars_to_scope ctor.ctor_tvars nonrec_scope in
  List.for_all (positive ~nonrec_scope) ctor.ctor_arg_types

(** Check if all types on non-positive positions fits in given
  scope (for list of ADT constructors) *)
let positive_ctors ~nonrec_scope ctors =
  List.for_all (positive_ctor ~nonrec_scope) ctors

type ex = Ex : 'k typ -> ex

let t_pure_arrows tps tp =
  List.fold_right (fun tp1 tp2 -> TArrow(tp1, tp2, TEffPure)) tps tp

let t_foralls xs tp =
  List.fold_right (fun (TVar.Ex x) tp -> TForall(x, tp)) xs tp
