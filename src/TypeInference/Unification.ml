(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Unification and subtyping of types *)

open Common

type arrow =
  | Arr_No
  | Arr_UVar
  | Arr_Pure   of T.scheme * T.typ
  | Arr_Impure of T.scheme * T.typ * T.effrow

type handler =
  | H_No
  | H_Handler of T.tvar * T.typ * T.typ * T.effrow

type label =
  | L_No
  | L_NoEffect
  | L_Label of T.effect * T.typ * T.effrow

type error_info =
  | TVarEscapesScope of Env.t * T.tvar

type result =
  | Unify_Success
  | Unify_Fail of error_info list

(** Internal exception *)
exception Error
exception Escapes_scope of Env.t * T.tvar

let unify_with_kuvar x k =
  if T.Kind.contains_uvar x k then
    raise Error
  else if T.KUVar.set x k then
    ()
  else
    (* TODO: Give some information about not-satisfied constraints to the user
      *)
    raise Error

let rec check_kind_equal k1 k2 =
  match T.Kind.view k1, T.Kind.view k2 with
  | KUVar x1, KUVar x2 when T.KUVar.equal x1 x2 -> ()
  | KUVar x, _ -> unify_with_kuvar x k2
  | _, KUVar x -> unify_with_kuvar x k1

  | KType, KType -> ()
  | KType, _ -> raise Error

  | KEffect, KEffect -> ()
  | KEffect, _ -> raise Error

  | KEffrow, KEffrow -> ()
  | KEffrow, _ -> raise Error

  | KArrow(ka1, kv1), KArrow(ka2, kv2) ->
    check_kind_equal ka1 ka2;
    check_kind_equal kv1 kv2
  | KArrow _, _ -> raise Error

let unify_kind k1 k2 =
  match BRef.bracket (fun () -> check_kind_equal k1 k2) with
  | ()              -> true
  | exception Error -> false

let kind_to_arrow k =
  match T.Kind.view k with
  | KUVar x ->
    let k1 = T.Kind.fresh_uvar () in
    let k2 = T.Kind.fresh_uvar ~non_effect:true () in
    if T.KUVar.set x (T.Kind.k_arrow k1 k2) then
      Some (k1, k2)
    else
      None
  | KArrow(k1, k2) -> Some(k1, k2)

  | KType | KEffect | KEffrow -> None

let set_uvar env p u tp =
  let scope = T.UVar.raw_set p u tp in
  match T.Type.try_shrink_scope ~scope tp with
  | Ok   () -> ()
  | Error e -> raise (Escapes_scope (env, e))

let rec unify_named_type_args env sub1 sub2 args1 args2 =
  match args1, args2 with
  | [], [] -> (env, sub1, sub2)
  | (n1, x1) :: args1, (n2, x2) :: args2 ->
    if n1 <> n2 then raise Error;
    let kind = T.TVar.kind x1 in
    check_kind_equal kind (T.TVar.kind x2);
    let (env, x) = Env.add_anon_tvar env kind in
    let sub1 = T.Subst.rename_to_fresh sub1 x1 x in
    let sub2 = T.Subst.rename_to_fresh sub2 x2 x in
    unify_named_type_args env sub1 sub2 args1 args2
  | [], _ :: _ | _ :: _, [] -> raise Error

let rec check_effect_mem env x eff =
  match T.Effect.view eff with
  | RPure | RVar _ | RApp _ -> raise Error
  | RUVar(p, u) ->
    set_uvar env p u (T.Effect.cons x (Env.fresh_uvar env T.Kind.k_effrow))
  | RCons(y, eff) ->
    if T.TVar.equal x y then ()
    else check_effect_mem env x eff

let rec check_subeffect env eff1 eff2 =
  match T.Effect.view eff1 with
  | RPure    -> ()
  | RUVar(p1, u1) ->
    if T.Effect.is_pure eff2 then
      set_uvar env p1 u1 T.Effect.pure
    else
      begin match T.Effect.view_end eff2 with
      | EEUVar(p2, u2) when T.UVar.equal u1 u2 ->
        T.UVar.filter_scope u1 (T.UVar.level u2) (T.TVar.Perm.agree_on p1 p2)
      | _ ->
        (** TODO: add constraint, instead of such combinations *)
        let scope = T.Scope.perm p1 (T.UVar.scope u1) in
        let (xs, ee) = T.Type.effrow_view eff2 in
        let eff2 =
          T.Type.t_effrow (T.TVar.Set.filter (T.Scope.mem scope) xs) ee in
        set_uvar env p1 u1 eff2
      end
  | RVar x1 ->
    begin match T.Effect.view_end eff2 with
    | EEUVar(p, u) ->
      set_uvar env p u (T.Type.t_var x1)
    | EEVar x2 when T.TVar.equal x1 x2 -> ()
    | _ -> raise Error
    end
  | RApp(tp1, tp2) ->
    begin match T.Effect.view_end eff2 with
    | EEUVar(p, u) ->
      set_uvar env p u (T.Type.t_app tp1 tp2)
    | EEApp(tp1', tp2') ->
      unify env (T.Type.t_app tp1 tp2) (T.Type.t_app tp1' tp2')
    | _ -> raise Error
    end
  | RCons(x1, eff1) ->
    check_effect_mem env x1 eff2;
    check_subeffect env eff1 eff2

and unify_at_kind env tp1 tp2 k =
  match T.Kind.view k with
  | KEffrow ->
    check_subeffect env tp1 tp2;
    check_subeffect env tp2 tp1

  | KEffect ->
    let xs1 = T.Type.effect_view tp1 in
    let xs2 = T.Type.effect_view tp2 in
    if T.TVar.Set.equal xs1 xs2 then ()
    else
      raise Error

  | _ -> unify env tp1 tp2

and unify env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TEffect _, _ | _, TEffect _
  | TEffrow _, _ | _, TEffrow _ ->
    (* To unify types that may possibly be effects or effect rows,
      use [unify_at_kind] function. *)
    assert false

  | TUVar(p1, u1), TUVar(p2, u2) when T.UVar.equal u1 u2 ->
    T.UVar.filter_scope u1 (T.UVar.level u2) (T.TVar.Perm.agree_on p1 p2)
  | TUVar(p, u), _ ->
    if T.Type.contains_uvar u tp2 then
      raise Error
    else
      set_uvar env p u tp2
  | _, TUVar(p, u) ->
    if T.Type.contains_uvar u tp1 then
      raise Error
    else
      set_uvar env p u tp1

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, _ -> raise Error

  | TPureArrow(atp1, vtp1), TPureArrow(atp2, vtp2) ->
    unify_scheme env atp1 atp2;
    unify env vtp1 vtp2
  | TPureArrow _, _ -> raise Error

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    unify_scheme env atp1 atp2;
    unify env vtp1 vtp2;
    unify_at_kind env eff1 eff2 T.Kind.k_effrow
  | TArrow _, _ -> raise Error

  | THandler(a1, tp1, rtp1, reff1), THandler(a2, tp2, rtp2, reff2) ->
    let (env, a) = Env.add_anon_tvar env T.Kind.k_effect in
    let sub1 = T.Subst.rename_to_fresh T.Subst.empty a1 a in
    let sub2 = T.Subst.rename_to_fresh T.Subst.empty a2 a in
    unify env (T.Type.subst sub1 tp1) (T.Type.subst sub2 tp2);
    unify env (T.Type.subst sub1 rtp1) (T.Type.subst sub2 rtp2);
    unify_at_kind env
      (T.Type.subst sub1 reff1) (T.Type.subst sub2 reff2) T.Kind.k_effrow
  | THandler _, _ -> raise Error

  | TLabel(eff1, rtp1, reff1), TLabel(eff2, rtp2, reff2) ->
    unify_at_kind env eff1 eff2 T.Kind.k_effect;
    unify env rtp1 rtp2;
    unify_at_kind env reff1 reff2 T.Kind.k_effrow;
  | TLabel _, _ -> raise Error

  | TApp(ftp1, atp1), TApp(ftp2, atp2) ->
    let k1 = T.Type.kind atp1 in
    let k2 = T.Type.kind atp2 in
    check_kind_equal k1 k2;
    unify env ftp1 ftp2;
    unify_at_kind env atp1 atp2 k1
  | TApp _, _ -> raise Error

and unify_scheme env sch1 sch2 =
  if List.length sch1.sch_named <> List.length sch2.sch_named then
    raise Error;
  let (env, sub1, sub2) =
    unify_named_type_args env T.Subst.empty T.Subst.empty
      sch1.sch_targs sch2.sch_targs in
  List.iter2
    (fun (name1, isch1) (name2, isch2) ->
      let name1 = T.Name.subst sub1 name1 in
      let name2 = T.Name.subst sub2 name2 in
      if not (T.Name.equal name1 name2) then raise Error;
      unify_scheme env
        (T.Scheme.subst sub1 isch1) (T.Scheme.subst sub2 isch2))
    sch1.sch_named sch2.sch_named;
  unify env
    (T.Type.subst sub1 sch1.sch_body) (T.Type.subst sub2 sch2.sch_body)

let rec check_subtype env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TEffect _, _ | _, TEffect _
  | TEffrow _, _ | _, TEffrow _ ->
    failwith "Internal kind error"

  | TUVar(p1, u1), TUVar(p2, u2) when T.UVar.equal u1 u2 ->
    T.UVar.filter_scope u1 (T.UVar.level u2) (T.TVar.Perm.agree_on p1 p2)
  | TUVar(p, u), _ ->
    if T.Type.contains_uvar u tp2 then
      raise Error
    else
      set_uvar env p u (T.Type.open_down ~scope:(Env.scope env) tp2)
  | _, TUVar(p, u) ->
    if T.Type.contains_uvar u tp1 then
      raise Error
    else
      set_uvar env p u (T.Type.open_up ~scope:(Env.scope env) tp1)

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, _ -> raise Error

  | TPureArrow(atp1, vtp1), TPureArrow(atp2, vtp2) ->
    check_subscheme env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2
  | TPureArrow(atp1, vtp1), TArrow(atp2, vtp2, _) ->
    check_subscheme env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2
  | TPureArrow _, _ -> raise Error

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    check_subscheme env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2;
    check_subeffect env eff1 eff2
  | TArrow _, _ -> raise Error

  | THandler(a1, tp1, rtp1, reff1), THandler(a2, tp2, rtp2, reff2) ->
    let (env, a) = Env.add_anon_tvar env T.Kind.k_effect in
    let sub1 = T.Subst.rename_to_fresh T.Subst.empty a1 a in
    let sub2 = T.Subst.rename_to_fresh T.Subst.empty a2 a in
    check_subtype env (T.Type.subst sub1 tp1) (T.Type.subst sub2 tp2);
    unify env (T.Type.subst sub1 rtp1) (T.Type.subst sub2 rtp2);
    unify_at_kind env
      (T.Type.subst sub1 reff1) (T.Type.subst sub2 reff2) T.Kind.k_effrow
  | THandler _, _ -> raise Error

  | TLabel(eff1, rtp1, reff1), TLabel(eff2, rtp2, reff2) ->
    unify_at_kind env eff1 eff2 T.Kind.k_effect;
    unify env rtp1 rtp2;
    unify_at_kind env reff1 reff2 T.Kind.k_effrow
  | TLabel _, _ -> raise Error

  | TApp _, TApp _ -> unify env tp1 tp2
  | TApp _, _ -> raise Error

and check_subscheme env sch1 sch2 =
  if List.length sch1.sch_named <> List.length sch2.sch_named then
    raise Error;
  let (env, sub1, sub2) =
    unify_named_type_args env T.Subst.empty T.Subst.empty
      sch1.sch_targs sch2.sch_targs in
  List.iter2
    (fun (name1, isch1) (name2, isch2) ->
      let name1 = T.Name.subst sub1 name1 in
      let name2 = T.Name.subst sub2 name2 in
      if not (T.Name.equal name1 name2) then raise Error;
      (* contravariant *)
      check_subscheme env
        (T.Scheme.subst sub2 isch2) (T.Scheme.subst sub1 isch1))
    sch1.sch_named sch2.sch_named;
  check_subtype env
    (T.Type.subst sub1 sch1.sch_body) (T.Type.subst sub2 sch2.sch_body)

let unify_type env tp1 tp2 =
  match BRef.bracket (fun () -> 
  	unify_at_kind env tp1 tp2 (T.Type.kind tp1)) with
  | ()              -> Unify_Success
  | exception Error -> Unify_Fail []
  | exception Escapes_scope(env, tv) -> Unify_Fail [TVarEscapesScope(env, tv)]

let subeffect env eff1 eff2 =
  match BRef.bracket (fun () -> check_subeffect env eff1 eff2) with
  | ()              -> Unify_Success
  | exception Error -> Unify_Fail []
  | exception Escapes_scope(env, tv) -> Unify_Fail [TVarEscapesScope(env, tv)]

let subtype env tp1 tp2 =
  match BRef.bracket (fun () -> check_subtype env tp1 tp2) with
  | ()              -> Unify_Success
  | exception Error -> Unify_Fail []
  | exception Escapes_scope(env, tv) -> Unify_Fail [TVarEscapesScope(env, tv)]

let subscheme env sch1 sch2 =
  match BRef.bracket (fun () -> check_subscheme env sch1 sch2) with
  | ()              -> Unify_Success
  | exception Error -> Unify_Fail []
  | exception Escapes_scope(env, tv) -> Unify_Fail [TVarEscapesScope(env, tv)]

let to_arrow env tp =
  match T.Type.view tp with
  | TUVar(p, u) ->
    let sch = T.Scheme.of_type (Env.fresh_uvar env T.Kind.k_type) in
    let tp2 = Env.fresh_uvar env T.Kind.k_type in
    let eff = Env.fresh_uvar env T.Kind.k_effrow in
    set_uvar env p u (T.Type.t_arrow sch tp2 eff);
    Arr_Impure(sch, tp2, eff)
  | TPureArrow(sch, tp2)  -> Arr_Pure(sch, tp2)
  | TArrow(tp1, tp2, eff) -> Arr_Impure(tp1, tp2, eff)

  | TVar _ | THandler _ | TLabel _ | TApp _ -> Arr_No

  | TEffect _ | TEffrow _ ->
    failwith "Internal kind error"

let from_arrow env tp =
  match T.Type.view tp with
  | TUVar _ -> Arr_UVar
  | TPureArrow(tp1, tp2) -> Arr_Pure(tp1, tp2)
  | TArrow(tp1, tp2, eff) -> Arr_Impure(tp1, tp2, eff)

  | TVar _ | THandler _ | TLabel _ | TApp _ -> Arr_No

  | TEffect _ | TEffrow _ ->
    failwith "Internal kind error"

let to_handler env tp =
  match T.Type.view tp with
  | TUVar(p, u) ->
    let (env1, a) = Env.add_anon_tvar env T.Kind.k_effect in
    let tp   = Env.fresh_uvar env1 T.Kind.k_type in
    let tp0  = Env.fresh_uvar env1 T.Kind.k_type in
    let eff0 = Env.fresh_uvar env1 T.Kind.k_effrow in
    set_uvar env p u (T.Type.t_handler a tp tp0 eff0);
    H_Handler(a, tp, tp0, eff0)

  | THandler(a, tp, tp0, eff0) ->
    H_Handler(a, tp, tp0, eff0)

  | TVar _ | TPureArrow _ | TArrow _ | TLabel _ | TApp _ -> H_No

  | TEffect _ | TEffrow _ ->
    failwith "Internal kind error"

let from_handler env tp =
  match T.Type.view tp with
  | TUVar(p, u) ->
    let (env1, a) = Env.add_anon_tvar env T.Kind.k_effect in
    let tp   = Env.fresh_uvar env1 T.Kind.k_type in
    let tp0  = Env.fresh_uvar env1 T.Kind.k_type in
    let eff0 = Env.fresh_uvar env1 T.Kind.k_effrow in
    set_uvar env p u (T.Type.t_handler a tp tp0 eff0);
    H_Handler(a, tp, tp0, eff0)

  | THandler(a, tp, tp0, eff0) ->
    H_Handler(a, tp, tp0, eff0)

  | TVar _ | TPureArrow _ | TArrow _ | TLabel _ | TApp _ -> H_No

  | TEffect _ | TEffrow _ ->
    failwith "Internal kind error"

let as_label env tp =
  match T.Type.view tp with
  | TUVar(p, u) ->
    begin match Env.lookup_the_effect env with
    | Some eff ->
      let tp0  = Env.fresh_uvar env T.Kind.k_type in
      let eff0 = Env.fresh_uvar env T.Kind.k_effrow in
      begin match set_uvar env p u (T.Type.t_label eff tp0 eff0) with
      | _ -> L_Label(eff, tp0, eff0)
      | exception Error -> L_NoEffect
      end;
    | None -> L_NoEffect
    end

  | TLabel(eff, tp0, eff0) -> L_Label(eff, tp0, eff0)

  | TVar _ | TPureArrow _ | TArrow _ | THandler _ | TApp _ -> L_No

  | TEffect _ | TEffrow _ ->
    failwith "Internal kind error"

