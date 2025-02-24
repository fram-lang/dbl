(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Unification and subtyping of types *)

open Common

type arrow =
  | Arr_No
  | Arr_UVar
  | Arr_Arrow of T.scheme * T.typ * T.effct

type handler =
  | H_No
  | H_Handler of T.tvar * T.typ * T.typ * T.typ

type label =
  | L_No
  | L_Label of T.typ

type error_info =
  | TVarEscapesScope of PPTree.t * T.tvar

type result =
  | Unify_Success
  | Unify_Fail of error_info list

(** Internal exception *)
exception Error
exception Escapes_scope of PPTree.t * T.tvar

let unify_with_kuvar x k =
  if T.Kind.contains_uvar x k then
    raise Error
  else T.KUVar.set x k

let rec check_kind_equal k1 k2 =
  match T.Kind.view k1, T.Kind.view k2 with
  | KUVar x1, KUVar x2 when T.KUVar.equal x1 x2 -> ()

  | KUVar x, _ -> unify_with_kuvar x k2
  | _, KUVar x -> unify_with_kuvar x k1

  | KType, KType -> ()
  | KType, _ -> raise Error

  | KEffect, KEffect -> ()
  | KEffect, _ -> raise Error

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
    let k2 = T.Kind.fresh_uvar () in
    T.KUVar.set x (T.Kind.k_arrow k1 k2);
    Some (k1, k2)
  | KArrow(k1, k2) -> Some(k1, k2)

  | KType | KEffect -> None

let set_uvar env u tp =
  if T.Type.contains_uvar u tp then
    raise Error
  else
    let scope = T.UVar.raw_set u tp in
    match T.Type.try_shrink_scope ~scope tp with
    | Ok   () -> ()
    | Error e -> raise (Escapes_scope (Env.pp_tree env, e))

let rec unify_named_type_args env sub1 sub2 args1 args2 =
  match args1, args2 with
  | [], [] -> (env, sub1, sub2)
  | (n1, x1) :: args1, (n2, x2) :: args2 ->
    if n1 <> n2 then raise Error;
    let kind = T.TVar.kind x1 in
    check_kind_equal kind (T.TVar.kind x2);
    let (env, x) = Env.add_anon_tvar env kind in
    let sub1 = T.Subst.rename_tvar sub1 x1 x in
    let sub2 = T.Subst.rename_tvar sub2 x2 x in
    unify_named_type_args env sub1 sub2 args1 args2
  | [], _ :: _ | _ :: _, [] -> raise Error

let rec unify_at_kind env tp1 tp2 k =
  match T.Kind.view k with
  | KEffect ->
    (* In the type-checking phase we treat all effects as equal. *)
    ()
  | _ -> unify env tp1 tp2

and unify env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TEffect, _ | _, TEffect ->
    (* To unify types that may possibly be effects, use [unify_at_kind]
      function. *)
    assert false

  | TUVar u1, TUVar u2 when T.UVar.equal u1 u2 -> ()
  | TUVar u, _ -> set_uvar env u tp2
  | _, TUVar u -> set_uvar env u tp1

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, _ -> raise Error

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) when eff1 = eff2 ->
    unify_scheme env atp1 atp2;
    unify env vtp1 vtp2
  | TArrow _, _ -> raise Error

  | THandler(a1, cap_tp1, tp_in1, tp_out1),
    THandler(a2, cap_tp2, tp_in2, tp_out2) ->
    unify env tp_out1 tp_out2;
    let (env, scope) = Env.enter_scope env in
    let (env, a) = Env.add_anon_tvar env T.Kind.k_effect in
    let sub1 = T.Subst.rename_tvar (T.Subst.empty ~scope) a1 a in
    let sub2 = T.Subst.rename_tvar (T.Subst.empty ~scope) a2 a in
    unify env (T.Type.subst sub1 cap_tp1) (T.Type.subst sub2 cap_tp2);
    unify env (T.Type.subst sub1 tp_in1) (T.Type.subst sub2 tp_in2)
  | THandler _, _ -> raise Error

  | TLabel delim_tp1, TLabel delim_tp2 ->
    unify env delim_tp1 delim_tp2
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
  let (env, scope) = Env.enter_scope env in
  let (env, sub1, sub2) =
    unify_named_type_args env (T.Subst.empty ~scope) (T.Subst.empty ~scope)
      sch1.sch_targs sch2.sch_targs in
  List.iter2
    (fun (name1, isch1) (name2, isch2) ->
      if not (T.Name.equal name1 name2) then raise Error;
      unify_scheme env
        (T.Scheme.subst sub1 isch1) (T.Scheme.subst sub2 isch2))
    sch1.sch_named sch2.sch_named;
  unify env
    (T.Type.subst sub1 sch1.sch_body) (T.Type.subst sub2 sch2.sch_body)

let rec check_subtype env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TEffect, _ | _, TEffect ->
    failwith "Internal kind error"

  | TUVar u1, TUVar u2 when T.UVar.equal u1 u2 -> ()

  | TUVar u, _ -> set_uvar env u tp2
  | _, TUVar u -> set_uvar env u tp1

  | TVar x, TVar y ->
    if T.TVar.equal x y then ()
    else raise Error
  | TVar _, _ -> raise Error

  | TArrow(atp1, vtp1, eff1), TArrow(atp2, vtp2, eff2) ->
    check_subscheme env atp2 atp1; (* contravariant *)
    check_subtype env vtp1 vtp2;
    begin match eff1, eff2 with
    | Pure,   _      -> ()
    | _,      Impure -> ()
    | Impure, Pure   -> raise Error
    end
  | TArrow _, _ -> raise Error

  | THandler(a1, cap_tp1, tp_in1, tp_out1),
    THandler(a2, cap_tp2, tp_in2, tp_out2) ->
    check_subtype env tp_out1 tp_out2;
    let (env, scope) = Env.enter_scope env in
    let (env, a) = Env.add_anon_tvar env T.Kind.k_effect in
    let sub1 = T.Subst.rename_tvar (T.Subst.empty ~scope) a1 a in
    let sub2 = T.Subst.rename_tvar (T.Subst.empty ~scope) a2 a in
    check_subtype env (T.Type.subst sub1 cap_tp1) (T.Type.subst sub2 cap_tp2);
    (* contravariant *)
    check_subtype env (T.Type.subst sub1 tp_in2) (T.Type.subst sub2 tp_in1)
  | THandler _, _ -> raise Error

  | TLabel delim_tp1, TLabel delim_tp2 ->
    unify env delim_tp1 delim_tp2
  | TLabel _, _ -> raise Error

  | TApp _, TApp _ -> unify env tp1 tp2
  | TApp _, _ -> raise Error

and check_subscheme env (sch1 : T.scheme) (sch2 : T.scheme) =
  if List.length sch1.sch_named <> List.length sch2.sch_named then
    raise Error;
  let (env, scope) = Env.enter_scope env in
  let (env, sub1, sub2) =
    unify_named_type_args env (T.Subst.empty ~scope) (T.Subst.empty ~scope)
      sch1.sch_targs sch2.sch_targs in
  List.iter2
    (fun (name1, isch1) (name2, isch2) ->
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
  | TUVar u ->
    let sch = T.Scheme.of_type (Env.fresh_uvar env T.Kind.k_type) in
    let tp2 = Env.fresh_uvar env T.Kind.k_type in
    set_uvar env u (T.Type.t_arrow sch tp2 Impure);
    Arr_Arrow(sch, tp2, Impure)

  | TArrow(tp1, tp2, eff) -> Arr_Arrow(tp1, tp2, eff)

  | TVar _ | THandler _ | TLabel _ | TApp _ -> Arr_No

  | TEffect ->
    failwith "Internal kind error"

let from_arrow env tp =
  match T.Type.view tp with
  | TUVar _ -> Arr_UVar
  | TArrow(tp1, tp2, eff) -> Arr_Arrow(tp1, tp2, eff)

  | TVar _ | THandler _ | TLabel _ | TApp _ -> Arr_No

  | TEffect ->
    failwith "Internal kind error"

let to_handler env tp =
  match T.Type.view tp with
  | TUVar u ->
    let (env1, _) = Env.enter_scope env in
    let (_, a) = Env.add_anon_tvar env1 T.Kind.k_effect in
    let cap_tp = Env.fresh_uvar env T.Kind.k_type in
    let tp_in  = Env.fresh_uvar env T.Kind.k_type in
    let tp_out = Env.fresh_uvar env T.Kind.k_type in
    set_uvar env u (T.Type.t_handler a cap_tp tp_in tp_out);
    H_Handler(a, cap_tp, tp_in, tp_out)

  | THandler(a, tp, tp_in, tp_out) ->
    H_Handler(a, tp, tp_in, tp_out)

  | TVar _ | TArrow _ | TLabel _ | TApp _ -> H_No

  | TEffect ->
    failwith "Internal kind error"

let from_handler env tp =
  match T.Type.view tp with
  | TUVar u ->
    let (env1, _) = Env.enter_scope env in
    let (_, a) = Env.add_anon_tvar env1 T.Kind.k_effect in
    let cap_tp = Env.fresh_uvar env T.Kind.k_type in
    let tp_in  = Env.fresh_uvar env T.Kind.k_type in
    let tp_out = Env.fresh_uvar env T.Kind.k_type in
    set_uvar env u (T.Type.t_handler a cap_tp tp_in tp_out);
    H_Handler(a, cap_tp, tp_in, tp_out)

  | THandler(a, tp, tp_in, tp_out) ->
    H_Handler(a, tp, tp_in, tp_out)

  | TVar _ | TArrow _ | TLabel _ | TApp _ -> H_No

  | TEffect ->
    failwith "Internal kind error"

let to_label env tp =
  match T.Type.view tp with
  | TUVar u ->
    let tp0 = Env.fresh_uvar env T.Kind.k_type in
    set_uvar env u (T.Type.t_label tp0);
    L_Label tp0

  | TLabel delim_tp -> L_Label delim_tp

  | TVar _ | TArrow _ | THandler _ | TApp _ -> L_No

  | TEffect ->
    failwith "Internal kind error"
