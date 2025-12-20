(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Collecting constraints arising from subtyping. *)

open Common

let open_named_tvars1 ~scope tvs =
  List.fold_left_map
    (fun sub (name, x) ->
      let y = T.TVar.clone ~scope x in
      let sub = T.Subst.rename sub x y in
      (sub, (name, y)))
    T.Subst.empty
    tvs

let open_named_tvars2 ~scope tvs1 tvs2 =
  assert (List.length tvs1 = List.length tvs2);
  let ((sub1, sub2), tvars) =
    List.fold_left_map
      (fun (sub1, sub2) ((_, x1), (_, x2)) ->
        let x = T.TVar.clone ~scope x1 in
        let sub1 = T.Subst.rename sub1 x1 x in
        let sub2 = T.Subst.rename sub2 x2 x in
        ((sub1, sub2), x))
      (T.Subst.empty, T.Subst.empty)
      (List.combine tvs1 tvs2)
  in
  (sub1, sub2, tvars)

(* ========================================================================= *)

let effect_equiv ~origin env eff1 eff2 =
  ConstrSolver.add_constraint ~origin env eff1 eff2;
  ConstrSolver.add_constraint ~origin env eff2 eff1

let effect_equiv' ~origin env eff1 eff2 =
  effect_equiv ~origin:(OEffectEquiv(origin, eff1, eff2)) env eff1 eff2

let ceffect_equiv ~origin env eff1 eff2 =
  match eff1, eff2 with
  | T.Pure, T.Pure -> ()
  | T.Pure, _ ->
    failwith "Internal effect error"

  | T.Impure eff1, T.Impure eff2 ->
    effect_equiv' ~origin env eff1 eff2
  | T.Impure _, _ ->
    failwith "Internal effect error"

let rec type_equiv ~origin env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TEffect _, _ | _, TEffect _ ->
    effect_equiv' ~origin env (T.Type.to_effect tp1) (T.Type.to_effect tp2)

  | TAlias(id1, _), TAlias(id2, _) when id1 = id2 -> ()
  | TAlias(_, tp1), _ -> type_equiv ~origin env tp1 tp2
  | _, TAlias(_, tp2) -> type_equiv ~origin env tp1 tp2

  | TVar x, TVar y ->  assert (T.TVar.equal x y)
  | TVar _, _ ->
    failwith "Internal type error"

  | TArrow(sch1, tp1, eff1), TArrow(sch2, tp2, eff2) ->
    scheme_equiv  ~origin env sch1 sch2;
    type_equiv    ~origin env tp1  tp2;
    ceffect_equiv ~origin env eff1 eff2
  | TArrow _, _ ->
    failwith "Internal type error"

  | TLabel(eff1, delim_tp1, delim_eff1), TLabel(eff2, delim_tp2, delim_eff2) ->
    effect_equiv' ~origin env eff1       eff2;
    type_equiv    ~origin env delim_tp1  delim_tp2;
    effect_equiv' ~origin env delim_eff1 delim_eff2
  | TLabel _, _ ->
    failwith "Internal type error"

  | THandler h1, THandler h2 ->
    begin
      let env0 = env in
      let env  = Env.enter_scope env in
      let x = T.TVar.clone ~scope:(Env.scope env) h1.tvar in
      let sub1 = T.Subst.rename T.Subst.empty h1.tvar x in
      let sub2 = T.Subst.rename T.Subst.empty h2.tvar x in
      type_equiv ~origin env 
        (T.Type.subst sub1 h1.cap_tp)   (T.Type.subst sub2 h2.cap_tp);
      type_equiv ~origin env
        (T.Type.subst sub1 h1.in_tp)    (T.Type.subst sub2 h2.in_tp);
      effect_equiv' ~origin env
        (T.Effct.subst sub1 h1.in_eff)  (T.Effct.subst sub2 h2.in_eff);
      ConstrSolver.leave_scope ~env0 ~tvars:[x] (Env.constraints env)
    end;
    type_equiv ~origin env h1.out_tp h2.out_tp;
    effect_equiv' ~origin env h1.out_eff h2.out_eff
  | THandler _, _ ->
    failwith "Internal type error"

  | TApp(ftp1, atp1), TApp(ftp2, atp2) ->
    type_equiv ~origin env ftp1 ftp2;
    type_equiv ~origin env atp1 atp2
  | TApp _, _ ->
    failwith "Internal type error"

and scheme_equiv ~origin env sch1 sch2 =
  match sch1.sch_targs, sch2.sch_targs with
  | [], [] ->
    assert (List.length sch1.sch_named = List.length sch2.sch_named);
    List.iter2 (named_scheme_equiv ~origin env) sch1.sch_named sch2.sch_named;
    type_equiv ~origin env sch1.sch_body sch2.sch_body

  | _ ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (sub1, sub2, tvars) =
      open_named_tvars2 ~scope:(Env.scope env) sch1.sch_targs sch2.sch_targs
    in
    assert (List.length sch1.sch_named = List.length sch2.sch_named);
    List.iter2
      (fun (_, sch1) (_, sch2) ->
        scheme_equiv ~origin env
          (T.Scheme.subst sub1 sch1) (T.Scheme.subst sub2 sch2))
      sch1.sch_named sch2.sch_named;
    type_equiv ~origin env
      (T.Type.subst sub1 sch1.sch_body)
      (T.Type.subst sub2 sch2.sch_body);
    ConstrSolver.leave_scope ~env0 ~tvars (Env.constraints env)

and named_scheme_equiv ~origin env (_, sch1) (_, sch2) =
  scheme_equiv ~origin env sch1 sch2

(* ========================================================================= *)

let subeffect' ~origin env eff1 eff2 =
  ConstrSolver.add_constraint ~origin:(OSubEffect(origin, eff1, eff2))
    env eff1 eff2

let subceffect ~origin env (eff1 : T.ceffect) (eff2 : T.ceffect) =
  match eff1, eff2 with
  | Pure, _ -> ()
  | Impure eff1, Impure eff2 ->
    ConstrSolver.add_constraint ~origin env eff1 eff2
  | Impure _, Pure ->
    failwith "Internal effect error"

let subceffect' ~origin env (eff1 : T.ceffect) (eff2 : T.ceffect) =
  match eff1, eff2 with
  | Pure, _ -> ()
  | Impure eff1, Impure eff2 -> subeffect' ~origin env eff1 eff2
  | Impure _, Pure ->
    failwith "Internal effect error"

let rec subtype ~origin env tp1 tp2 =
  match T.Type.view tp1, T.Type.view tp2 with
  | TAlias(id1, _), TAlias(id2, _) when id1 = id2 -> ()
  | TAlias(_, tp1), _ -> subtype ~origin env tp1 tp2
  | _, TAlias(_, tp2) -> subtype ~origin env tp1 tp2

  | (TVar _ | TLabel _ | TApp _), _ -> type_equiv ~origin env tp1 tp2

  | TArrow(sch1, tp1, eff1), TArrow(sch2, tp2, eff2) ->
    subscheme   ~origin env sch2 sch1; (* contravariant *)
    subtype     ~origin env tp1  tp2;
    subceffect' ~origin env eff1 eff2
  | TArrow _, _ ->
    failwith "Internal type error"

  | THandler h1, THandler h2 ->
    begin
      let env0 = env in
      let env  = Env.enter_scope env in
      let x = T.TVar.clone ~scope:(Env.scope env) h1.tvar in
      let sub1 = T.Subst.rename T.Subst.empty h1.tvar x in
      let sub2 = T.Subst.rename T.Subst.empty h2.tvar x in
      subtype ~origin env 
        (T.Type.subst sub1 h1.cap_tp)   (T.Type.subst sub2 h2.cap_tp);
      subtype ~origin env (* contravariant *)
        (T.Type.subst sub2 h2.in_tp)    (T.Type.subst sub1 h1.in_tp);
      subeffect' ~origin env (*contravariant *)
        (T.Effct.subst sub2 h2.in_eff) (T.Effct.subst sub1 h1.in_eff);
      ConstrSolver.leave_scope ~env0 ~tvars:[x] (Env.constraints env)
    end;
    subtype ~origin env h1.out_tp  h2.out_tp;
    subeffect' ~origin env h1.out_eff h2.out_eff
  | THandler _, _ ->
    failwith "Internal type error"

  | TEffect _, _ ->
    failwith "Internal kind error"

and subscheme ~origin env (sch1 : T.scheme) (sch2 : T.scheme) =
  match sch1.sch_targs, sch2.sch_targs with
  | [], [] ->
    assert (List.length sch1.sch_named = List.length sch2.sch_named);
    (* contravariant *)
    List.iter2 (named_subscheme ~origin env) sch2.sch_named sch1.sch_named;
    (* covariant *)
    subtype ~origin env sch1.sch_body sch2.sch_body

  | _ ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (sub1, sub2, tvars) =
      open_named_tvars2 ~scope:(Env.scope env) sch1.sch_targs sch2.sch_targs
    in
    assert (List.length sch1.sch_named = List.length sch2.sch_named);
    List.iter2
      (fun (_, sch1) (_, sch2) ->
        (* contravariant *)
        subscheme ~origin env
          (T.Scheme.subst sub2 sch2) (T.Scheme.subst sub1 sch1))
      sch1.sch_named sch2.sch_named;
    (* covariant *)
    subtype ~origin env
      (T.Type.subst sub1 sch1.sch_body)
      (T.Type.subst sub2 sch2.sch_body);
    ConstrSolver.leave_scope ~env0 ~tvars (Env.constraints env)

and named_subscheme ~origin env (name1, sch1) (name2, sch2) =
  assert (name1 = name2);
  subscheme ~origin env sch1 sch2

(* ========================================================================= *)

let ceffect_shape env (eff : T.ceffect) =
  match eff with
  | Pure     -> T.Pure
  | Impure _ -> T.Impure (Env.fresh_gvar env)

let rec type_shape env tp =
  match T.Type.view tp with
  | TVar x ->
    if T.TVar.in_scope x (Env.scope env) then T.Type.t_var x
    else
      begin match Lang.Unif.Kind.view (T.TVar.kind x) with
      | KEffect -> T.Type.t_effect (Env.fresh_gvar env)
      | _ -> assert false
      end

  | TArrow(sch, tp, eff) ->
    T.Type.t_arrow
      (scheme_shape  env sch)
      (type_shape    env tp)
      (ceffect_shape env eff)

  | TLabel(_, delim_tp, _) ->
    T.Type.t_label
      (Env.fresh_gvar env)
      (type_shape env delim_tp)
      (Env.fresh_gvar env)

  | THandler h ->
    let out_tp  = type_shape env h.out_tp in
    let out_eff = Env.fresh_gvar env in
    let env0 = env in
    let env = Env.enter_scope env in
    let x = T.TVar.clone ~scope:(Env.scope env) h.tvar in
    let sub = T.Subst.rename T.Subst.empty h.tvar x in
    let cap_tp = type_shape env (T.Type.subst sub h.cap_tp) in
    let in_tp  = type_shape env (T.Type.subst sub h.in_tp) in
    let in_eff = Env.fresh_gvar env in
    let tp = T.Type.t_handler x cap_tp in_tp in_eff out_tp out_eff in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:[x]
      (Env.constraints env) (T.Scheme.of_type tp);
    tp

  | TEffect _ -> T.Type.t_effect (Env.fresh_gvar env)

  | TApp(tp1, tp2) ->
    T.Type.t_app (type_shape env tp1) (type_shape env tp2)

  | TAlias(_, tp) ->
    type_shape env tp

and scheme_shape env (sch : T.scheme) =
  match sch.sch_targs with
  | [] ->
    { T.sch_targs = [];
      T.sch_named = List.map (named_scheme_shape env) sch.sch_named;
      T.sch_body  = type_shape env sch.sch_body
    }

  | _ ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (sub, targs) =
      open_named_tvars1 ~scope:(Env.scope env) sch.sch_targs in
    let named =
      List.map
        (fun (name, sch) ->
          (name, scheme_shape env (T.Scheme.subst sub sch)))
        sch.sch_named
    in
    let sch =
      { T.sch_targs = targs;
        T.sch_named = named;
        T.sch_body  = type_shape env (T.Type.subst sub sch.sch_body)
      }
    in
    ConstrSolver.leave_scope_with_scheme ~env0 ~tvars:(List.map snd targs)
      (Env.constraints env) sch;
    sch

and named_scheme_shape env (name, sch) =
  (name, scheme_shape env sch)

(* ========================================================================= *)

let as_arrow tp =
  match T.Type.view tp with
  | TArrow(sch, tp, eff) -> (sch, tp, eff)
  | _ -> failwith "Internal type error"

let as_label tp =
  match T.Type.view tp with
  | TLabel(eff, delim_tp, delim_eff) -> (eff, delim_tp, delim_eff)
  | _ -> failwith "Internal type error"

let as_handler tp =
  match T.Type.view tp with
  | THandler h -> (h.tvar, h.cap_tp, h.in_tp, h.in_eff, h.out_tp, h.out_eff)
  | _ -> failwith "Internal type error"
