(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraint solver. *)

open Common

let add_constraint ~origin env eff1 eff2 =
  Env.add_constraints env [Constr.CSubeffect(origin, eff1, eff2)]

(* ========================================================================= *)

let check_sat_solver_result (res : _ IncrSAT.Solver.solve_result) =
  match res with
  | Ok -> ()
  | Error(x, origin) ->
    Error.fatal (Error.escaping_effect_var ~origin x)

let solve_partial env =
  check_sat_solver_result (IncrSAT.Solver.solve_partial (Env.sat_solver env))

(* ========================================================================= *)

let is_effect_var x =
  match Lang.Unif.Kind.view (T.TVar.kind x) with
  | KEffect -> true
  | _ -> false

let gvar_leave_scope ~scope ~tvars gv =
  let _ : T.gvar = T.GVar.update_scope ~scope ~tvars gv in ()

let tvar_constr_leave_scope ~env0 ~origin eff2 (x, p) =
  let scope = Env.scope env0 in
  if T.TVar.in_scope x scope then begin
    let eff1 = T.Effct.guard (T.Effct.var x) p in
    let eff2 = T.Effct.filter_to_scope ~scope eff2 in
    let c = Constr.CSubeffect(origin, eff1, eff2) in
    Env.add_constraints env0 [c]
  end else begin
    let p2 = T.Effct.lookup_tvar eff2 x in
    Env.add_formula_constraint env0 ~origin x p p2
  end

let gvar_constr_leave_scope ~env0 ~origin eff2 (gv, p) =
  let scope = Env.scope env0 in
  assert (T.GVar.in_scope gv scope);
  let eff1 = T.Effct.guard (T.Effct.gvar gv) p in
  let eff2 = T.Effct.filter_to_scope ~scope eff2 in
  let c = Constr.CSubeffect(origin, eff1, eff2) in
  Env.add_constraints env0 [c]

let constr_leave_scope ~env0 c =
  match c with
  | Constr.CSubeffect(origin, eff1, eff2) ->
    let (tvars, gvars) = T.Effct.view eff1 in
    List.iter (tvar_constr_leave_scope ~env0 ~origin eff2) tvars;
    List.iter (gvar_constr_leave_scope ~env0 ~origin eff2) gvars

let leave_scope_with_gvars ~env0 ~tvars cs gvs =
  let scope = Env.scope env0 in
  let gvs = Constr.collect_gvars ~scope cs gvs in
  let tvars = List.filter is_effect_var tvars in
  T.GVar.Set.iter (gvar_leave_scope ~scope ~tvars) gvs;
  List.iter (constr_leave_scope ~env0) cs;
  solve_partial env0

(* ========================================================================= *)

(** Split all generalizable variables that are not in the given scope but
  appear on the RHS of given constraints to a join of fresh generalizable
  variables, one at given scope, and one at the original scope. *)
let prepare_generalize ~scope cs =
  cs
  |> List.fold_left
      (fun gvs (Constr.CSubeffect(_, _, eff)) ->
        T.Effct.collect_gvars ~scope eff gvs)
      T.GVar.Set.empty
  |> T.GVar.Set.elements
  |> List.iter (fun gv ->
    let gv1 = T.Effct.fresh_gvar ~scope in
    let gv2 = T.Effct.fresh_gvar ~scope:(T.GVar.scope gv) in
    T.GVar.set gv (T.Effct.join gv1 gv2))

let split_tvar_constr ~env0 ~origin eff2 (x, p) =
  let scope = Env.scope env0 in
  if T.TVar.in_scope x scope then begin
    let eff1 = T.Effct.guard (T.Effct.var x) p in
    let eff2 = T.Effct.filter_to_scope ~scope eff2 in
    let c = Constr.CSubeffect(origin, eff1, eff2) in
    Env.add_constraints env0 [c];
    []
  end else begin
    let eff1 = T.Effct.guard (T.Effct.var x) p in
    [(eff1, eff2)]
  end

let split_constr ~env0 c =
  match c with
  | Constr.CSubeffect(origin, eff1, eff2) ->
    let (tvars, gvars) = T.Effct.view eff1 in
    List.iter (gvar_constr_leave_scope ~env0 ~origin eff2) gvars;
    List.concat_map (split_tvar_constr ~env0 ~origin eff2) tvars

let effect_gvars ~scope effs =
  List.fold_left
    (fun gvs eff -> T.Effct.collect_gvars ~scope eff gvs)
    T.GVar.Set.empty
    effs

let generalize_with_gvars ~env0 cs (pgvs, ngvs) =
  let effs =
    T.GVar.Set.union pgvs ngvs
    |> T.GVar.Set.elements
    |> List.map T.Effct.gvar
  in
  let scope = Env.scope env0 in
  let cs = ConstrSimplify.simplify ~scope ~pgvs ~ngvs cs in
  prepare_generalize ~scope cs;
  let gvs = Constr.collect_gvars ~scope cs (effect_gvars ~scope effs) in
  let evs = List.map T.GVar.fix (T.GVar.Set.elements gvs) in
  let cs  = List.concat_map (split_constr ~env0) cs in
  (evs, cs)

(* ========================================================================= *)

let leave_scope ~env0 ~tvars cs =
  leave_scope_with_gvars ~env0 ~tvars cs T.GVar.Set.empty

let leave_scope_with_scheme ~env0 ~tvars cs sch =
  let scope = Env.scope env0 in
  leave_scope_with_gvars ~env0 ~tvars cs
    (T.Scheme.collect_gvars ~scope sch T.GVar.Set.empty)

let leave_scope_with_schemes ~env0 ~tvars cs schs =
  let scope = Env.scope env0 in
  leave_scope_with_gvars ~env0 ~tvars cs
    (List.fold_left
      (fun gvs sch -> T.Scheme.collect_gvars ~scope sch gvs)
      T.GVar.Set.empty
      schs)

let leave_scope_with_type_eff ~env0 ~tvars cs tp eff =
  let scope = Env.scope env0 in
  let gvs =
    T.GVar.Set.empty
    |> T.Type.collect_gvars ~scope tp
    |> T.CEffect.collect_gvars ~scope eff
  in
  leave_scope_with_gvars ~env0 ~tvars cs gvs

let leave_scope_with_ctors ~env0 ~tvars cs ctors =
  let scope = Env.scope env0 in
  let gvs =
    List.fold_left
      (fun gvs ctor -> T.CtorDecl.collect_gvars ~scope ctor gvs)
      T.GVar.Set.empty
      ctors
  in
  leave_scope_with_gvars ~env0 ~tvars cs gvs

let generalize_with_scheme ~env0 cs sch =
  let scope = Env.scope env0 in
  let gvsp = (T.GVar.Set.empty, T.GVar.Set.empty) in
  generalize_with_gvars ~env0 cs (T.Scheme.collect_gvars_p ~scope sch gvsp)

let generalize_with_schemes ~env0 cs schs =
  let scope = Env.scope env0 in
  generalize_with_gvars ~env0 cs
    (List.fold_left
      (fun gvsp sch -> T.Scheme.collect_gvars_p ~scope sch gvsp)
      (T.GVar.Set.empty, T.GVar.Set.empty)
      schs)

(* ========================================================================= *)

let gvar_final_solve ~tvars gv =
  let gv = T.GVar.update_scope ~scope:Scope.root ~tvars gv in
  T.GVar.set gv T.Effct.pure

let tvar_constr_final_solve ~origin env eff2 (x, p1) =
  let p2 = T.Effct.lookup_tvar eff2 x in
  Env.add_formula_constraint env ~origin x p1 p2

let constr_final_solve env c =
  match c with
  | Constr.CSubeffect(origin, eff1, eff2) ->
    let (tvars, gvars) = T.Effct.view eff1 in
    assert (List.is_empty gvars);
    List.iter (tvar_constr_final_solve ~origin env eff2) tvars

exception Leave_bracket

let final_sat_solve env =
  if Env.solve_all_is_set env then
    check_sat_solver_result
      (IncrSAT.Solver.solve_all (Env.sat_solver env))
  else begin
    solve_partial env;
    try
      BRef.bracket (fun () ->
        check_sat_solver_result
          (IncrSAT.Solver.solve_all (Env.sat_solver env));
        raise Leave_bracket)
    with
    | Leave_bracket -> ()
  end

let final_solve env =
  let cs  = Env.constraints env in
  Env.clear_constraints env;
  let gvs = Constr.collect_gvars ~scope:Scope.root cs T.GVar.Set.empty in
  let tvars = List.filter is_effect_var (Env.all_tvars env) in
  T.GVar.Set.iter (gvar_final_solve ~tvars) gvs;
  List.iter (constr_final_solve env) cs;
  final_sat_solve env
