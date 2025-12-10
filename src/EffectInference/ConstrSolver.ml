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

let gvar_leave_scope ~outer_scope ~tvars gv =
  let _ : T.gvar = T.GVar.update_scope ~scope:outer_scope ~tvars gv in ()

let tvar_constr_leave_scope ~outer_env ~origin eff2 (x, p) =
  let outer_scope = Env.scope outer_env in
  if T.TVar.in_scope x outer_scope then begin
    let eff1 = T.Effct.guard (T.Effct.var x) p in
    let eff2 = T.Effct.filter_to_scope ~scope:outer_scope eff2 in
    let c = Constr.CSubeffect(origin, eff1, eff2) in
    Env.add_constraints outer_env [c]
  end else begin
    let p2 = T.Effct.lookup_tvar eff2 x in
    Env.add_formula_constraint outer_env ~origin x p p2
  end

let gvar_constr_leave_scope ~outer_env ~origin eff2 (gv, p) =
  let outer_scope = Env.scope outer_env in
  (* All remaining gvars from the inner scope were already promoted to tvars.
   *)
  assert (T.GVar.in_scope gv outer_scope);
  let eff1 = T.Effct.guard (T.Effct.gvar gv) p in
  let eff2 = T.Effct.filter_to_scope ~scope:outer_scope eff2 in
  let c = Constr.CSubeffect(origin, eff1, eff2) in
  Env.add_constraints outer_env [c]

let constr_leave_scope ~outer_env c =
  match c with
  | Constr.CSubeffect(origin, eff1, eff2) ->
    let (tvars, gvars) = T.Effct.view eff1 in
    List.iter (tvar_constr_leave_scope ~outer_env ~origin eff2) tvars;
    List.iter (gvar_constr_leave_scope ~outer_env ~origin eff2) gvars

let leave_scope_with_gvars ~outer_env ~tvars cs gvs =
  let outer_scope = Env.scope outer_env in
  let gvs = Constr.collect_gvars ~outer_scope cs gvs in
  let tvars = List.filter is_effect_var tvars in
  T.GVar.Set.iter (gvar_leave_scope ~outer_scope ~tvars) gvs;
  List.iter (constr_leave_scope ~outer_env) cs;
  solve_partial outer_env

(* ========================================================================= *)

(** Split all generalizable variables that are not in the given outer scope
  but appear on the RHS of given constraints to a join of fresh generalizable
  variables, one at given (outer) scope, and one at the original (inner)
  scope. *)
let prepare_generalize ~outer_scope cs =
  cs
  |> List.fold_left
      (fun gvs (Constr.CSubeffect(_, _, eff)) ->
        T.Effct.collect_gvars ~outer_scope eff gvs)
      T.GVar.Set.empty
  |> T.GVar.Set.elements
  |> List.iter (fun gv ->
    let gv1 = T.Effct.fresh_gvar ~scope:outer_scope in
    let gv2 = T.Effct.fresh_gvar ~scope:(T.GVar.scope gv) in
    T.GVar.set gv (T.Effct.join gv1 gv2))

let split_tvar_constr ~outer_env ~origin eff2 (x, p) =
  let outer_scope = Env.scope outer_env in
  if T.TVar.in_scope x outer_scope then begin
    let eff1 = T.Effct.guard (T.Effct.var x) p in
    let eff2 = T.Effct.filter_to_scope ~scope:outer_scope eff2 in
    let c = Constr.CSubeffect(origin, eff1, eff2) in
    Env.add_constraints outer_env [c];
    []
  end else begin
    let eff1 = T.Effct.guard (T.Effct.var x) p in
    [(eff1, eff2)]
  end

let split_constr ~outer_env c =
  match c with
  | Constr.CSubeffect(origin, eff1, eff2) ->
    let (tvars, gvars) = T.Effct.view eff1 in
    List.iter (gvar_constr_leave_scope ~outer_env ~origin eff2) gvars;
    List.concat_map (split_tvar_constr ~outer_env ~origin eff2) tvars

let effect_gvars ~outer_scope effs =
  List.fold_left
    (fun gvs eff -> T.Effct.collect_gvars ~outer_scope eff gvs)
    T.GVar.Set.empty
    effs

let generalize_with_gvars ~outer_env cs (pgvs, ngvs) =
  let effs =
    T.GVar.Set.union pgvs ngvs
    |> T.GVar.Set.elements
    |> List.map T.Effct.gvar
  in
  let outer_scope = Env.scope outer_env in
  let cs = ConstrSimplify.simplify ~outer_scope ~pgvs ~ngvs cs in
  prepare_generalize ~outer_scope cs;
  let gvs =
    Constr.collect_gvars ~outer_scope cs (effect_gvars ~outer_scope effs) in
  let evs = List.map T.GVar.fix (T.GVar.Set.elements gvs) in
  let cs  = List.concat_map (split_constr ~outer_env) cs in
  (evs, cs)

(* ========================================================================= *)

let leave_scope ~outer_env ~tvars cs =
  leave_scope_with_gvars ~outer_env ~tvars cs T.GVar.Set.empty

let leave_scope_with_scheme ~outer_env ~tvars cs sch =
  let outer_scope = Env.scope outer_env in
  leave_scope_with_gvars ~outer_env ~tvars cs
    (T.Scheme.collect_gvars ~outer_scope sch T.GVar.Set.empty)

let leave_scope_with_schemes ~outer_env ~tvars cs schs =
  let outer_scope = Env.scope outer_env in
  leave_scope_with_gvars ~outer_env ~tvars cs
    (List.fold_left
      (fun gvs sch -> T.Scheme.collect_gvars ~outer_scope sch gvs)
      T.GVar.Set.empty
      schs)

let leave_scope_with_type_eff ~outer_env ~tvars cs tp eff =
  let outer_scope = Env.scope outer_env in
  let gvs =
    T.GVar.Set.empty
    |> T.Type.collect_gvars ~outer_scope tp
    |> T.CEffect.collect_gvars ~outer_scope eff
  in
  leave_scope_with_gvars ~outer_env ~tvars cs gvs

let leave_scope_with_ctors ~outer_env ~tvars cs ctors =
  let outer_scope = Env.scope outer_env in
  let gvs =
    List.fold_left
      (fun gvs ctor -> T.CtorDecl.collect_gvars ~outer_scope ctor gvs)
      T.GVar.Set.empty
      ctors
  in
  leave_scope_with_gvars ~outer_env ~tvars cs gvs

let generalize_with_scheme ~outer_env cs sch =
  let outer_scope = Env.scope outer_env in
  let gvsp = (T.GVar.Set.empty, T.GVar.Set.empty) in
  generalize_with_gvars ~outer_env cs
    (T.Scheme.collect_gvars_p ~outer_scope sch gvsp)

let generalize_with_schemes ~outer_env cs schs =
  let outer_scope = Env.scope outer_env in
  generalize_with_gvars ~outer_env cs
    (List.fold_left
      (fun gvsp sch -> T.Scheme.collect_gvars_p ~outer_scope sch gvsp)
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
  let gvs = Constr.collect_gvars ~outer_scope:Scope.root cs T.GVar.Set.empty in
  let tvars = List.filter is_effect_var (Env.all_tvars env) in
  T.GVar.Set.iter (gvar_final_solve ~tvars) gvs;
  List.iter (constr_final_solve env) cs;
  final_sat_solve env
