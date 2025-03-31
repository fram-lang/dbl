(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraint simplification before generalization *)

open Common

type state =
  {         scope : Scope.t;
    mutable pgvs  : T.GVar.Set.t;
    mutable ngvs  : T.GVar.Set.t;
    mutable irr   : Constr.t list (** Irrelevant constraints. *)
  }

let normalize_tvar_constr st orig eff2 (x, p) =
  if IncrSAT.Formula.implies p (T.Effct.lookup_tvar eff2 x) then []
  else
    let eff1 = T.Effct.guard (T.Effct.var x) p in
    [ Constr.CSubeffect(orig, eff1, eff2) ]

let normalize_gvar_constr st orig eff2 (gv, p) =
  if IncrSAT.Formula.implies p (T.Effct.lookup_gvar eff2 gv) then []
  else
    let eff1 = T.Effct.guard (T.Effct.gvar gv) p in
    if T.GVar.in_scope gv st.scope then begin
      [ Constr.CSubeffect(orig, eff1, eff2) ]
    end else
      [ Constr.CSubeffect(orig, eff1, eff2) ]

let normalize st (Constr.CSubeffect(orig, eff1, eff2)) =
  let (tvs, gvs) = T.Effct.view eff1 in
  List.concat_map (normalize_tvar_constr st orig eff2) tvs @
  List.concat_map (normalize_gvar_constr st orig eff2) gvs

(** Variables that appears on the left-hand-side of any constraint. *)
let lhs_gvars st cs =
  let scope = st.scope in
  List.fold_left
    (fun gvs (Constr.CSubeffect(_, eff, _)) ->
      T.Effct.collect_gvars ~scope eff gvs)
    T.GVar.Set.empty
    cs

(** Variables that appears on the right-hand-side of any constraint. *)
let rhs_gvars st cs =
  let scope = st.scope in
  List.fold_left
    (fun gvs (Constr.CSubeffect(_, _, eff)) ->
      T.Effct.collect_gvars ~scope eff gvs)
    T.GVar.Set.empty
    cs

let close_positive st gv =
  st.pgvs <- T.GVar.Set.remove gv st.pgvs;
  T.GVar.set gv T.Effct.pure

let set_negative st (gv, eff) =
  if IncrSAT.Formula.is_false (T.Effct.lookup_gvar eff gv) then begin
    if T.GVar.Set.mem gv st.ngvs then begin
      let (_, tvars) = T.Effct.view eff in
      List.iter
        (fun (gv', _) ->
          if not (T.GVar.in_scope gv' st.scope) then
            st.ngvs <- T.GVar.Set.add gv' st.ngvs)
        tvars;
      st.ngvs <- T.GVar.Set.remove gv st.ngvs
    end;
    T.GVar.set gv eff
  end

let build_gvar_bounds st eff2 bnd (gv, p) =
  if T.GVar.in_scope gv st.scope then bnd
  else if T.GVar.Set.mem gv st.pgvs then bnd
  else if not (IncrSAT.Formula.is_true p) then
    T.GVar.Map.add gv None bnd
  else if not (IncrSAT.Formula.is_false (T.Effct.lookup_gvar eff2 gv)) then
    T.GVar.Map.add gv None bnd
  else
    match T.GVar.Map.find_opt gv bnd with
    | Some None -> bnd
    | Some (Some _) -> T.GVar.Map.add gv None bnd
    | None -> T.GVar.Map.add gv (Some eff2) bnd

let build_bounds st bnd (Constr.CSubeffect(_, eff1, eff2)) =
  let (_, gvars) = T.Effct.view eff1 in
  List.fold_left (build_gvar_bounds st eff2) bnd gvars

(* ========================================================================= *)

let rec simplify_loop st cs =
  let cs = List.concat_map (normalize st) cs in
  simplify_positive st cs

(** Set positive variables to pure effect *)
and simplify_positive st cs =
  let lhs = lhs_gvars st cs in
  let rhs = rhs_gvars st cs in
  let positive =
    T.GVar.Set.diff
      (T.GVar.Set.union lhs st.pgvs)
      (T.GVar.Set.union rhs st.ngvs)
  in
  if T.GVar.Set.is_empty positive then
    simplify_negative st cs
  else begin
    T.GVar.Set.iter (close_positive st) positive;
    simplify_loop st cs
  end
    
(** Set negative variables with only one upper-bound to their bound. *)
and simplify_negative st cs =
  let bounds = List.fold_left (build_bounds st) T.GVar.Map.empty cs in
  let bounds =
    List.filter_map
      (fun (gv, bnd) -> Option.map (fun eff -> (gv, eff)) bnd)
      (T.GVar.Map.bindings bounds)
  in
  match bounds with
  | [] -> cs
  | _ ->
    List.iter (set_negative st) bounds;
    simplify_loop st cs

let simplify ~scope ~pgvs ~ngvs cs =
  let st = { scope; pgvs; ngvs; irr = [] } in
  let cs = simplify_loop st cs in
  cs @ st.irr
