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

(** [set_variant_nb st cs gv] sets [gv] to the join of all its lower-bounds
    in [cs]. The variable [gv] is assumed to have no positive nor negative
    occurrence, i.e., it should not appear in [st.pgvs] nor [st.ngvs]. *)
let set_variant_nb st cs gv =
  assert (not (T.GVar.Set.mem gv st.pgvs));
  assert (not (T.GVar.Set.mem gv st.ngvs));
  let lower_bound =
    List.fold_left
      (fun lb (Constr.CSubeffect(_, eff1, eff2)) ->
        let p = T.Effct.lookup_gvar eff2 gv in
        if not (IncrSAT.Formula.is_false p) then
          T.Effct.join lb (T.Effct.guard eff1 p)
        else
          lb)
      T.Effct.pure
      cs
  in
  T.GVar.set gv lower_bound

(** Check if effect [eff1] is trivially a subeffect of [eff2]. *)
let trivial_subeffect eff1 eff2 =
  let (tvs, gvs) = T.Effct.view eff1 in
  List.for_all
    (fun (x, p1) ->
      IncrSAT.Formula.implies p1 (T.Effct.lookup_tvar eff2 x))
    tvs &&
  List.for_all
    (fun (gv, p1) ->
      IncrSAT.Formula.implies p1 (T.Effct.lookup_gvar eff2 gv))
    gvs

(** Check if constraint [c1] is implied by constraint [c2]. *)
let subsumes c1 c2 =
  let (Constr.CSubeffect(_, eff_l1, eff_r1)) = c1 in
  let (Constr.CSubeffect(_, eff_l2, eff_r2)) = c2 in
  trivial_subeffect eff_l1 eff_l2 && trivial_subeffect eff_r2 eff_r1

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
  | [] -> simplify_variant_nb st cs
  | _ ->
    List.iter (set_negative st) bounds;
    simplify_loop st cs

(** Set variant variables (with no positive nor negative occurrence) with no
  upper-bound to the join of all lower-bounds. *)
and simplify_variant_nb st cs =
  let lhs = lhs_gvars st cs in
  let rhs = rhs_gvars st cs in
  let variant_nb =
    rhs
    |> Fun.flip T.GVar.Set.diff st.ngvs
    |> Fun.flip T.GVar.Set.diff st.pgvs
    |> Fun.flip T.GVar.Set.diff lhs
  in
  if T.GVar.Set.is_empty variant_nb then
    remove_redundant st cs
  else begin
    T.GVar.Set.iter (set_variant_nb st cs) variant_nb;
    simplify_loop st cs
  end

(** Remove redundant constraints. *)
and remove_redundant st cs =
  let modified = ref false in
  let rec loop acc cs =
    match cs with
    | [] ->
      if !modified then
        simplify_loop st acc
      else
        acc
    | c :: cs when (List.exists (subsumes c) (acc @ cs)) ->
      modified := true;
      loop acc cs
    | c :: cs ->
      loop (c :: acc) cs
  in
  loop [] cs

let simplify ~scope ~pgvs ~ngvs cs =
  let st = { scope; pgvs; ngvs; irr = [] } in
  let cs = simplify_loop st cs in
  cs @ st.irr
