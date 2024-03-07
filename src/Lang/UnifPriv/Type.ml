(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Basic operations on types *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

let view = TypeBase.view

let fresh_uvar ~scope kind = t_uvar TVar.Perm.id (UVar.fresh ~scope kind)

let t_unit = t_var BuiltinType.tv_unit

let t_pure_arrows tps tp = List.fold_right t_pure_arrow tps tp

let t_apps tp tps = List.fold_left t_app tp tps

let rec kind tp =
  match view tp with
  | TUVar(_, u) -> UVar.kind u
  | TVar      x -> TVar.kind x
  | TEffect   _ -> KindBase.k_effect
  | TEffrow   _ -> KindBase.k_effrow
  | TPureArrow _ | TArrow _ | THandler _ | TLabel _ -> KindBase.k_type
  | TApp(tp, _) ->
    begin match KindBase.view (kind tp) with
    | KArrow(_, k) -> k
    | KType | KEffect | KEffrow | KUVar _ ->
      failwith "Internal kind error"
    end

let refresh_scheme sch =
  let (sub, tvars) = Subst.add_named_tvars Subst.empty sch.sch_targs in
  let ims =
    List.map
      (fun (name, isch) -> (Subst.in_name sub name, Subst.in_scheme sub isch))
      sch.sch_named
  in
  { sch_targs = tvars;
    sch_named = ims;
    sch_body  = Subst.in_type sub sch.sch_body
  }

let open_effrow_up ~scope eff =
  let (xs, ee) = effrow_view eff in
  match ee with
  | EEClosed ->
    t_effrow xs (EEUVar(TVar.Perm.id, UVar.fresh ~scope KindBase.k_effrow))
  | EEUVar _ | EEVar _ | EEApp _ -> eff

let rec open_down ~scope tp =
  match view tp with
  | TUVar _ | TVar _ | TLabel _ | TApp _ -> tp
  | TPureArrow(sch, tp2) ->
    t_pure_arrow (open_scheme_up ~scope sch) (open_down ~scope tp2)
  | TArrow(sch, tp2, eff) ->
    t_arrow (open_scheme_up ~scope sch) (open_down ~scope tp2) eff
  | THandler(a, tp, tp0, eff0) ->
    t_handler a (open_down ~scope:(Scope.add scope a) tp) tp0 eff0

  | TEffect _ | TEffrow _ ->
    failwith "Internal kind error"

and open_scheme_down ~scope sch =
  let sch = refresh_scheme sch in
  let scope = List.fold_left Scope.add_named scope sch.sch_targs in
  { sch_targs = sch.sch_targs;
    sch_named =
      List.map (fun (name, isch) -> (name, open_scheme_up ~scope isch))
        sch.sch_named;
    sch_body  = open_down ~scope sch.sch_body
  }

and open_up ~scope tp =
  match view tp with
  | TUVar _ | TVar _ | TLabel _ | TApp _ -> tp
  | TPureArrow(sch, tp2) ->
    t_pure_arrow (open_scheme_down ~scope sch) (open_up ~scope tp2)
  | TArrow(sch, tp2, eff) ->
    t_arrow
      (open_scheme_down ~scope sch)
      (open_up          ~scope tp2)
      (open_effrow_up   ~scope eff)
  | THandler(a, tp, tp0, eff0) ->
    t_handler a (open_up ~scope:(Scope.add scope a) tp) tp0 eff0

  | TEffect _ | TEffrow _ ->
    failwith "Internal kind error"

and open_scheme_up ~scope sch =
  let sch = refresh_scheme sch in
  let scope = List.fold_left Scope.add_named scope sch.sch_targs in
  { sch_targs = sch.sch_targs;
    sch_named =
      List.map (fun (name, isch) -> (name, open_scheme_down ~scope isch))
        sch.sch_named;
    sch_body   = open_up ~scope sch.sch_body
  }

let rec contains_uvar u tp =
  match view tp with
  | TVar _ | TEffect _ | TEffrow(_, (EEClosed | EEVar _)) -> false
  | TUVar(_, u') | TEffrow(_, EEUVar(_, u')) -> UVar.equal u u'
  | TPureArrow(sch, tp2) ->
    scheme_contains_uvar u sch || contains_uvar u tp2
  | TArrow(sch, tp2, eff) ->
    scheme_contains_uvar u sch || contains_uvar u tp2 || contains_uvar u eff
  | THandler(_, tp, tp0, eff0) ->
    contains_uvar u tp || contains_uvar u tp0 || contains_uvar u eff0
  | TLabel(eff, tp0, eff0) ->
    contains_uvar u eff || contains_uvar u tp0 || contains_uvar u eff0
  | TEffrow(_, EEApp(tp1, tp2)) | TApp(tp1, tp2) ->
    contains_uvar u tp1 || contains_uvar u tp2

and scheme_contains_uvar u sch =
  List.exists (fun (_, isch) -> scheme_contains_uvar u isch) sch.sch_named ||
  contains_uvar u sch.sch_body

let rec collect_uvars tp uvs =
  match view tp with
  | TVar _ | TEffect _ | TEffrow(_, (EEClosed | EEVar _)) -> uvs
  | TUVar(_, u) | TEffrow(_, EEUVar(_, u)) -> UVar.Set.add u uvs
  | TEffrow(_, EEApp(tp1, tp2)) | TApp(tp1, tp2) ->
    collect_uvars tp1 (collect_uvars tp2 uvs)
  | TPureArrow(sch, tp2) ->
    collect_scheme_uvars sch (collect_uvars tp2 uvs)
  | TArrow(sch, tp2, eff) ->
    collect_scheme_uvars sch (collect_uvars tp2 (collect_uvars eff uvs))
  | THandler(_, tp, tp0, eff0) ->
    collect_uvars tp (collect_uvars tp0 (collect_uvars eff0 uvs))
  | TLabel(eff, tp0, eff0) ->
    collect_uvars eff (collect_uvars tp0 (collect_uvars eff0 uvs))

and collect_scheme_uvars sch uvs =
  let uvs =
    List.fold_left
      (fun uvs (_, sch) -> collect_scheme_uvars sch uvs)
      uvs
      sch.sch_named
  in
  collect_uvars sch.sch_body uvs

let collect_ctor_uvars ctor uvs =
  uvs
  |> List.fold_right
      (fun (_, sch) -> collect_scheme_uvars sch)
      ctor.ctor_named
  |> List.fold_right collect_scheme_uvars ctor.ctor_arg_schemes

let uvars tp         = collect_uvars tp UVar.Set.empty
let scheme_uvars sch = collect_scheme_uvars sch UVar.Set.empty

(* ========================================================================= *)

exception Escapes_scope of tvar

let shrink_var_scope ~scope x =
  if Scope.mem scope x then ()
  else raise (Escapes_scope x)

let shrink_uvar_scope ~scope p u =
  UVar.filter_scope u (fun x -> Scope.mem scope (TVar.Perm.apply p x))

let rec shrink_effrow_end_scope ~scope ee =
  match ee with
  | EEClosed -> ()
  | EEUVar(p, u) -> shrink_uvar_scope ~scope p u
  | EEVar  x -> shrink_var_scope ~scope x
  | EEApp(tp1, tp2) ->
    shrink_scope ~scope tp1;
    shrink_scope ~scope tp2

and shrink_scope ~scope tp =
  match view tp with
  | TUVar(p, u) -> shrink_uvar_scope ~scope p u
  | TVar  x -> shrink_var_scope  ~scope x
  | TEffect xs ->
    TVar.Set.iter (shrink_var_scope ~scope) xs
  | TEffrow(xs, ee) ->
    TVar.Set.iter (shrink_var_scope ~scope) xs;
    shrink_effrow_end_scope ~scope ee
  | TPureArrow(sch, tp2) ->
    shrink_scheme_scope ~scope sch;
    shrink_scope ~scope tp2
  | TArrow(sch, tp2, eff) ->
    shrink_scheme_scope ~scope sch;
    shrink_scope ~scope tp2;
    shrink_scope ~scope eff
  | THandler(a, tp, tp0, eff0) ->
    let scope = Scope.add scope a in
    shrink_scope ~scope tp;
    shrink_scope ~scope tp0;
    shrink_scope ~scope eff0
  | TLabel(eff, tp0, eff0) ->
    shrink_scope ~scope eff;
    shrink_scope ~scope tp0;
    shrink_scope ~scope eff0
  | TApp(tp1, tp2) ->
    shrink_scope ~scope tp1;
    shrink_scope ~scope tp1

and shrink_scheme_scope ~scope sch =
  let scope = List.fold_left Scope.add_named scope sch.sch_targs in
  List.iter (fun (_, isch) -> shrink_scheme_scope ~scope isch)
    sch.sch_named;
  shrink_scope ~scope sch.sch_body

let try_shrink_scope ~scope tp =
  (* TODO: set backtracking point *)
  match shrink_scope ~scope tp with
  | () -> Ok ()
  | exception Escapes_scope x -> Error x

(* ========================================================================= *)

let mono_scheme tp =
  { sch_targs = [];
    sch_named = [];
    sch_body  = tp
  }

let scheme_is_monomorphic sch =
  match sch with
  | { sch_targs = []; sch_named = []; sch_body = _ } -> true
  | _ -> false
