(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Basic operations on types *)

open UnifCommon
open TypeBase

let view = TypeBase.view

let fresh_uvar ~pos ~scope kind = t_uvar (UVar.fresh ~pos ~scope kind)

let t_unit = t_var BuiltinType.tv_unit

let t_bool = t_var BuiltinType.tv_bool

let t_option tp = t_app (t_var BuiltinType.tv_option) tp

let t_pure_arrow sch tp = t_arrow sch tp Pure

let t_pure_arrows schs tp = List.fold_right t_pure_arrow schs tp

let t_apps tp tps = List.fold_left t_app tp tps

let rec kind tp =
  match view tp with
  | TEffect -> Kind.k_effect
  | TUVar u -> UVar.kind u
  | TVar  x -> TVar.kind x
  | TArrow _ | THandler _ | TLabel _ -> Kind.k_type
  | TApp(tp, _) ->
    begin match Kind.view (kind tp) with
    | KArrow(_, k) -> k
    | KType | KEffect | KUVar _ ->
      failwith "Internal kind error"
    end
  | TAlias(_, tp) -> kind tp

let rec contains_uvar u tp =
  match view tp with
  | TEffect | TVar _ -> false
  | TUVar u' -> UVar.equal u u'
  | TArrow(sch, tp2, _) ->
    scheme_contains_uvar u sch || contains_uvar u tp2
  | THandler(_, tp, itp, otp) ->
    contains_uvar u tp || contains_uvar u itp || contains_uvar u otp
  | TLabel tp0 ->
    contains_uvar u tp0
  | TApp(tp1, tp2) ->
    contains_uvar u tp1 || contains_uvar u tp2
  | TAlias(_, tp) ->
    contains_uvar u tp

and scheme_contains_uvar u sch =
  List.exists (fun (_, isch) -> scheme_contains_uvar u isch) sch.sch_named ||
  contains_uvar u sch.sch_body

let rec collect_uvars tp uvs =
  match view tp with
  | TEffect | TVar _ -> uvs
  | TUVar u -> UVar.Set.add u uvs
  | TArrow(sch, tp2, _) ->
    collect_scheme_uvars sch (collect_uvars tp2 uvs)
  | THandler(_, tp, itp, otp) ->
    uvs
    |> collect_uvars tp
    |> collect_uvars itp
    |> collect_uvars otp
  | TLabel tp0 -> collect_uvars tp0 uvs
  | TApp(tp1, tp2) ->
    collect_uvars tp1 (collect_uvars tp2 uvs)
  | TAlias(_, tp) -> collect_uvars tp uvs

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

let shrink_var_scope ~tvars ~scope x =
  if TVar.in_scope x scope then ()
  else if TVar.Set.mem x tvars then ()
  else raise (Escapes_scope x)

let add_named_tvars ~tvars tvs =
  List.fold_left (fun tvars (_, x) -> TVar.Set.add x tvars) tvars tvs

(** Shrink scopes of unification variables in given type and check if all
  type variables fit in given scope. The [tvars] set contains all type
  variables that are bound in the type, so they may appear even if they are
  outside of the scope. *)
let rec shrink_scope_rec ~tvars ~scope tp =
  match view tp with
  | TEffect -> tp
  | TUVar u ->
    UVar.shrink_scope u scope;
    tp
  | TVar  x ->
    shrink_var_scope ~tvars ~scope x;
    tp
  | TArrow(sch, tp2, eff) ->
    t_arrow
      (shrink_scheme_scope_rec ~tvars ~scope sch)
      (shrink_scope_rec ~tvars ~scope tp2)
      eff
  | THandler(a, tp, itp, otp) ->
    let otp = shrink_scope_rec ~tvars ~scope otp in
    let tvars = TVar.Set.add a tvars in
    t_handler
      a
      (shrink_scope_rec ~tvars ~scope tp)
      (shrink_scope_rec ~tvars ~scope itp)
      otp
  | TLabel tp0 ->
    t_label (shrink_scope_rec ~tvars ~scope tp0)
  | TApp(tp1, tp2) ->
    t_app
      (shrink_scope_rec ~tvars ~scope tp1)
      (shrink_scope_rec ~tvars ~scope tp2)
  | TAlias(a, tp) ->
    if TyAlias.in_scope a scope then
      t_alias a (shrink_scope_rec ~tvars ~scope tp)
    else
      shrink_scope_rec ~tvars ~scope tp

and shrink_scheme_scope_rec ~tvars ~scope sch =
  let tvars = add_named_tvars ~tvars sch.sch_targs in
  { sch_targs = sch.sch_targs;
    sch_named =
      List.map
        (fun (name, sch) ->
          (name, shrink_scheme_scope_rec ~tvars ~scope sch))
        sch.sch_named;
    sch_body = shrink_scope_rec ~tvars ~scope sch.sch_body
  }

let shrink_scope ~scope tp =
  match shrink_scope_rec ~tvars:TVar.Set.empty ~scope tp with
  | tp -> Ok tp
  | exception Escapes_scope x -> Error x

(* ========================================================================= *)

let ctor_is_positive ~scope ~args ~nonrec_scope ctor =
  (** Check if the scope of unification variable [u] fits in [nonrec_scope],
    assuming that the unification variable fits in [scope]. *)
  let uvar_fits_in_scope u =
    Scope.mem (Scope.inter (UVar.scope u) scope) nonrec_scope
  in

  (** Check if type variable [x] fits in [nonrec_scope] extended with [tvars],
    assuming that the type variable fits in [scope] extended with [tvars]. *)
  let tvar_fits_in_scope ~tvars x =
    Scope.mem (Scope.inter (TVar.scope x) scope) nonrec_scope ||
    TVar.Set.mem x tvars
  in

  (** Check if all type variables and all scopes of unification variables fit
    in [nonrec_scope] extended with [tvars] assuming that the type fits [scope]
    extended with [tvars]. *)
  let rec fits_in_scope ~tvars tp =
    match view tp with
    | TEffect -> true
    | TUVar u -> uvar_fits_in_scope u
    | TVar  x -> tvar_fits_in_scope ~tvars x
    | TArrow(sch, tp, _) ->
      scheme_fits_in_scope ~tvars sch && fits_in_scope ~tvars tp
    | THandler(x, tp, itp, otp) ->
      fits_in_scope ~tvars otp &&
      begin
        let tvars = TVar.Set.add x tvars in
        fits_in_scope ~tvars tp &&
        fits_in_scope ~tvars itp
      end
    | TLabel tp0 -> fits_in_scope ~tvars tp0
    | TApp(tp1, tp2) ->
      fits_in_scope ~tvars tp1 && fits_in_scope ~tvars tp2
    | TAlias(_, tp) -> fits_in_scope ~tvars tp

  and scheme_fits_in_scope ~tvars sch =
    let tvars = add_named_tvars ~tvars sch.sch_targs in
    List.for_all (named_scheme_fits_in_scope ~tvars) sch.sch_named &&
    fits_in_scope ~tvars sch.sch_body

  and named_scheme_fits_in_scope ~tvars (_, sch) =
    scheme_fits_in_scope ~tvars sch
  in

  (** Check if all type variables on non positive (or non negative, when [pol]
    flag is set to [false]) positions and all scopes of unification variables
    fit in given scope. *)
  let rec is_positive ~pol ~tvars tp =
    match view tp with
    | TEffect -> true
    | TVar  x ->
      if pol then true
      else fits_in_scope ~tvars tp
    | TUVar u -> fits_in_scope ~tvars tp
    | TArrow(sch, tp, _) ->
      scheme_is_negative ~pol ~tvars sch && is_positive ~pol ~tvars tp
    | THandler(x, tp, itp, otp) ->
      is_positive ~pol ~tvars otp &&
      begin
        let tvars = TVar.Set.add x tvars in
        is_positive ~pol ~tvars tp &&
        is_negative ~pol ~tvars itp
      end
    | TLabel _ ->
      fits_in_scope ~tvars tp
    | TApp(tp1, tp2) ->
      is_positive ~pol ~tvars tp1 && fits_in_scope ~tvars tp2
    | TAlias(_, tp) ->
      is_positive ~pol ~tvars tp

  and scheme_is_positive ~pol ~tvars sch =
    let tvars = add_named_tvars ~tvars sch.sch_targs in
    List.for_all (named_scheme_is_negative ~pol ~tvars) sch.sch_named &&
    is_positive ~pol ~tvars sch.sch_body

  and named_scheme_is_positive ~pol ~tvars (_, sch) =
    scheme_is_positive ~pol ~tvars sch

  and is_negative ~pol ~tvars tp =
    is_positive ~pol:(not pol) ~tvars tp

  and scheme_is_negative ~pol ~tvars sch =
    scheme_is_positive ~pol:(not pol) ~tvars sch

  and named_scheme_is_negative ~pol ~tvars (_, sch) =
    scheme_is_negative ~pol ~tvars sch
  in

  let tvars = add_named_tvars ~tvars:TVar.Set.empty args in
  let tvars = add_named_tvars ~tvars ctor.ctor_targs in
  List.for_all (named_scheme_is_positive ~pol:true ~tvars) ctor.ctor_named &&
  List.for_all (scheme_is_positive ~pol:true ~tvars) ctor.ctor_arg_schemes

(* ========================================================================= *)

let mono_scheme tp =
  { sch_targs = [];
    sch_named = [];
    sch_body  = tp
  }

let scheme_to_type sch =
  match sch with
  | { sch_targs = []; sch_named = []; sch_body } -> Some sch_body
  | _ -> None

let scheme_is_monomorphic sch =
  match scheme_to_type sch with
  | Some _ -> true
  | None   -> false
