(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Basic operations on types *)

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
  | THandler(a, tp, itp, ieff, otp, oeff) ->
    let otp  = open_down ~scope otp in
    let scope = Scope.add scope a in
    t_handler a
      (open_down ~scope tp)
      (open_up   ~scope itp)
      (open_effrow_up ~scope ieff)
      otp
      oeff

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
  | THandler(a, tp, itp, ieff, otp, oeff) ->
    let otp  = open_up ~scope otp in
    let oeff = open_effrow_up ~scope oeff in
    let scope = Scope.add scope a in
    t_handler a (open_up ~scope tp) (open_down ~scope itp) ieff otp oeff

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
  | THandler(_, tp, itp, ieff, otp, oeff) ->
    contains_uvar u tp ||
    contains_uvar u itp || contains_uvar u ieff ||
    contains_uvar u otp || contains_uvar u oeff
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
  | THandler(_, tp, itp, ieff, otp, oeff) ->
    uvs
    |> collect_uvars tp
    |> collect_uvars itp
    |> collect_uvars ieff
    |> collect_uvars otp
    |> collect_uvars oeff
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
  UVar.filter_scope u (Scope.level scope)
    (fun x -> Scope.mem scope (TVar.Perm.apply p x))

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
  | THandler(a, tp, itp, ieff, otp, oeff) ->
    shrink_scope ~scope otp;
    shrink_scope ~scope oeff;
    let scope = Scope.add scope a in
    shrink_scope ~scope tp;
    shrink_scope ~scope itp;
    shrink_scope ~scope ieff
  | TLabel(eff, tp0, eff0) ->
    shrink_scope ~scope eff;
    shrink_scope ~scope tp0;
    shrink_scope ~scope eff0
  | TApp(tp1, tp2) ->
    shrink_scope ~scope tp1;
    shrink_scope ~scope tp2

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

let uvar_fits_in_scope ~scope p u =
  Scope.for_all
    (fun x -> Scope.mem scope (TVar.Perm.apply p x))
    (UVar.scope u)

(** Check if all type variables and all scopes of unification variables fit in
  given scope *)
let rec fits_in_scope ~scope tp =
  match view tp with
  | TUVar(p, u) -> uvar_fits_in_scope ~scope p u
  | TVar    x   -> Scope.mem scope x
  | TEffect xs  ->
    TVar.Set.for_all (Scope.mem scope) xs
  | TEffrow(xs, ee) ->
    TVar.Set.for_all (Scope.mem scope) xs &&
    begin match ee with
    | EEClosed -> true
    | EEUVar(p, u) -> uvar_fits_in_scope ~scope p u
    | EEVar x -> Scope.mem scope x
    | EEApp(tp1, tp2) ->
      fits_in_scope ~scope tp1 && fits_in_scope ~scope tp2
    end
  | TPureArrow(sch, tp) ->
    scheme_fits_in_scope ~scope sch &&
    fits_in_scope ~scope tp
  | TArrow(sch, tp, eff) ->
    scheme_fits_in_scope ~scope sch &&
    fits_in_scope ~scope tp &&
    fits_in_scope ~scope eff
  | THandler(x, tp, itp, ieff, otp, oeff) ->
    fits_in_scope ~scope otp &&
    fits_in_scope ~scope oeff &&
    begin
      let scope = Scope.add scope x in
      fits_in_scope ~scope tp &&
      fits_in_scope ~scope itp &&
      fits_in_scope ~scope otp
    end
  | TLabel(eff0, tp, eff) ->
    fits_in_scope ~scope eff0 &&
    fits_in_scope ~scope tp &&
    fits_in_scope ~scope eff
  | TApp(tp1, tp2) ->
    fits_in_scope ~scope tp1 && fits_in_scope ~scope tp2

and scheme_fits_in_scope ~scope sch =
  let scope = List.fold_left Scope.add_named scope sch.sch_targs in
  List.for_all (named_scheme_fits_in_scope ~scope) sch.sch_named &&
  fits_in_scope ~scope sch.sch_body

and named_scheme_fits_in_scope ~scope (_, sch) =
  scheme_fits_in_scope ~scope sch

(** Check if all type variables on non-strictly positive positions and all
  scopes of unification variables fit in given scope *)
let rec strictly_positive ~nonrec_scope tp =
  match view tp with
  | TUVar(p, u) | TEffrow(_, EEUVar(p, u)) ->
    uvar_fits_in_scope ~scope:nonrec_scope p u
  | TVar _ | TEffect _ | TEffrow(_, (EEClosed | EEVar _)) ->
    true
  | TPureArrow(sch, tp) ->
    scheme_fits_in_scope ~scope:nonrec_scope sch &&
    strictly_positive ~nonrec_scope tp
  | TArrow(sch, tp, eff) ->
    scheme_fits_in_scope ~scope:nonrec_scope sch &&
    strictly_positive ~nonrec_scope tp &&
    strictly_positive ~nonrec_scope eff
  | THandler(x, tp, itp, ieff, otp, oeff) ->
    strictly_positive ~nonrec_scope otp &&
    strictly_positive ~nonrec_scope oeff &&
    begin
      let scope = Scope.add nonrec_scope x in
      (* Type [tp] is on positive position, but in encoding of handler
        types in Core it is not strictly positive. *)
      fits_in_scope ~scope tp &&
      fits_in_scope ~scope itp &&
      fits_in_scope ~scope ieff
    end
  | TLabel _ ->
    fits_in_scope ~scope:nonrec_scope tp
  | TApp(tp1, tp2) | TEffrow(_, EEApp(tp1, tp2)) ->
    strictly_positive ~nonrec_scope tp1 &&
    fits_in_scope ~scope:nonrec_scope tp2

let scheme_strictly_positive ~nonrec_scope sch =
  let nonrec_scope =
    List.fold_left Scope.add_named nonrec_scope sch.sch_targs in
  List.for_all (named_scheme_fits_in_scope ~scope:nonrec_scope)
    sch.sch_named &&
  strictly_positive ~nonrec_scope sch.sch_body

let named_scheme_strictly_positive ~nonrec_scope (_, sch) =
  scheme_strictly_positive ~nonrec_scope sch

let ctor_strictly_positive ~nonrec_scope ctor =
  let nonrec_scope =
    List.fold_left Scope.add_named nonrec_scope ctor.ctor_targs in
  List.for_all (named_scheme_strictly_positive ~nonrec_scope)
    ctor.ctor_named &&
  List.for_all (scheme_strictly_positive ~nonrec_scope)
    ctor.ctor_arg_schemes

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
