(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Basic operations on types *)

open TypeBase

let view = TypeBase.view

let fresh_uvar ~scope kind = t_uvar TVar.Perm.id (UVar.fresh ~scope kind)

let t_unit = t_var BuiltinType.tv_unit

let t_option tp = t_app (t_var BuiltinType.tv_option) tp

let t_pure_arrow sch tp = t_arrow sch tp Pure

let t_pure_arrows schs tp = List.fold_right t_pure_arrow schs tp

let t_apps tp tps = List.fold_left t_app tp tps

let rec kind tp =
  match view tp with
  | TEffect     -> KindBase.k_effect
  | TUVar(_, u) -> UVar.kind u
  | TVar      x -> TVar.kind x
  | TArrow _ | THandler _ | TLabel _ -> KindBase.k_type
  | TApp(tp, _) ->
    begin match KindBase.view (kind tp) with
    | KArrow(_, k) -> k
    | KType | KEffect | KUVar _ ->
      failwith "Internal kind error"
    end

let refresh_scheme sch =
  let (sub, tvars) = Subst.add_named_tvars Subst.empty sch.sch_targs in
  { sch_targs = tvars;
    sch_named = List.map (Subst.in_named_scheme sub) sch.sch_named;
    sch_body  = Subst.in_type sub sch.sch_body
  }

let rec contains_uvar u tp =
  match view tp with
  | TEffect | TVar _ -> false
  | TUVar(_, u') -> UVar.equal u u'
  | TArrow(sch, tp2, _) ->
    scheme_contains_uvar u sch || contains_uvar u tp2
  | THandler(_, tp, itp, otp) ->
    contains_uvar u tp || contains_uvar u itp || contains_uvar u otp
  | TLabel tp0 ->
    contains_uvar u tp0
  | TApp(tp1, tp2) ->
    contains_uvar u tp1 || contains_uvar u tp2

and scheme_contains_uvar u sch =
  List.exists (fun (_, isch) -> scheme_contains_uvar u isch) sch.sch_named ||
  contains_uvar u sch.sch_body

let rec collect_uvars tp uvs =
  match view tp with
  | TEffect | TVar _ -> uvs
  | TUVar(_, u) -> UVar.Set.add u uvs
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

let rec shrink_scope ~scope tp =
  match view tp with
  | TEffect -> ()
  | TUVar(p, u) -> shrink_uvar_scope ~scope p u
  | TVar  x -> shrink_var_scope  ~scope x
  | TArrow(sch, tp2, _) ->
    shrink_scheme_scope ~scope sch;
    shrink_scope ~scope tp2
  | THandler(a, tp, itp, otp) ->
    shrink_scope ~scope otp;
    let scope = Scope.add scope a in
    shrink_scope ~scope tp;
    shrink_scope ~scope itp
  | TLabel tp0 -> shrink_scope ~scope tp0
  | TApp(tp1, tp2) ->
    shrink_scope ~scope tp1;
    shrink_scope ~scope tp2

and shrink_scheme_scope ~scope sch =
  let scope = List.fold_left Scope.add_named scope sch.sch_targs in
  List.iter (fun (_, isch) -> shrink_scheme_scope ~scope isch)
    sch.sch_named;
  shrink_scope ~scope sch.sch_body

let try_shrink_scope ~scope tp =
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
  | TEffect     -> true
  | TUVar(p, u) -> uvar_fits_in_scope ~scope p u
  | TVar    x   -> Scope.mem scope x
  | TArrow(sch, tp, _) ->
    scheme_fits_in_scope ~scope sch &&
    fits_in_scope ~scope tp
  | THandler(x, tp, itp, otp) ->
    fits_in_scope ~scope otp &&
    begin
      let scope = Scope.add scope x in
      fits_in_scope ~scope tp &&
      fits_in_scope ~scope itp
    end
  | TLabel tp0 -> fits_in_scope ~scope tp0
  | TApp(tp1, tp2) ->
    fits_in_scope ~scope tp1 && fits_in_scope ~scope tp2

and scheme_fits_in_scope ~scope sch =
  let scope = List.fold_left Scope.add_named scope sch.sch_targs in
  List.for_all (named_scheme_fits_in_scope ~scope) sch.sch_named &&
  fits_in_scope ~scope sch.sch_body

and named_scheme_fits_in_scope ~scope (_, sch) =
  scheme_fits_in_scope ~scope sch

(** Check if all type variables on non-strictly positive positions and all
  scopes of unification variables fit in given scope. This function ignores
  effects, because effect-handling always has NTerm effect. *)
let rec strictly_positive ~nonrec_scope tp =
  match view tp with
  | TEffect | TVar _ -> true
  | TUVar(p, u) ->
    uvar_fits_in_scope ~scope:nonrec_scope p u
  | TArrow(sch, tp, _) ->
    scheme_fits_in_scope ~scope:nonrec_scope sch &&
    strictly_positive ~nonrec_scope tp
  | THandler(x, tp, itp, otp) ->
    strictly_positive ~nonrec_scope otp &&
    begin
      let scope = Scope.add nonrec_scope x in
      (* Type [tp] is on positive position, but in encoding of handler
        types in Core it is not strictly positive. *)
      fits_in_scope ~scope tp &&
      fits_in_scope ~scope itp
    end
  | TLabel _ ->
    fits_in_scope ~scope:nonrec_scope tp
  | TApp(tp1, tp2) ->
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

let scheme_to_type sch =
  match sch with
  | { sch_targs = []; sch_named = []; sch_body } -> Some sch_body
  | _ -> None

let scheme_is_monomorphic sch =
  match scheme_to_type sch with
  | Some _ -> true
  | None   -> false
