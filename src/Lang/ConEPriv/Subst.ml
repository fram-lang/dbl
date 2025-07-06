(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Substitutions *)

open TypeBase

type t = typ TVar.Map.t

let empty = TVar.Map.empty

let is_empty sub = TVar.Map.is_empty sub

let add sub x tp = TVar.Map.add x tp sub

let rename sub x y = add sub x (t_var y)

let for_named_tvars xs tps =
  List.fold_left2 (fun sub (_, x) tp -> add sub x tp) empty xs tps

(* ========================================================================= *)

let open_tvar sub x =
  let y = TVar.clone ~scope:Scope.any x in
  (rename sub x y, y)

let open_named_tvar sub (name, x) =
  let (sub, x) = open_tvar sub x in
  (sub, (name, x))

let open_named_tvars sub xs =
  List.fold_left_map open_named_tvar sub xs

(* ========================================================================= *)

let in_effect_rec sub eff =
  let (tvs, eff) = Effct.take_tvars eff in
  List.fold_left
    (fun eff (x, p) ->
      match TVar.Map.find_opt x sub with
      | None    -> Effct.cons x p eff
      | Some tp -> Effct.join eff (Effct.guard (to_effect tp) p))
    eff tvs

let in_ceffect_rec sub eff =
  match eff with
  | Pure       -> Pure
  | Impure eff -> Impure (in_effect_rec sub eff)

let rec in_type_rec sub tp =
  match view tp with
  | TVar x ->
    begin match TVar.Map.find_opt x sub with
    | None    -> tp
    | Some tp -> tp
    end

  | TArrow(sch, tp, eff) ->
    t_arrow
      (in_scheme_rec sub sch)
      (in_type_rec sub tp)
      (in_ceffect_rec sub eff)

  | TLabel(eff, delim_tp, delim_eff) ->
    t_label
      (in_effect_rec sub eff)
      (in_type_rec   sub delim_tp)
      (in_effect_rec sub delim_eff)

  | THandler { tvar; cap_tp; in_tp; in_eff; out_tp; out_eff } ->
    let (sub_in, tvar) = open_tvar sub tvar in
    t_handler
      tvar
      (in_type_rec   sub_in cap_tp)
      (in_type_rec   sub_in in_tp)
      (in_effect_rec sub_in in_eff)
      (in_type_rec   sub    out_tp)
      (in_effect_rec sub    out_eff)

  | TEffect eff ->
    t_effect (in_effect_rec sub eff)

  | TApp(tp1, tp2) ->
    t_app (in_type_rec sub tp1) (in_type_rec sub tp2)

  | TAlias(uid, tp) ->
    t_alias uid (in_type_rec sub tp)

and in_scheme_rec sub sch =
  let (sub, targs) = open_named_tvars sub sch.sch_targs in
  { sch_targs = targs;
    sch_named = List.map (in_named_scheme_rec sub) sch.sch_named;
    sch_body  = in_type_rec sub sch.sch_body
  }

and in_named_scheme_rec sub (name, sch) =
  (name, in_scheme_rec sub sch)

(* ========================================================================= *)

let in_effect sub eff =
  if is_empty sub then eff
  else in_effect_rec sub eff

let in_type sub tp =
  if is_empty sub then tp
  else in_type_rec sub tp

let in_scheme sub sch =
  if is_empty sub then sch
  else in_scheme_rec sub sch

(* ========================================================================= *)

let in_ctor_decl sub ctor =
  if is_empty sub then ctor
  else
    let (sub, targs) = open_named_tvars sub ctor.ctor_targs in
    { ctor_name  = ctor.ctor_name;
      ctor_targs = targs;
      ctor_named = List.map (in_named_scheme_rec sub) ctor.ctor_named;
      ctor_arg_schemes = List.map (in_scheme_rec sub) ctor.ctor_arg_schemes
    }
