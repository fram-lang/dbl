(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open TypeBase

type t = {
  perm : TVar.Perm.t;
    (** Permutation of type variables, applied before type substitution. *)

  sub  : typ TVar.Map.t
    (** Simultaneous substitution *)
}

let empty =
  { perm = TVar.Perm.id;
    sub  = TVar.Map.empty
  }

let rename_to_fresh sub x y =
  { sub with perm = TVar.Perm.swap_with_fresh_r sub.perm x y }

let add_tvar sub x =
  let y = TVar.clone x in
  (rename_to_fresh sub x y, y)

let add_named_tvar sub (n, x) =
  let (sub, x) = add_tvar sub x in
  (sub, (n, x))

let add_tvars sub xs =
  List.fold_left_map add_tvar sub xs

let add_named_tvars sub xs =
  List.fold_left_map add_named_tvar sub xs

let add_type sub x tp =
  { sub with sub = TVar.Map.add x tp sub.sub }

let is_empty sub =
  TVar.Perm.is_identity sub.perm && TVar.Map.is_empty sub.sub

(** Returns a new permutation attached to (modified in place) unification
  variable *)
let in_uvar sub p u =
  let p = TVar.Perm.compose sub.perm p in
  UVar.filter_scope u
    (fun x -> not (TVar.Map.mem (TVar.Perm.apply p x) sub.sub));
  p

let in_name sub n =
  match n with
  | NLabel | NVar _ | NImplicit _ -> n

(* Substitute in variable of kind [effect]. Since kind [effect] can contain
  only ground effects, substituted type is always (in some sense) a set of
  type variables. These type variables are added to a set [ys]. *)
let in_effvar sub x ys =
  let x = TVar.Perm.apply sub.perm x in
  match TVar.Map.find_opt x sub.sub with
  | None     -> TVar.Set.add x ys
  | Some eff -> TVar.Set.union (effect_view eff) ys

let rec in_effrow_end sub ee =
  match ee with
  | EEClosed -> (TVar.Set.empty, ee)
  | EEUVar(p, u) ->
    (TVar.Set.empty, EEUVar(in_uvar sub p u, u))
  | EEVar x  ->
    let x = TVar.Perm.apply sub.perm x in
    begin match TVar.Map.find_opt x sub.sub with
    | None     -> (TVar.Set.empty, EEVar x)
    | Some eff -> effrow_view eff
    end
  | EEApp(tp1, tp2) ->
    (TVar.Set.empty, EEApp(in_type_rec sub tp1, in_type_rec sub tp2))

and in_type_rec sub tp =
  match TypeBase.view tp with
  | TUnit -> tp
  | TUVar(p, u) ->
    let p = in_uvar sub p u in
    t_uvar p u
  | TVar x ->
    let x = TVar.Perm.apply sub.perm x in
    begin match TVar.Map.find_opt x sub.sub with
    | None    -> t_var x
    | Some tp -> tp
    end
  | TEffect xs ->
    t_effect (TVar.Set.fold (in_effvar sub) xs TVar.Set.empty)
  | TEffrow(xs, ee) ->
    let (ys, ee) = in_effrow_end sub ee in
    t_effrow (TVar.Set.fold (in_effvar sub) xs ys) ee
  | TPureArrow(sch, tp2) ->
    t_pure_arrow (in_scheme_rec sub sch) (in_type_rec sub tp2)
  | TArrow(sch, tp2, eff) ->
    t_arrow (in_scheme_rec sub sch) (in_type_rec sub tp2) (in_type_rec sub eff)
  | THandler(a, tp, tp0, eff0) ->
    let (sub, a) = add_tvar sub a in
    t_handler a (in_type_rec sub tp)
      (in_type_rec sub tp0) (in_type_rec sub eff0)
  | TLabel(eff, tp0, eff0) ->
    t_label (in_type_rec sub eff) (in_type_rec sub tp0) (in_type_rec sub eff0)
  | TApp(tp1, tp2) ->
    t_app (in_type_rec sub tp1) (in_type_rec sub tp2)

and in_scheme_rec sub sch =
  let (sub, tvars) = add_named_tvars sub sch.sch_targs in
  let named = List.map (in_named_scheme_rec sub) sch.sch_named in
  { sch_targs = tvars;
    sch_named = named;
    sch_body  = in_type_rec sub sch.sch_body
  }

and in_named_scheme_rec sub (n, sch) =
  (in_name sub n, in_scheme_rec sub sch)

let in_type sub tp =
  if is_empty sub then tp
  else in_type_rec sub tp

let in_scheme sub sch =
  if is_empty sub then sch
  else in_scheme_rec sub sch

let in_named_scheme sub nsch =
  if is_empty sub then nsch
  else in_named_scheme_rec sub nsch

let in_ctor_decl sub ctor =
  if is_empty sub then ctor
  else
    let (sub, tvs) = add_named_tvars sub ctor.ctor_targs in
    let named = List.map (in_named_scheme sub) ctor.ctor_named in
    { ctor_name        = ctor.ctor_name;
      ctor_targs       = tvs;
      ctor_named       = named;
      ctor_arg_schemes = List.map (in_scheme_rec sub) ctor.ctor_arg_schemes
  }
