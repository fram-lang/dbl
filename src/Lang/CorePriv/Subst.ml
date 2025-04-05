(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

open TypeBase

module Sub = TVar.Map.Make(struct type 'k t = 'k typ end)

type t = Sub.t

let empty = Sub.empty

let singleton = Sub.singleton

let is_empty = Sub.is_empty

let add sub x tp =
  Sub.add x tp sub

let add_tvar sub x =
  let y = TVar.clone x in
  (Sub.add x (TVar y) sub, y)

let add_tvar_ex sub (TVar.Ex x) =
  let (sub, x) = add_tvar sub x in
  (sub, TVar.Ex x)

let add_tvars sub xs =
  List.fold_left_map add_tvar_ex sub xs

let rec in_type_rec : type k. t -> k typ -> k typ =
  fun sub tp ->
  match tp with
  | TEffPure -> tp
  | TEffJoin(eff1, eff2) ->
    TEffJoin(in_type_rec sub eff1, in_type_rec sub eff2)
  | TVar x ->
    begin match Sub.find_opt x sub with
    | None    -> tp
    | Some tp -> tp
    end
  | TArrow(tp1, tp2, eff) ->
    TArrow(in_type_rec sub tp1, in_type_rec sub tp2, in_type_rec sub eff)
  | TForall(x, tp) ->
    let (sub, x) = add_tvar sub x in
    TForall(x, in_type_rec sub tp)
  | TGuard(cs, tp) ->
    TGuard(List.map (in_constr_rec sub) cs, in_type_rec sub tp)
  | TLabel lbl ->
    let effect = in_type_rec sub lbl.effect in
    let (sub, tvars) = add_tvars sub lbl.tvars in
    TLabel
      { effect; tvars;
        val_types = List.map (in_type_rec sub) lbl.val_types;
        delim_tp  = in_type_rec sub lbl.delim_tp;
        delim_eff = in_type_rec sub lbl.delim_eff
      }
  | TData(tp, eff, ctors) ->
    TData(in_type_rec sub tp, in_type_rec sub eff,
      List.map (in_ctor_type_rec sub) ctors)
  | TApp(tp1, tp2) ->
    TApp(in_type_rec sub tp1, in_type_rec sub tp2)

and in_constr_rec sub (eff1, eff2) =
  (in_type_rec sub eff1, in_type_rec sub eff2)

and in_ctor_type_rec sub ctor =
  let (sub, tvars) = add_tvars sub ctor.ctor_tvars in
  { ctor_name      = ctor.ctor_name;
    ctor_tvars     = tvars;
    ctor_arg_types = List.map (in_type_rec sub) ctor.ctor_arg_types
  }

let in_type sub tp =
  if is_empty sub then tp
  else in_type_rec sub tp
