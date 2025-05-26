(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

open UnifCommon
open TypeBase

type t = {
  sub   : typ TVar.Map.t;
  scope : Scope.t
}

let empty ~scope =
  { sub = TVar.Map.empty; scope }

let add_tvar sub x =
  let y = TVar.clone ~scope:sub.scope x in
  { sub with sub = TVar.Map.add x (t_var y) sub.sub }, y

let add_named_tvar sub (n, x) =
  let (sub, x) = add_tvar sub x in
  (sub, (n, x))

let add_tvars sub xs =
  List.fold_left_map add_tvar sub xs

let add_named_tvars sub xs =
  List.fold_left_map add_named_tvar sub xs

let add_type sub x tp =
  { sub with sub = TVar.Map.add x tp sub.sub }

let rename_tvar sub x y =
  add_type sub x (t_var y)

let is_empty sub =
  TVar.Map.is_empty sub.sub

let enter_scope sub =
  { sub with scope = Scope.enter sub.scope }

let rec in_type_rec sub tp =
  match TypeBase.view tp with
  | TEffect -> tp
  | TUVar u ->
    (* We allow unification variables to have a scope that contains variables
      bound inside the type traversed by the substitution. However, each time
      we close a unification variable inside the binder we treat it in a
      way that causes the variable to have a scope that is outside the binder. 
      Therefore, during substitution we need to shrink the scope of the
      unification variable. *)
    UVar.shrink_scope u sub.scope;
    tp
  | TVar x ->
    begin match TVar.Map.find_opt x sub.sub with
    | Some tp -> tp
    | None ->
      assert (TVar.in_scope x sub.scope);
      tp
    end
  | TArrow(sch, tp2, eff) ->
    t_arrow (in_scheme_rec sub sch) (in_type_rec sub tp2) eff
  | THandler(a, tp, itp, otp) ->
    let otp = in_type_rec sub otp in
    let (sub, a) = add_tvar (enter_scope sub) a in
    t_handler a (in_type_rec sub tp) (in_type_rec sub itp) otp
  | TLabel tp0 ->
    t_label (in_type_rec sub tp0)
  | TApp(tp1, tp2) ->
    t_app (in_type_rec sub tp1) (in_type_rec sub tp2)

and in_scheme_rec sub sch =
  let (sub, tvars) = add_named_tvars (enter_scope sub) sch.sch_targs in
  { sch_targs = tvars;
    sch_named = List.map (in_named_scheme_rec sub) sch.sch_named;
    sch_body  = in_type_rec sub sch.sch_body
  }

and in_named_scheme_rec sub (n, sch) =
  (n, in_scheme_rec sub sch)

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
    let (sub, tvs) = add_named_tvars (enter_scope sub) ctor.ctor_targs in
    let named = List.map (in_named_scheme sub) ctor.ctor_named in
    { ctor_name        = ctor.ctor_name;
      ctor_targs       = tvs;
      ctor_named       = named;
      ctor_arg_schemes = List.map (in_scheme_rec sub) ctor.ctor_arg_schemes
    }
