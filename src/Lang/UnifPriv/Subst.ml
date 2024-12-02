(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type substitutions *)

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
    (UVar.level u)
    (fun x -> not (TVar.Map.mem (TVar.Perm.apply p x) sub.sub));
  p

let rec in_type_rec sub tp =
  match TypeBase.view tp with
  | TEffect -> tp
  | TUVar(p, u) ->
    let p = in_uvar sub p u in
    t_uvar p u
  | TVar x ->
    let x = TVar.Perm.apply sub.perm x in
    begin match TVar.Map.find_opt x sub.sub with
    | None    -> t_var x
    | Some tp -> tp
    end
  | TArrow(sch, tp2, eff) ->
    t_arrow (in_scheme_rec sub sch) (in_type_rec sub tp2) eff
  | THandler(a, tp, itp, otp) ->
    let otp  = in_type_rec sub otp in
    let (sub, a) = add_tvar sub a in
    t_handler a (in_type_rec sub tp) (in_type_rec sub itp) otp
  | TLabel tp0 ->
    t_label (in_type_rec sub tp0)
  | TApp(tp1, tp2) ->
    t_app (in_type_rec sub tp1) (in_type_rec sub tp2)

and in_scheme_rec sub sch =
  let (sub, tvars) = add_named_tvars sub sch.sch_targs in
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
    let (sub, tvs) = add_named_tvars sub ctor.ctor_targs in
    let named = List.map (in_named_scheme sub) ctor.ctor_named in
    { ctor_name        = ctor.ctor_name;
      ctor_targs       = tvs;
      ctor_named       = named;
      ctor_arg_schemes = List.map (in_scheme_rec sub) ctor.ctor_arg_schemes
  }
