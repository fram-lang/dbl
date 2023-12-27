(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for patterns *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

let check_scheme ~env ~scope (pat : S.pattern) sch =
  let make data = { pat with T.data = data } in
  match pat.data with
  | PWildcard ->
    let x = Var.fresh () in
    (env, make (T.PVar x))
  | PVar  x ->
    let (env, x) = Env.add_poly_var env x sch in
    (env, make (T.PVar x))
  | PName n ->
    let (env, x) = Env.add_poly_implicit env n sch ignore in
    (env, make (T.PVar x))

let check_type ~env ~scope (pat : S.pattern) tp =
  match pat.data with
  | PWildcard | PVar _ | PName _ ->
    let sch = {
      T.sch_tvars    = [];
      T.sch_implicit = [];
      T.sch_body     = tp
    } in
    check_scheme ~env ~scope (pat : S.pattern) sch
