(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions that help to build Unif expressions *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Make polymorphic function with given type parameters *)
let rec make_tfun tvs body =
  match tvs with
  | [] -> body
  | x :: tvs ->
    { T.pos  = body.T.pos
    ; T.data = T.ETFun(x, make_tfun tvs body)
    }

let rec make_tapp e tps =
  match tps with
  | [] -> e
  | tp :: tps ->
    let e =
      { T.pos  = e.T.pos
      ; T.data = T.ETApp(e, tp)
      }
    in
    make_tapp e tps

let generalize env e tp =
  let tvs =
    T.UVar.Set.diff (T.Type.uvars tp) (Env.uvars env)
    |> T.UVar.Set.elements
    |> List.map T.UVar.fix
  in
  let sch =
    { T.sch_tvars = tvs
    ; T.sch_body  = tp
    }
  in
  (make_tfun tvs e, sch)

let instantiate env e (sch : T.scheme) =
  let guess_type sub tv =
    let tp = Env.fresh_uvar env (T.TVar.kind tv) in
    (T.Subst.add_type sub tv tp, tp)
  in
  let (sub, tps) =
    List.fold_left_map guess_type T.Subst.empty sch.sch_tvars in
  (make_tapp e tps, T.Type.subst sub sch.sch_body)
