(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Kind-checking and translation of type expressions *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

let rec infer_kind env (tp : S.type_expr) =
  match tp.data with
  | TWildcard ->
    let k = T.Kind.fresh_uvar () in
    (Env.fresh_uvar env k, k)

  | TVar x ->
    begin match Env.lookup_tvar env x with
    | Some x -> (T.Type.t_var x, T.TVar.kind x)
    | None ->
      Error.report (Error.unbound_type_var ~pos:tp.pos x);
      let k = T.Kind.fresh_uvar () in
      (Env.fresh_uvar env k, k)
    end

  | TPureArrow(tp1, tp2) ->
    let tp1 = check_kind env tp1 T.Kind.k_type in
    let tp2 = check_kind env tp2 T.Kind.k_type in
    (T.Type.t_pure_arrow tp1 tp2, T.Kind.k_type)

  | TArrow(tp1, tp2, eff) ->
    let tp1 = check_kind env tp1 T.Kind.k_type in
    let tp2 = check_kind env tp2 T.Kind.k_type in
    let eff = check_kind env eff T.Kind.k_effect in
    (T.Type.t_arrow tp1 tp2 eff, T.Kind.k_type)

  | TEffect(tps, ee) ->
    (check_effect env tps ee, T.Kind.k_effect)

and check_kind env (tp : S.type_expr) k =
  match tp.data with
  | TVar _ | TPureArrow _ | TArrow _ ->
    check_kind_default env tp k

  | TEffect(tps, ee) ->
    let tp' = check_effect env tps ee in
    begin match T.Kind.view k with
    | KEffect   -> tp'
    | KUVar u ->
      T.KUVar.set u T.Kind.k_effect;
      tp'
    | KClEffect ->
      begin match T.Effect.view_end tp' with
      | EEClosed -> tp'
      | EEVar _ | EEUVar _ ->
        Error.fatal (Error.kind_mismatch ~pos:tp.pos T.Kind.k_effect k)
      end
    | KType ->
      Error.report (Error.kind_mismatch ~pos:tp.pos T.Kind.k_effect k);
      Env.fresh_uvar env k
    end

  | TWildcard -> Env.fresh_uvar env k

and check_kind_default env tp k =
  let (tp', k') = infer_kind env tp in
  if Unification.unify_kind k k' then
    tp'
  else begin
    Error.report (Error.kind_mismatch ~pos:tp.pos k' k);
    Env.fresh_uvar env k
  end

and check_effect env tps ee =
  let tvs = List.fold_left (check_cl_effect_it env) T.TVar.Set.empty tps in
  match ee with
  | None    -> T.Type.t_closed_effect tvs
  | Some ee ->
    let (tvs', ee) = T.Type.effect_view (check_kind env ee T.Kind.k_effect) in
    T.Type.t_effect (T.TVar.Set.union tvs tvs') ee

and check_cl_effect_it env tvs tp =
  match T.Type.effect_view (check_kind env tp T.Kind.k_cleffect) with
  | (tvs', EEClosed) -> T.TVar.Set.union tvs tvs'
  | (tvs', (EEVar _ | EEUVar _)) ->
    Error.report
      (Error.kind_mismatch ~pos:tp.pos T.Kind.k_effect T.Kind.k_cleffect);
    T.TVar.Set.union tvs tvs'

let tr_ttype env tp =
  check_kind env tp T.Kind.k_type
