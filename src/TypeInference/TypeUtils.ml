(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility function for extracting information about types *)

open Common

type hints = T.typ T.TVar.Map.t

let is_tvar_neutral tp =
  match T.Type.whnf tp with
  | Whnf_Neutral(NH_Var _, _) -> true
  | _ -> false

let method_owner_of_scheme ~pos ~env (sch : T.scheme) =
  let free_for_scheme x =
    not (List.exists (fun (_, y) -> T.TVar.equal x y) sch.sch_targs) in
  let self_sch =
    match T.Type.view sch.sch_body with
    | TArrow(sch, _, _) | TPureArrow(sch, _) -> sch

    | _ ->
      Error.fatal (Error.non_arrow_method ~pos ~env sch)
  in
  let { T.sch_targs; sch_named; sch_body = self_tp } = self_sch in
  if not (List.is_empty sch_targs && List.is_empty sch_named) then
    Error.fatal (Error.method_of_polymorphic_type ~pos ~env sch);
  match T.Type.whnf self_tp with
  | Whnf_Neutral(NH_Var owner, _) ->
    if free_for_scheme owner then owner
    else
      Error.fatal (Error.method_of_bound_tvar ~pos ~env sch)
  | Whnf_Neutral(NH_UVar _, _) ->
    Error.fatal (Error.method_of_unknown_type ~pos ~env sch)
  
  | Whnf_PureArrow _ | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
    Error.fatal (Error.method_of_invalid_type ~pos ~env sch self_tp)

  | Whnf_Effect _ | Whnf_Effrow _ ->
    failwith "Internal kind error"

let open_scheme ~pos (env : Env.t) (sch : T.scheme) =
  let (env, sch) = Env.extend_scope env sch in
  let (env, ims) =
    List.fold_left_map
      (fun env (name, sch) ->
        let (env, x) =
          match name with
          | T.NLabel -> Env.add_the_label_sch env sch
          | T.NVar x  -> Env.add_poly_var env x sch
          | T.NOptionalVar x -> 
            let { T.sch_targs; sch_named; sch_body } = sch in
            Env.add_mono_var env x sch_body
          | T.NImplicit n -> Env.add_poly_implicit env n sch ignore
          | T.NMethod   n ->
            let owner = method_owner_of_scheme ~pos ~env sch in
            Env.add_poly_method env owner n sch
        in
        (env, (name, x, sch)))
      env
      sch.sch_named in
  (env, sch.sch_targs, ims, sch.sch_body)
