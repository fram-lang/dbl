(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility function for extracting information about types *)

open Common

let method_owner_of_scheme ~pos ~env (sch : T.scheme) =
  let free_for_scheme x =
    not (List.exists (fun (_, y) -> T.TVar.equal x y) sch.sch_targs) in
  let self_sch =
    match T.Type.view sch.sch_body with
    | TArrow(sch, _, _) -> sch

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
  
  | Whnf_Arrow _ | Whnf_Handler _ | Whnf_Label _ ->
    Error.fatal (Error.method_of_invalid_type ~pos ~env sch self_tp)

  | Whnf_Effect ->
    failwith "Internal kind error"
