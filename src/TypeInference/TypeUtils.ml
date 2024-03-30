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

let merge_hints hints1 hints2 =
  T.TVar.Map.merge
    (fun _ h1 h2 ->
      match h1 with
      | Some tp -> Some tp
      | None    -> h2)
    hints1
    hints2

let update_hints hints s_tp tp =
  match T.Type.view s_tp with
  | TVar x when T.TVar.Map.mem x hints ->
    T.TVar.Map.add x (Some tp) hints
  | _ -> hints

let method_inst_hints (sch : T.scheme) self_tp =
  (* initial map, with all type variables bound by the scheme *)
  let hints =
    sch.sch_targs
    |> List.map (fun (_, x) -> (x, None))
    |> List.to_seq |> T.TVar.Map.of_seq
  in
  let self_sch_s =
    match T.Type.view sch.sch_body with
    | TArrow(sch, _, _) | TPureArrow(sch, _) -> sch

    | _ -> failwith "Internal error: invalid method scheme"
  in
  assert (List.is_empty self_sch_s.sch_targs);
  assert (List.is_empty self_sch_s.sch_named);
  match T.Type.whnf self_sch_s.sch_body, T.Type.whnf self_tp with
  | Whnf_Neutral(NH_Var a1, s_targs), Whnf_Neutral(NH_Var a2, args) ->
    assert (T.TVar.equal a1 a2);
    assert (List.length s_targs = List.length args);
    List.fold_left2 update_hints hints s_targs args
    |> T.TVar.Map.filter_map (fun _ x -> x)

  | _ -> failwith "Internal error: invalid method scheme"

let type_inst_hints sch_targs req_tp act_tp =
  match T.Type.whnf req_tp, T.Type.whnf act_tp with
  | Whnf_Neutral(NH_Var a1, args1), Whnf_Neutral(NH_Var a2, args2)
      when T.TVar.equal a1 a2 ->
    assert (List.length args1 = List.length args2);
    (* initial map, with all type variables bound by the scheme *)
    let hints =
      sch_targs
      |> List.map (fun (_, x) -> (x, None))
      |> List.to_seq |> T.TVar.Map.of_seq
    in
    List.fold_left2 update_hints hints args1 args2
    |> T.TVar.Map.filter_map (fun _ x -> x)

  | _ -> T.TVar.Map.empty
