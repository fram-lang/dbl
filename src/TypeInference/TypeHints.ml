(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Extraction of type hints *)

open Common
open TypeCheckFix

type t = T.typ T.TVar.Map.t
type inst_cache = (T.name * (T.expr * T.typ * ret_effect)) list

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

(* ------------------------------------------------------------------------- *)
let extract_implicit_type_hints ~tcfix ~pos env (sch : T.scheme) inst eff =
  let open (val tcfix : TCFix) in
  let targs = sch.sch_targs in
  let inst =
    List.map (fun { T.data = (n, e); _ } -> (Name.tr_name env n, e)) inst in
  let rec loop hints cache named =
    match named with
    | [] -> (hints, cache)
    | (name, arg_sch) :: named
        when T.Scheme.is_monomorphic arg_sch
        && TypeUtils.is_tvar_neutral arg_sch.sch_body ->
      let expr_opt =
        match T.Name.assoc name inst with
        | None ->
          let make data = { S.pos = pos; S.data = data } in
          begin match name with
          | NLabel | NVar _ | NOptionalVar _ | NMethod _ -> None
          | NImplicit n ->
            (* type hints can be extracted only from monomorphic implicits
              (and explicit instantiation, of course), in order to avoid
              infinite loops: implicit parameters can depend on itself. *)
            begin match Env.lookup_implicit env (NPName n) with
            | Some(_, isch, _) when T.Scheme.is_monomorphic isch ->
              Some (make (S.EPoly(make (S.EImplicit (S.NPName n)), [], [])))
            | _ -> None
            end
          end
        | Some e -> Some e
      in
      begin match expr_opt with
      | None -> loop hints cache named
      | Some e ->
        let (e, tp, r_eff) = infer_expr_type env e eff in
        let hints2 = type_inst_hints targs arg_sch.sch_body tp in
        loop
          (merge_hints hints hints2)
          ((name, (e, tp, r_eff)) :: cache)
          named
      end
    | _ :: named ->
      loop hints cache named
  in
  loop T.TVar.Map.empty [] sch.sch_named
