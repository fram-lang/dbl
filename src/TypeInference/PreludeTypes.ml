(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Functions allowing the use of some of the types defined in the Prelude. *)

open Common

let mk_Option ~env tp_arg =
  begin match Env.lookup_tvar env (S.NPName "Option") with
  | None    -> failwith "Couldn't find Option type"
  | Some tp -> 
    if not (Unification.unify_kind 
      (T.Type.kind tp)
      (T.Kind.k_arrow T.Kind.k_type T.Kind.k_type))
      then failwith "Option is not of Kind k_arrow k_tp => k_tp";
    (* We can do more checks on the ADT to make sure it's correct Option type (eg. it contains 2 constructors etc.) *)
    (* begin match T.Type.view tp with
    | T.Type.TVar x -> 
      begin match Env.lookup_adt env x with
      | Some adt_info -> failwith "Not Implemented"
      | None -> failwith "Error"
      end
    | _ -> failwith "Error"
    end *)
    T.Type.t_app tp tp_arg
  end

let extr_arg_tp option_tp =
  match T.Type.whnf option_tp with
  | Whnf_Neutral(NH_Var x, tp :: []) -> tp
  | _ -> failwith "Option's whnf form is incorrect"

let mk_Some ~env tp_arg expr_arg : T.expr =
  let make data pos = { T.data; T.pos } in
  let tp' = mk_Option ~env tp_arg in
  let _ = extr_arg_tp tp' in
  begin match Env.lookup_ctor env (S.NPName "Some") with
  | Some (idx, adt_info) -> 
    let {Module.adt_proof; adt_args; adt_ctors; adt_type} = adt_info in
    make (T.ECtor (make (T.ETApp (adt_proof, tp_arg)) Position.nowhere, idx, [], [expr_arg])) Position.nowhere
  | None -> failwith "Couldn't find constructor Some of Option type"
  end

let mk_None ~env option_tp : T.expr =
  let make data pos = { T.data; T.pos } in
  let tp_arg = extr_arg_tp option_tp in
  begin match Env.lookup_ctor env (S.NPName "None") with
  | Some (idx, adt_info) -> 
    let {Module.adt_proof; adt_args; adt_ctors; adt_type} = adt_info in
    make (T.ECtor (make (T.ETApp (adt_proof, tp_arg)) Position.nowhere, idx, [], [])) Position.nowhere
  | None -> failwith "Couldn't find constructor None of Option type"
  end

  
