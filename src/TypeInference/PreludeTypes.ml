(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Functions allowing the use of some of the types defined in the Prelude. *)

open Common

let mk_Option ~env tp_arg =
  begin match Env.lookup_tvar env (S.NPName "Option") with
  | None    -> failwith "Error"
  | Some tp -> 
    if not (Unification.unify_kind 
      (T.Type.kind tp)
      (T.Kind.k_arrow T.Kind.k_type T.Kind.k_type))
      then failwith "Error";
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

let mk_Some ~env tp_arg expr_arg : T.expr =
  let make data pos = { T.data; T.pos } in
  let tp' = mk_Option ~env tp_arg in
  match T.Type.whnf tp' with
  | Whnf_Neutral(NH_Var x, tp :: []) ->
    begin match Env.lookup_ctor env (S.NPName "Some") with
    | Some (idx, adt_info) -> 
      (* Feed the ADT proof with argument type *)
      let {Module.adt_proof; adt_args; adt_ctors; adt_type} = adt_info in
      make (T.ECtor (adt_proof, idx, [], [expr_arg])) Position.nowhere
    | None -> failwith "Error"
    end
  | _ -> failwith "Error"

let mk_None ~env tp_arg : T.expr =
  let make data pos = { T.data; T.pos } in
  let tp' = mk_Option ~env tp_arg in
  match T.Type.whnf tp' with
  | Whnf_Neutral(NH_Var x, tp :: []) ->
    begin match Env.lookup_ctor env (S.NPName "None") with
    | Some (idx, adt_info) -> 
      (* Feed the ADT proof with argument type *)
      let {Module.adt_proof; adt_args; adt_ctors; adt_type} = adt_info in
      make (T.ECtor (adt_proof, idx, [], [])) Position.nowhere
    | None -> failwith "Error"
    end
  | _ -> failwith "Error"

  
