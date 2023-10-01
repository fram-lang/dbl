(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal type-checker of the Core language *)

(* Author: Piotr Polesiuk, 2023 *)

open Syntax
open TypeBase

(** Environment of type-checking *)
module Env : sig
  type t

  val empty : t

  val add_var  : t -> var -> ttype -> t

  (** Extend environment with a type variable. It returns its refreshed
    version *)
  val add_tvar : t -> 'k tvar -> t * 'k tvar

  val lookup_var : t -> var -> ttype

  val lookup_tvar : t -> 'k tvar -> 'k tvar
end = struct
  module TMap = TVar.Map.Make(TVar)

  type t = {
    var_map  : ttype Var.Map.t;
    tvar_map : TMap.t
  }

  let empty =
    { var_map  = Var.Map.empty
    ; tvar_map = TMap.empty
    }

  let add_var env x tp =
    { env with
      var_map = Var.Map.add x tp env.var_map
    }

  let add_tvar env x =
    let y = TVar.clone x in
    { env with
      tvar_map = TMap.add x y env.tvar_map
    }, y

  let lookup_var env x =
    try Var.Map.find x env.var_map with
    | Not_found ->
      failwith "Internal error: unbound variable"

  let lookup_tvar env x =
    try TMap.find x env.tvar_map with
    | Not_found ->
      failwith "Internal error: unbound type variable"
end

(** Ensure well-formedness of a type and refresh its type variables according
  to an environment. *)
let rec tr_type : type k. Env.t -> k typ -> k typ =
  fun env tp ->
  match tp with
  | TUnit  -> TUnit
  | TVar x -> TVar (Env.lookup_tvar env x)
  | TArrow(tp1, tp2) ->
    TArrow(tr_type env tp1, tr_type env tp2)
  | TForall(x, tp) ->
    let (env, x) = Env.add_tvar env x in
    TForall(x, tr_type env tp)

let rec infer_type env e =
  match e with
  | EValue v -> infer_vtype env v
  | ELetPure(x, e1, e2) ->
    let tp1 = infer_type env e1 in
    infer_type (Env.add_var env x tp1) e2
  | ELet(x, e1, e2) ->
    let tp1 = infer_type env e1 in
    infer_type (Env.add_var env x tp1) e2
  | EApp(v1, v2) ->
    begin match infer_vtype env v1 with
    | TArrow(tp2, tp1) ->
      check_vtype env v2 tp2;
      tp1
    | TUnit | TVar _ | TForall _ ->
      failwith "Internal type error"
    end
  | ETApp(v, tp) ->
    begin match infer_vtype env v with
    | TForall(x, body) ->
      let tp = tr_type env tp in
      begin match Kind.equal (TVar.kind x) (Type.kind tp) with
      | Equal    -> Type.subst_type x tp body
      | NotEqual -> failwith "Internal kind error"
      end
    | TUnit | TVar _ | TArrow _ ->
      failwith "Internal type error"
    end

and infer_vtype env v =
  match v with
  | VUnit -> TUnit
  | VVar x -> Env.lookup_var env x
  | VFn(x, tp, body) ->
    let tp = tr_type env tp in
    let env = Env.add_var env x tp in
    TArrow(tp, infer_type env body)
  | VTFun(x, body) ->
    let (env, x) = Env.add_tvar env x in
    TForall(x, infer_type env body)

and check_type env e tp =
  let tp' = infer_type env e in
  if Type.subtype tp' tp then
    ()
  else failwith "Internal type error"

and check_vtype env v tp =
  let tp' = infer_vtype env v in
  if Type.subtype tp' tp then
    ()
  else failwith "Internal type error"

let check_program p =
  check_type Env.empty p TUnit
