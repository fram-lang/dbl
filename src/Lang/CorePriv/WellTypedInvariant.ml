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

  (** Extend the environment with a regular variable *)
  val add_var  : t -> var -> ttype -> t

  (** Extend the environment with a computationally irrelevant variable.
    Computationally irrelevant variables are available only in computationally
    irrelevant contexts. *)
  val add_irr_var : t -> var -> ttype -> t

  (** Extend environment with a type variable. It returns its refreshed
    version *)
  val add_tvar : t -> 'k tvar -> t * 'k tvar

  val lookup_var : t -> var -> ttype

  val lookup_tvar : t -> 'k tvar -> 'k tvar
end = struct
  module TMap = TVar.Map.Make(TVar)

  type t = {
    var_map    : (bool * ttype) Var.Map.t;
    tvar_map   : TMap.t;
    irrelevant : bool
  }

  let empty =
    { var_map    = Var.Map.empty
    ; tvar_map   = TMap.empty
    ; irrelevant = false
    }

  let add_var env x tp =
    { env with
      var_map = Var.Map.add x (false, tp) env.var_map
    }

  let add_irr_var env x tp =
    { env with
      var_map = Var.Map.add x (true, tp) env.var_map
    }

  let add_tvar env x =
    let y = TVar.clone x in
    { env with
      tvar_map = TMap.add x y env.tvar_map
    }, y

  let lookup_var env x =
    match Var.Map.find x env.var_map with
    | (irr, _) when irr && not env.irrelevant ->
      failwith
        "Internal error: using irrelevant variable in a relevant context."
    | (_, tp) -> tp
    | exception Not_found ->
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
  | TEffPure -> TEffPure
  | TEffJoin(eff1, eff2) ->
    TEffJoin(tr_type env eff1, tr_type env eff2)
  | TVar x -> TVar (Env.lookup_tvar env x)
  | TArrow(tp1, tp2, eff) ->
    TArrow(tr_type env tp1, tr_type env tp2, tr_type env eff)
  | TForall(x, tp) ->
    let (env, x) = Env.add_tvar env x in
    TForall(x, tr_type env tp)
  | TData(tp, ctors) ->
    TData(tr_type env tp, List.map (tr_ctor_type env) ctors)

and tr_ctor_type env ctor =
  { ctor_name      = ctor.ctor_name;
    ctor_arg_types = List.map (tr_type env) ctor.ctor_arg_types
  }

let check_data : type k.
    Env.t -> k typ -> ctor_type list -> ttype * ctor_type list =
  fun env tp ctors ->
  match Type.kind tp with
  | KType -> (tp, List.map (tr_ctor_type env) ctors)
  | _ ->
    failwith "Internal kind error"

let rec infer_type_eff env e =
  match e with
  | EValue v -> (infer_vtype env v, TEffPure)
  | ELetPure(x, e1, e2) ->
    let tp1 = infer_type_check_eff env e1 TEffPure in
    infer_type_eff (Env.add_var env x tp1) e2
  | ELet(x, e1, e2) ->
    let (tp1, eff1) = infer_type_eff env e1 in
    let (tp2, eff2) = infer_type_eff (Env.add_var env x tp1) e2 in
    (tp2, Effect.join eff1 eff2)
  | EApp(v1, v2) ->
    begin match infer_vtype env v1 with
    | TArrow(tp2, tp1, eff) ->
      check_vtype env v2 tp2;
      (tp1, eff)
    | TUnit | TVar _ | TForall _ | TData _ ->
      failwith "Internal type error"
    end
  | ETApp(v, tp) ->
    begin match infer_vtype env v with
    | TForall(x, body) ->
      let tp = tr_type env tp in
      begin match Kind.equal (TVar.kind x) (Type.kind tp) with
      | Equal    -> (Type.subst_type x tp body, TEffPure)
      | NotEqual -> failwith "Internal kind error"
      end
    | TUnit | TVar _ | TArrow _ | TData _ ->
      failwith "Internal type error"
    end
  | EData(a, x, ctors, e) ->
    let old_env = env in
    let (env, a) = Env.add_tvar env a in
    let (data_tp, ctors) = check_data old_env (TVar a) ctors in
    let env = Env.add_irr_var env x (TData(data_tp, ctors)) in
    let (tp, eff) = infer_type_eff env e in
    begin match
      Type.supertype_without a tp, Type.supereffect_without a eff
    with
    | Some tp, Some eff -> (tp, eff)
    | _ ->
      failwith "Internal type error: escaping type variable"
    end
  | EHandle(a, x, e, h, tp, eff) ->
    let tp  = tr_type env tp in
    let eff = tr_type env eff in
    let (env', a) = Env.add_tvar env a in
    let htp = infer_h_type env a h tp eff in
    check_type_eff (Env.add_var env' x htp) e tp (TEffJoin(TVar a, eff));
    (tp, eff)

  | ERepl(_, eff) ->
    (* In this case we have no means to check types further. *)
    (TUnit, tr_type env eff)

  | EReplExpr(e1, _, e2) ->
    let (_, eff1) = infer_type_eff env e1 in
    let (tp, eff2) = infer_type_eff env e2 in
    (tp, Effect.join eff1 eff2)

and infer_vtype env v =
  match v with
  | VUnit -> TUnit
  | VVar x -> Env.lookup_var env x
  | VFn(x, tp, body) ->
    let tp = tr_type env tp in
    let env = Env.add_var env x tp in
    let (tp2, eff) = infer_type_eff env body in
    TArrow(tp, tp2, eff)
  | VTFun(x, body) ->
    let (env, x) = Env.add_tvar env x in
    TForall(x, infer_type_check_eff env body TEffPure)

and infer_h_type env a h tp eff =
  match h with
  | HEffect(tp_in, tp_out, x, r, body) ->
    let tp_in  = tr_type env tp_in  in
    let tp_out = tr_type env tp_out in
    let env = Env.add_var env x tp_in in
    let env = Env.add_var env r (TArrow(tp_out, tp, eff)) in
    check_type_eff env body tp eff;
    TArrow(tp_in, tp_out, TVar a)

and infer_type_check_eff env e eff =
  let (tp, eff') = infer_type_eff env e in
  if Type.subeffect eff' eff then
    tp
  else failwith "Internal effect error"

and check_type_eff env e tp eff =
  let (tp', eff') = infer_type_eff env e in
  if Type.subtype tp' tp && Type.subeffect eff' eff then
    ()
  else failwith "Internal type error"

and check_vtype env v tp =
  let tp' = infer_vtype env v in
  if Type.subtype tp' tp then
    ()
  else failwith "Internal type error"

let check_program p =
  check_type_eff Env.empty p TUnit Effect.prog_effect
