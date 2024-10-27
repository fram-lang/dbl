(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal type-checker of the Core language *)

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

  (** Extend environment with a list of type variables. *)
  val add_tvars : t -> TVar.ex list -> t * TVar.ex list

  (** Move to computationally irrelevant context, making all irrelevant
    variables available *)
  val irrelevant : t -> t

  val lookup_var : t -> var -> ttype

  val lookup_tvar : t -> 'k tvar -> 'k tvar

  val scope : t -> TVar.Set.t
end = struct
  module TMap = TVar.Map.Make(TVar)

  type t = {
    var_map    : (bool * ttype) Var.Map.t;
    tvar_map   : TMap.t;
    scope      : TVar.Set.t;
    irrelevant : bool
  }

  let empty =
    { var_map    = Var.Map.empty
    ; tvar_map   =
      List.fold_left
        (fun tm (_, TVar.Ex x) -> TMap.add x x tm) 
        TMap.empty
        BuiltinType.all
    ; scope      =
      List.fold_left
        (fun scope (_, TVar.Ex x) -> TVar.Set.add x scope) 
        TVar.Set.empty
        BuiltinType.all
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
      tvar_map = TMap.add x y env.tvar_map;
      scope    = TVar.Set.add y env.scope
    }, y

  let add_tvar_ex env (TVar.Ex x) =
    let (env, x) = add_tvar env x in
    (env, TVar.Ex x)

  let add_tvars env xs =
    List.fold_left_map add_tvar_ex env xs

  let irrelevant env =
    { env with irrelevant = true }

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
      InterpLib.InternalError.report
        ~reason:"unbound type variable"
        ~provided:(SExprPrinter.tr_tvar x)
        ()

  let scope env = env.scope
end

(** Ensure well-formedness of a type and refresh its type variables according
  to an environment. *)
let rec tr_type : type k. Env.t -> k typ -> k typ =
  fun env tp ->
  match tp with
  | TUVar _ ->
    InterpLib.InternalError.report
      ~reason:"Unsolved unification variables left." ();
  | TEffPure -> TEffPure
  | TEffJoin(eff1, eff2) ->
    TEffJoin(tr_type env eff1, tr_type env eff2)
  | TVar x -> TVar (Env.lookup_tvar env x)
  | TArrow(tp1, tp2, eff) ->
    TArrow(tr_type env tp1, tr_type env tp2, tr_type env eff)
  | TForall(x, tp) ->
    let (env, x) = Env.add_tvar env x in
    TForall(x, tr_type env tp)
  | TLabel lbl ->
    let effect = tr_type env lbl.effect in
    let (env, tvars) = Env.add_tvars env lbl.tvars in
    TLabel
      { effect; tvars;
        val_types = List.map (tr_type env) lbl.val_types;
        delim_tp  = tr_type env lbl.delim_tp;
        delim_eff = tr_type env lbl.delim_eff
      }
  | TData(tp, eff, ctors) ->
    TData(tr_type env tp, tr_type env eff, List.map (tr_ctor_type env) ctors)
  | TApp(tp1, tp2) ->
    TApp(tr_type env tp1, tr_type env tp2)

and tr_ctor_type env ctor =
  let (env, tvars) = Env.add_tvars env ctor.ctor_tvars in
  { ctor_name      = ctor.ctor_name;
    ctor_tvars     = tvars;
    ctor_arg_types = List.map (tr_type env) ctor.ctor_arg_types
  }

let rec tr_types_sub env sub xs tps =
  match xs, tps with
  | [], [] -> sub
  | TVar.Ex x :: xs, Type.Ex tp :: tps ->
    begin match Kind.equal (TVar.kind x) (Type.kind tp) with
    | Equal ->
      tr_types_sub env (Subst.add sub x (tr_type env tp)) xs tps
    | NotEqual ->
      failwith "Internal kind error"
    end
  | [], _ :: _ | _ :: _, [] ->
    failwith "Internal type error"

let rec tr_tvars_sub env sub xs ys =
  match xs, ys with
  | [], [] -> (env, sub)
  | TVar.Ex x :: xs, TVar.Ex y :: ys ->
    begin match Kind.equal (TVar.kind x) (TVar.kind y) with
    | Equal ->
      let (env, z) = Env.add_tvar env x in
      tr_tvars_sub env (Subst.add sub y (TVar z)) xs ys
    | NotEqual ->
      failwith "Internal kind error"
    end
  | [], _ :: _ | _ :: _, [] ->
    failwith "Internal type error"

let rec check_data : type k.
    Env.t -> k typ -> TVar.ex list -> ctor_type list ->
      TVar.ex list * ttype * ctor_type list =
  fun env tp xs ctors ->
  match xs, Type.kind tp with
  | [], KType -> (xs, tp, List.map (tr_ctor_type env) ctors)
  | Ex x :: xs, KArrow(k1, k2) ->
    begin match Kind.equal (TVar.kind x) k1 with
    | Equal ->
      let (env, x) = Env.add_tvar env x in
      let (xs, tp, ctors) = check_data env (TApp(tp, TVar x)) xs ctors in
      (Ex x :: xs, tp, ctors)
    | NotEqual ->
      failwith "Internal kind error"
    end
  | _ ->
    failwith "Internal kind error"

let prepare_data_def env (dd : data_def) =
  match dd with
  | DD_Data adt ->
    let (TVar.Ex a) = adt.tvar in
    let (env, a) = Env.add_tvar env a in
    (env, DD_Data { adt with tvar = TVar.Ex a })
  | DD_Label lbl ->
    let (env, a) = Env.add_tvar env lbl.tvar in
    (env, DD_Label { lbl with tvar = a })

let finalize_data_def ~nonrec_scope (env, dd_eff) dd =
  match dd with
  | DD_Data adt ->
    let (TVar.Ex a) = adt.tvar in
    let (xs, data_tp, ctors) = check_data env (TVar a) adt.args adt.ctors in
    let eff =
      if not adt.strictly_positive then
        Effect.nterm
      else if Type.strictly_positive_ctors ~nonrec_scope ctors then
        TEffPure
      else
        InterpLib.InternalError.report
          ~reason:"Type is not strictly positvely recursive"
          ()
    in
    let env =
      Env.add_irr_var env adt.proof
        (Type.t_foralls xs (TData(data_tp, eff, ctors))) in
    (env, dd_eff)

  | DD_Label lbl ->
    let effect = TVar lbl.tvar in
    let (eff_env, tvars) = Env.add_tvars env lbl.tvars in
    let val_types = List.map (tr_type eff_env) lbl.val_types in
    let delim_tp  = tr_type eff_env lbl.delim_tp in
    let delim_eff = tr_type eff_env lbl.delim_eff in
    let lbl_tp = TLabel { effect; tvars; val_types; delim_tp; delim_eff } in
    let env = Env.add_var env lbl.var lbl_tp in
    (* We add nterm effect, since generation of a fresh label is not pure *)
    (env, Effect.join Effect.nterm dd_eff)

let check_data_defs env dds =
  let nonrec_scope = Env.scope env in
  let (env, dds) = List.fold_left_map prepare_data_def env dds in
  List.fold_left (finalize_data_def ~nonrec_scope) (env, TEffPure) dds

let rec infer_type_eff env e =
  match e with
  | EValue v -> (infer_vtype env v, TEffPure)
  | ELet(x, e1, e2) ->
    let (tp1, eff1) = infer_type_eff env e1 in
    let (tp2, eff2) = infer_type_eff (Env.add_var env x tp1) e2 in
    (tp2, Effect.join eff1 eff2)
  | ELetPure(x, e1, e2) ->
    let tp1 = infer_type_check_eff env e1 TEffPure in
    infer_type_eff (Env.add_var env x tp1) e2
  | ELetIrr(x, e1, e2) ->
    let tp1 = infer_type_check_eff (Env.irrelevant env) e1 TEffPure in
    infer_type_eff (Env.add_irr_var env x tp1) e2
  | ELetRec(rds, e) ->
    let env = check_rec_defs env rds in
    infer_type_eff env e
  | EApp(v1, v2) ->
    begin match infer_vtype env v1 with
    | TArrow(tp2, tp1, eff) ->
      check_vtype env v2 tp2;
      (tp1, eff)
    | TUVar _ ->
      InterpLib.InternalError.report
        ~reason:"Unsolved unification variables left." ();
    | TVar _ | TForall _ | TLabel _ | TData _ | TApp _ ->
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
    | TUVar _ ->
      InterpLib.InternalError.report
        ~reason:"Unsolved unification variables left." ();
    | TVar _ | TArrow _ | TLabel _ | TData _ | TApp _ ->
      failwith "Internal type error"
    end

  | EData(dds, e) ->
    let scope = Env.scope env in
    let (env, eff1) = check_data_defs env dds in
    let (tp, eff2) = infer_type_eff env e in
    begin match
      Type.supertype_in_scope scope tp, Type.supereffect_in_scope scope eff2
    with
    | Some tp, Some eff2 -> (tp, Effect.join eff1 eff2)
    | _ ->
      failwith "Internal type error: escaping type variable"
    end

  | EMatch(proof, v, cls, tp, eff) ->
    let tp  = tr_type env tp in
    let eff = tr_type env eff in
    begin match infer_type_check_eff (Env.irrelevant env) proof TEffPure with
    | TData(data_tp, meff, ctors) when List.length cls = List.length ctors ->
      check_vtype env v data_tp;
      List.iter2 (fun cl ctor ->
          let xs  = cl.cl_vars in
          let (env, sub) = 
            tr_tvars_sub env Subst.empty cl.cl_tvars ctor.ctor_tvars in
          let tps = List.map (Subst.in_type sub) ctor.ctor_arg_types in
          assert (List.length xs = List.length tps);
          let env = List.fold_left2 Env.add_var env xs tps in
          check_type_eff env cl.cl_body tp eff
        ) cls ctors;
      (tp, Effect.join meff eff)
    | _ ->
      failwith "Internal type error"
    end

  | EShift(v, tvs, xs, k, body, tp) ->
    begin match infer_vtype env v with
    | TLabel lbl ->
      let tp = tr_type env tp in
      let (env, sub) = tr_tvars_sub env Subst.empty tvs lbl.tvars in
      let tps  = List.map (Subst.in_type sub) lbl.val_types in
      let tp0  = Subst.in_type sub lbl.delim_tp in
      let eff0 = Subst.in_type sub lbl.delim_eff in
      assert (List.length xs = List.length tps);
      let env = List.fold_left2 Env.add_var env xs tps in
      let env = Env.add_var env k (TArrow(tp, tp0, eff0)) in
      check_type_eff env body tp0 eff0;
      (tp, lbl.effect)

    | TUVar _ ->
      InterpLib.InternalError.report
        ~reason:"Unsolved unification variables left." ();
    | TVar _ | TArrow _ | TForall _ | TData _ | TApp _ ->
      failwith "Internal type error"
    end

  | EReset(v, tps, vs, body, x, ret) ->
    begin match infer_vtype env v with
    | TLabel lbl ->
      let sub = tr_types_sub env Subst.empty lbl.tvars tps in
      if List.length lbl.val_types <> List.length vs then
        failwith "Internal type error (constructor arity)";
      List.iter2 (check_vtype env) vs
        (List.map (Subst.in_type sub) lbl.val_types);
      let delim_tp = Subst.in_type sub lbl.delim_tp in
      let delim_eff = Subst.in_type sub lbl.delim_eff in
      let x_tp =
        infer_type_check_eff env body (TEffJoin(lbl.effect, delim_eff)) in
      let env = Env.add_var env x x_tp in
      check_type_eff env ret delim_tp delim_eff;
      (delim_tp, delim_eff)

    | TUVar _ ->
      InterpLib.InternalError.report
        ~reason:"Unsolved unification variables left." ();
    | TVar _ | TArrow _ | TForall _ | TData _ | TApp _ ->
      failwith "Internal type error"
    end

  | ERepl(_, tp, eff) ->
    (* In this case we have no means to check types further. *)
    (tr_type env tp, tr_type env eff)

  | EReplExpr(e1, _, e2) ->
    let (_, eff1) = infer_type_eff env e1 in
    let (tp, eff2) = infer_type_eff env e2 in
    (tp, Effect.join eff1 eff2)

and infer_vtype env v =
  match v with
  | VNum _ -> TVar BuiltinType.tv_int
  | VNum64 _ -> TVar BuiltinType.tv_int64
  | VStr _ -> TVar BuiltinType.tv_string
  | VVar x -> Env.lookup_var env x
  | VFn(x, tp, body) ->
    let tp = tr_type env tp in
    let env = Env.add_var env x tp in
    let (tp2, eff) = infer_type_eff env body in
    TArrow(tp, tp2, eff)
  | VTFun(x, body) ->
    let (env, x) = Env.add_tvar env x in
    TForall(x, infer_type_check_eff env body TEffPure)
  | VCtor(proof, n, tps, args) ->
    infer_ctor_type env proof n tps args (check_vtype env)
  | VExtern(_, tp) -> tr_type env tp

and infer_ctor_type env proof n tps args check_arg =
  assert (n >= 0);
  begin match infer_type_check_eff (Env.irrelevant env) proof TEffPure with
  | TData(tp, _, ctors) ->
    begin match List.nth_opt ctors n with
    | Some ctor ->
      let sub = tr_types_sub env Subst.empty ctor.ctor_tvars tps in
      if List.length ctor.ctor_arg_types <> List.length args then
        failwith "Internal type error (constructor arity)";
      List.iter2 check_arg args
        (List.map (Subst.in_type sub) ctor.ctor_arg_types);
      tp
    | None ->
      failwith "Internal type error"
    end
  | _ ->
    failwith "Internal type error"
  end

and infer_type_check_eff env e eff =
  let (tp, eff') = infer_type_eff env e in
  if Type.subeffect eff' eff then
    tp
  else failwith "Internal effect error"

and check_type_eff env e tp eff =
  let (tp', eff') = infer_type_eff env e in
  if not (Type.subtype tp' tp) then
    InterpLib.InternalError.report
      ~reason:"type mismatch"
      ~sloc:(SExprPrinter.tr_expr e)
      ~requested:(SExprPrinter.tr_type tp)
      ~provided:(SExprPrinter.tr_type tp')
      ();
  if not (Type.subeffect eff' eff) then
    InterpLib.InternalError.report
      ~reason:"effect mismatch"
      ~sloc:(SExprPrinter.tr_expr e)
      ~requested:(SExprPrinter.tr_type eff)
      ~provided:(SExprPrinter.tr_type eff')
      ();
  ()

and check_vtype env v tp =
  let tp' = infer_vtype env v in
  if Type.subtype tp' tp then
    ()
  else InterpLib.InternalError.report
    ~reason:"type mismatch"
    ~sloc:(SExprPrinter.tr_value v)
    ~requested:(SExprPrinter.tr_type tp)
    ~provided:(SExprPrinter.tr_type tp')
    ();

and check_rec_defs env rds =
  let rec_vars = List.map (fun (x, _, _) -> x) rds in
  let (env, rds) = List.fold_left_map prepare_rec_def env rds in
  List.iter (check_rec_def env rec_vars) rds;
  env

and prepare_rec_def env (x, tp, body) =
  let tp = tr_type env tp in
  let env = Env.add_var env x tp in
  (env, (body, tp))

and check_rec_def env rec_vars (body, tp) =
  check_vtype_productive env rec_vars body tp

(** Check both type and productiveness of given value. Value is considered
  productive if all non-trivial subexpressions are guarded by
  lambda-abstraction with [NTerm] effect. The expression is considered
  non-trivial if it is not a value or is a variable defined in current block
  of recursive definitions. The [rec_vars] parameter should be a list of
  such a variables. Only productive values can be used in recursive
  definitions. *)
and check_vtype_productive env rec_vars v tp =
  match v with
  | VNum _ | VNum64 _ | VStr _ | VExtern _ ->
    check_vtype env v tp
  | VVar x when not (List.exists (Var.equal x) rec_vars) ->
    check_vtype env v tp

  | VFn(x, xtp, body) ->
    begin match body, tp with
    | _, TArrow(_, _, eff) when Type.subeffect Effect.nterm eff ->
      check_vtype env v tp
    | EValue body, TArrow(tp1, tp2, _) ->
      let xtp = tr_type env xtp in
      if not (Type.subtype tp1 xtp) then
        failwith "Internal type error";
      let env = Env.add_var env x xtp in
      let rec_vars = List.filter (Fun.negate (Var.equal x)) rec_vars in
      check_vtype_productive env rec_vars body tp2
    | _, TArrow _ ->
      InterpLib.InternalError.report
        ~reason:"non-productive recursive definition"
        ()
    | _ ->
      failwith "Internal type error"
    end

  | VTFun(x, EValue body) ->
    let (env, x) = Env.add_tvar env x in
    begin match tp with
    | TForall(y, tp) ->
      begin match Kind.equal (TVar.kind x) (TVar.kind y) with
      | Equal ->
        let tp = Type.subst_type y (TVar x) tp in
        check_vtype_productive env rec_vars body tp
      | NotEqual ->
        failwith "Internal kind error"
      end
    | _ -> failwith "Internal type error"
    end

  | VCtor(proof, n, tps, args) ->
    let tp' = infer_ctor_type env proof n tps args
      (check_vtype_productive env rec_vars) in
    if Type.subtype tp' tp then
      ()
    else failwith "Internal type error"

  | VVar _ | VTFun _ ->
    InterpLib.InternalError.report
      ~reason:"non-productive recursive definition"
      ()

let check_program p =
  check_type_eff Env.empty p Type.t_unit Effect.prog_effect
