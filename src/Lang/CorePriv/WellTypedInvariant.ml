(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal type-checker of the Core language *)

(** The role of this type-checker is twofold. First of all, it can be seen as
  a specification of the Core type system. We refer to [Lang.Core] module for
  the definition of types and their intuitive meaning. Moreover, the
  type-checker serves as an assertion that each program representation in Core
  is well-typed. If not, the type-checker fails with internal error as quick
  as possible, making debugging the compiler bit easier.

  One important design decision of the implementation of this type-checker
  should be described here. In order to avoid accidental type-variable capture
  during the type-checking, the type checker refreshes all type-variables on
  the fly: the environment maps original type-variables to their refreshed
  version. The important invariant of the implementation is that all types
  already touched by the type checker (types stored in the environment, types
  returned by [infer_type_eff] function, etc.) are always refreshed, and shall
  not be refreshed further. *)

open Syntax
open TypeBase

(** Environment of type-checking *)
module Env : sig
  type t

  (** Empty (initial environment). It contains only built-in types. *)
  val empty : t

  (** Extend the environment with a regular variable. Types stored in the
    environment should be in their refreshed form, i.e., built from types
    returned by [tr_type] and [Env.add_tvar]. *)
  val add_var  : t -> var -> ttype -> t

  (** Extend the environment with a computationally irrelevant variable.
    Computationally irrelevant variables are available only in computationally
    irrelevant contexts. *)
  val add_irr_var : t -> var -> ttype -> t

  (** Extend the environment with a recursively defined variable. It will be
    available only after the call to [enter_rec_ctx]. *)
  val add_rec_var : t -> var -> ttype -> t

  (** Extend environment with a type variable. It returns its refreshed
    version *)
  val add_tvar : t -> 'k tvar -> t * 'k tvar

  (** Extend environment with a list of type variables. *)
  val add_tvars : t -> TVar.ex list -> t * TVar.ex list

  (** Add a list of constraints to the environment. *)
  val add_constrs : t -> constr list -> t

  (** Move to computationally irrelevant context, making all irrelevant
    variables available *)
  val irrelevant : t -> t

  (** Move to recursive context, making all recursively defined variables
    available *)
  val enter_rec_ctx : t -> t

  (** Lookup for a type of given variable in the environment. *)
  val lookup_var : t -> var -> ttype

  (** Lookup for a refreshed version of given type variable. *)
  val lookup_tvar : t -> 'k tvar -> 'k tvar

  (** Get the current set of constraints. *)
  val constr_set : t -> ConstrSet.t

  (** Get the current scope, i.e., set of available (refreshed) type
    variables. *)
  val scope : t -> TVar.Set.t
end = struct
  module TMap = TVar.Map.Make(TVar)

  type mode =
    | Regular
    | Irrelevant
    | Recursive of int

  type t = {
    var_map    : (mode * ttype) Var.Map.t;
    tvar_map   : TMap.t;
    constr_set : ConstrSet.t;
    scope      : TVar.Set.t;
    irrelevant : bool;
    rec_level  : int
  }

  let empty =
    { var_map    = Var.Map.empty
    ; tvar_map   =
      List.fold_left
        (fun tm (_, TVar.Ex x) -> TMap.add x x tm) 
        TMap.empty
        BuiltinType.all
    ; constr_set = ConstrSet.empty
    ; scope      =
      List.fold_left
        (fun scope (_, TVar.Ex x) -> TVar.Set.add x scope) 
        TVar.Set.empty
        BuiltinType.all
    ; irrelevant = false
    ; rec_level  = 0
    }

  let add_var env x tp =
    { env with
      var_map = Var.Map.add x (Regular, tp) env.var_map
    }

  let add_irr_var env x tp =
    { env with
      var_map = Var.Map.add x (Irrelevant, tp) env.var_map
    }

  let add_rec_var env x tp =
    { env with
      var_map = Var.Map.add x (Recursive env.rec_level, tp) env.var_map
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

  let add_constrs env cs =
    { env with constr_set = ConstrSet.add_list env.constr_set cs }

  let irrelevant env =
    { env with irrelevant = true }

  let enter_rec_ctx env =
    { env with rec_level = env.rec_level + 1 }

  let lookup_var env x =
    match Var.Map.find x env.var_map with
    | (Irrelevant, _) when not env.irrelevant ->
      failwith
        "Internal error: using irrelevant variable in a relevant context."
    | (Recursive lvl, _) when lvl >= env.rec_level ->
      failwith
        "Internal error: non-productive recursive definition"
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

  let constr_set env = env.constr_set

  let scope env = env.scope
end

(** Ensure well-formedness of a type and refresh its type variables according
  to the environment. *)
let rec tr_type : type k. Env.t -> k typ -> k typ =
  fun env tp ->
  match tp with
  | TEffPure -> TEffPure
  | TEffJoin(eff1, eff2) ->
    TEffJoin(tr_type env eff1, tr_type env eff2)
  | TVar x -> TVar (Env.lookup_tvar env x)
  | TArrow(tp1, tp2, eff) ->
    TArrow(tr_type env tp1, tr_type env tp2, tr_type env eff)
  | TForall(x, tp) ->
    let (env, x) = Env.add_tvar env x in
    TForall(x, tr_type env tp)
  | TGuard(cs, tp) ->
    TGuard(List.map (tr_constr env) cs, tr_type env tp)
  | TLabel lbl ->
    let effct = tr_type env lbl.effct in
    let (env, tvars) = Env.add_tvars env lbl.tvars in
    TLabel
      { effct; tvars;
        val_types = List.map (tr_type env) lbl.val_types;
        delim_tp  = tr_type env lbl.delim_tp;
        delim_eff = tr_type env lbl.delim_eff
      }
  | TData(tp, eff, ctors) ->
    TData(tr_type env tp, tr_type env eff, List.map (tr_ctor_type env) ctors)
  | TApp(tp1, tp2) ->
    TApp(tr_type env tp1, tr_type env tp2)

and tr_constr env (eff1, eff2) =
  (tr_type env eff1, tr_type env eff2)

(** Ensure well-formedness of a constructor type and refresh its type
  variables according to the environment. *)
and tr_ctor_type env ctor =
  let (env, tvars) = Env.add_tvars env ctor.ctor_tvars in
  { ctor_name      = ctor.ctor_name;
    ctor_tvars     = tvars;
    ctor_arg_types = List.map (tr_type env) ctor.ctor_arg_types
  }

(** [tr_types_sub env sub xs tps] extends substitution [sub] by a mapping from
  type variables [xs] to refreshed version of types [tps]. It raises internal
  type error when list [xs] and [tps] have different length. *)
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

(** [tr_tvars_sub env sub xs ys] extends environment [env] by variables [xs]
  and extends substitution [sub] by a mapping from [ys] to refreshed [xs].
  It returns a pair: the extended environment and the extended substitution. *)
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

(** Check well-formedness of datatype definition.
  In [check_data env tp xs ctors] it is checked if [ctors] are well-formed,
  assuming the type is parametrized by [xs]. The functions returns the triple
  [(ys, tp_ys, ctors')], where [ys] are refreshed type variables [xs],
  [ctors'] is a refreshed constructor list [ctors], and [tp_ys] is a type [tp]
  applied to variables [ys]. This value is later used to construct types of
  proofs of ADT shape (see [TData] constructor). *)
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

(** The first phase of checking recursive datatype: the type variable of the
  type definition is refreshed and added to the environment. The body of the
  definition remains unchanged. *)
let prepare_data_def env (dd : data_def) =
  match dd with
  | DD_Data adt ->
    let (TVar.Ex a) = adt.tvar in
    let (env, a) = Env.add_tvar env a in
    (env, DD_Data { adt with tvar = TVar.Ex a })
  | DD_Label lbl ->
    let (env, a) = Env.add_tvar env lbl.tvar in
    (env, DD_Label { lbl with tvar = a })

(** Compute the effect attached to the datatype (the effect of
  pattern-matching). Types flagged as positive have pure effect
  of pattern-matching, but the function ensures that provided constructors
  are positive, i.e., all type variables on non-positive
  position fit in [nonrec_scope]. The [nonrec_scope] should be a scope of
  the definition not extended with types defined in the current recursive
  block.

  If the type is not flagged as positive, have always NTerm effect
  attached, and no extra checks are performed. *)
let adt_effect ~nonrec_scope positive args ctors =
  if positive then
    let nonrec_scope = Type.add_tvars_to_scope args nonrec_scope in
    if Type.positive_ctors ~nonrec_scope ctors then
      TEffPure
    else
      InterpLib.InternalError.report
        ~reason:"Type is not positively recursive"
        ()
  else
    Effect.nterm

(** The second and the last phase of checking of recursive type definitions.
  With extended environment, the bodies of definitions are checked and
  refreshed, and the environment is extended with term-level counterparts of
  types (proofs in case of ADTs, and labels in case of effects). The
  [nonrec_scope] should be a scope of the definitions not extended with
  types defined in the current recursive block.

  The function returns extended environment and the effect of evaluating this
  block of definitions. The block has [NTerm] effect if there is a label
  definition in the block, otherwise the block is pure. *)
let finalize_data_def ~nonrec_scope (env, dd_eff) dd =
  match dd with
  | DD_Data adt ->
    let (TVar.Ex a) = adt.tvar in
    let (xs, data_tp, ctors) = check_data env (TVar a) adt.args adt.ctors in
    let eff = adt_effect ~nonrec_scope adt.positive xs ctors in
    let env =
      Env.add_irr_var env adt.proof
        (Type.t_foralls xs (TData(data_tp, eff, ctors))) in
    (env, dd_eff)

  | DD_Label lbl ->
    let effct = TVar lbl.tvar in
    let (eff_env, tvars) = Env.add_tvars env lbl.tvars in
    let val_types = List.map (tr_type eff_env) lbl.val_types in
    let delim_tp  = tr_type eff_env lbl.delim_tp in
    let delim_eff = tr_type eff_env lbl.delim_eff in
    let lbl_tp = TLabel { effct; tvars; val_types; delim_tp; delim_eff } in
    let env = Env.add_var env lbl.var lbl_tp in
    (* We add nterm effect, since generation of a fresh label is not pure *)
    (env, Effect.join Effect.nterm dd_eff)

(** Check block of mutually recursive type definitions.
  The function returns extended environment and the effect of evaluating this
  block of definitions. The block has [NTerm] effect if there is a label
  definition in the block, otherwise the block is pure. *)
let check_data_defs env dds =
  let nonrec_scope = Env.scope env in
  let (env, dds) = List.fold_left_map prepare_data_def env dds in
  List.fold_left (finalize_data_def ~nonrec_scope) (env, TEffPure) dds

(** Infer type end effect of given expression. *)
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
  | ERecCtx e ->
    let (tp, eff) = infer_type_eff (Env.enter_rec_ctx env) e in
    (tp, Effect.join eff Effect.nterm)
  | EApp(v1, v2) ->
    begin match infer_vtype env v1 with
    | TArrow(tp2, tp1, eff) ->
      check_vtype env v2 tp2;
      (tp1, eff)
    | TVar _ | TForall _ | TGuard _ | TLabel _ | TData _ | TApp _ ->
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
    | TVar _ | TArrow _ | TGuard _ | TLabel _ | TData _ | TApp _ ->
      failwith "Internal type error"
    end
  | ECApp v ->
    begin match infer_vtype env v with
    | TGuard(cs, tp) ->
      if
        List.for_all
          (fun (eff1, eff2) -> Type.subeffect (Env.constr_set env) eff1 eff2)
          cs
      then
        (tp, TEffPure)
      else
        failwith "Internal type error"
    | TVar _ | TArrow _ | TForall _ | TLabel _ | TData _ | TApp _ ->
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
      (tp, lbl.effct)

    | TVar _ | TArrow _ | TForall _ | TGuard _ | TData _ | TApp _ ->
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
        infer_type_check_eff env body (TEffJoin(lbl.effct, delim_eff)) in
      let env = Env.add_var env x x_tp in
      check_type_eff env ret delim_tp delim_eff;
      (delim_tp, delim_eff)

    | TVar _ | TArrow _ | TForall _ | TGuard _ | TData _ | TApp _ ->
      failwith "Internal type error"
    end

  | ERepl(_, tp, eff) ->
    (* In this case we have no means to check types further. *)
    (tr_type env tp, tr_type env eff)

  | EReplExpr(e1, _, e2) ->
    let (_, eff1) = infer_type_eff env e1 in
    let (tp, eff2) = infer_type_eff env e2 in
    (tp, Effect.join eff1 eff2)

(** Infer type of given value *)
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
  | VCAbs(cs, body) ->
    let cs = List.map (tr_constr env) cs in
    let env = Env.add_constrs env cs in
    TGuard(cs, infer_type_check_eff env body TEffPure)
  | VCtor(proof, n, tps, args) ->
    infer_ctor_type env proof n tps args (check_vtype env)
  | VExtern(_, tp) -> tr_type env tp

(** [infer_ctor_type env proof n tps args check_arg] checks the type of [n]-th
  constructor of the data type described by [proof], applied to type
  parameters [tps], and regular parameters [args]. The [check_arg] function
  is used type check types of [args]: it takes the parameter and its expected
  type. *)
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

(** Infer type of the expression, but check its effect *)
and infer_type_check_eff env e eff =
  let (tp, eff') = infer_type_eff env e in
  if Type.subeffect (Env.constr_set env) eff' eff then
    tp
  else failwith "Internal effect error"

(** Check both type and effect of the expression. Note that this function
  returns unit. It fails with internal type error, when the type doesn't match.
  *)
and check_type_eff env e tp eff =
  let (tp', eff') = infer_type_eff env e in
  if not (Type.subtype (Env.constr_set env) tp' tp) then
    InterpLib.InternalError.report
      ~reason:"type mismatch"
      ~sloc:(SExprPrinter.tr_expr e)
      ~requested:(SExprPrinter.tr_type tp)
      ~provided:(SExprPrinter.tr_type tp')
      ();
  if not (Type.subeffect (Env.constr_set env) eff' eff) then
    InterpLib.InternalError.report
      ~reason:"effect mismatch"
      ~sloc:(SExprPrinter.tr_expr e)
      ~requested:(SExprPrinter.tr_type eff)
      ~provided:(SExprPrinter.tr_type eff')
      ();
  ()

(** Check type of given value *)
and check_vtype env v tp =
  let tp' = infer_vtype env v in
  if Type.subtype (Env.constr_set env) tp' tp then
    ()
  else InterpLib.InternalError.report
    ~reason:"type mismatch"
    ~sloc:(SExprPrinter.tr_value v)
    ~requested:(SExprPrinter.tr_type tp)
    ~provided:(SExprPrinter.tr_type tp')
    ();

(** Check block of recursive definitions. It is done similarly to checking
  mutually recursive type definitions: in the first phase, the environment is
  extended, and in the second phase, bodies of the definitions are checked. *)
and check_rec_defs env rds =
  let ((rec_env, env), rds) =
    List.fold_left_map prepare_rec_def (env, env) rds in
  List.iter (check_rec_def rec_env) rds;
  env

(** The first phase of checking recursive definition. It returns two extended
  environments (one for recursive definitions, and one for the body of
  let-rec), and untouched body of the definition together with the refreshed
  type. Note that actual variable is forgotten, because the environment is
  extended, so the variable is no longer needed. *)
and prepare_rec_def (rec_env, env) (x, tp, body) =
  let tp = tr_type env tp in
  let rec_env = Env.add_rec_var rec_env x tp in
  let env = Env.add_var env x tp in
  ((rec_env, env), (body, tp))

(** Check if given body of recursive definition has expected type. The
  function is called on environment extended with [Env.add_rec_var] function,
  so it also checks if the body is productive. *)
and check_rec_def env (body, tp) =
  check_type_eff env body tp TEffPure

(** Check if given program is well-typed *)
let check_program p =
  check_type_eff Env.empty p Type.t_unit Effect.prog_effect
