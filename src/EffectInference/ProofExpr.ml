(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of ADT-shape proof expressions. *)

open Common

let unit_ctor =
  { T.ctor_name        = "()";
    T.ctor_targs       = [];
    T.ctor_named       = [];
    T.ctor_arg_schemes = []
  }

let false_ctor =
  { T.ctor_name        = "False";
    T.ctor_targs        = [];
    T.ctor_named       = [];
    T.ctor_arg_schemes = []
  }

let true_ctor =
  { T.ctor_name        = "True";
    T.ctor_targs        = [];
    T.ctor_named       = [];
    T.ctor_arg_schemes = []
  }

let none_ctor =
  { T.ctor_name        = "None";
    T.ctor_targs       = [];
    T.ctor_named       = [];
    T.ctor_arg_schemes = []
  }

let some_ctor tp =
  { T.ctor_name        = "Some";
    T.ctor_targs       = [];
    T.ctor_named       = [];
    T.ctor_arg_schemes = [T.Scheme.of_type tp]
  }

let t_option tp =
  T.Type.t_app (T.Type.t_var T.BuiltinType.tv_option) tp

let tr_proof_expr env (prf : S.proof_expr) =
  match prf with
  | PE_Unit ->
    (T.EUnitPrf, T.Type.t_var T.BuiltinType.tv_unit, [unit_ctor], T.Pure)

  | PE_Bool -> 
    (T.EBoolPrf, T.Type.t_var T.BuiltinType.tv_bool, [false_ctor; true_ctor], T.Pure)

  | PE_Option tp ->
    let tp = Type.tr_type env tp in
    let prf = T.ETApp(T.EOptionPrf, tp) in
    (prf, t_option tp, [none_ctor; some_ctor tp], T.Pure)

  | PE_Var(x, tps) ->
    let tps = List.map (Type.tr_type env) tps in
    let adt = Env.lookup_adt env x in
    assert (List.length adt.adt_args = List.length tps);
    let sub   = T.Subst.for_named_tvars adt.adt_args tps in
    let prf   = ExprUtils.mk_tapps (T.EVar adt.adt_proof) tps in
    let tp    = T.Type.subst sub adt.adt_type in
    let ctors = List.map (T.CtorDecl.subst sub) adt.adt_ctors in
    let eff   =
      if adt.adt_positive then T.Pure
      else T.Impure (T.Effct.pure)
    in
    (prf, tp, ctors, eff)
