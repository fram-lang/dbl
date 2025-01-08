(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

open Common

type ctor_decl_list = T.ctor_decl_expr list

(* ========================================================================= *)

(** Check if ADT definition is strictly positively recursive *)
let adt_strictly_positive ~nonrec_scope args ctors =
  let nonrec_scope = List.fold_left T.Scope.add_named nonrec_scope args in
  List.for_all (T.CtorDecl.strictly_positive ~nonrec_scope) ctors

(* ========================================================================= *)

let kind args =
  T.Kind.k_arrows
    (List.map (fun (_, x) -> T.TVar.kind x) args)
    T.Kind.k_type

let check_ctor_decl ~data_targs env ({ data = ctor; _ } : S.ctor_decl) =
  let (env, targs, named) =
    Type.tr_scheme_args ~data_targs env ctor.cd_named_args in
  let arg_schemes = List.map (Type.tr_scheme env) ctor.cd_arg_schemes in
  { T.cde_name        = ctor.cd_name;
    T.cde_targs       = targs;
    T.cde_named       = named;
    T.cde_arg_schemes = arg_schemes
  }

let check_ctor_decls ~data_targs env ctors =
  Uniqueness.check_ctor_uniqueness ctors;
  List.map (check_ctor_decl ~data_targs env) ctors

let open_data_ctor ~public adt (env, penv, n) (ctor : T.ctor_decl) =
  let (env, penv) =
    ParameterEnv.add_ctor ~public env penv ctor.ctor_name n adt in
  (env, penv, n + 1)

let open_data ~public env penv adt ctors =
  let (env, penv, _) =
    List.fold_left (open_data_ctor ~public adt) (env, penv, 0) ctors in
  (env, penv)

let finalize_check ~nonrec_scope ~public env penv x ~name args ctors =
  let px = Var.fresh ~name () in
  let adt_ctors = List.map T.CtorDeclExpr.to_ctor_decl ctors in
  let adt_effect =
    if adt_strictly_positive ~nonrec_scope args adt_ctors then T.Pure
    else T.Impure
  in
  let info = {
    Module.adt_proof  = { T.pos = Position.nowhere; T.data = T.EVar px };
    Module.adt_args   = args;
    Module.adt_ctors  = adt_ctors;
    Module.adt_type   =
      T.Type.t_apps (T.Type.t_var x)
        (List.map (fun (_, x) -> T.Type.t_var x) args);
    Module.adt_effect = adt_effect
  } in
  let env = Env.add_data ~public env x info in
  let (env, penv) = open_data ~public env penv info adt_ctors in
  let dd =
    T.DD_Data {
      tvar   = x;
      proof  = px;
      args   = args;
      ctors  = ctors;
      effect = adt_effect
    } in
  (env, penv, dd)
