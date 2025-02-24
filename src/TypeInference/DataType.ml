(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

open Common

type ctor_decl_list = T.ctor_decl_expr list

(* ========================================================================= *)

(** Check if ADT definition is positively recursive *)
let adt_positive ~scope ~nonrec_scope args ctors =
  List.for_all (T.CtorDecl.is_positive ~scope ~args ~nonrec_scope) ctors

(* ========================================================================= *)

let kind args =
  T.Kind.k_arrows
    (List.map (fun (_, _, x) -> T.TVar.kind x) args)
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

let open_data_ctor ~public adt (env, n) (ctor : T.ctor_decl) =
  let env = Env.add_ctor ~public env ctor.ctor_name n adt in
  (env, n + 1)

let open_data ~public env adt ctors =
  let (env, _) =
    List.fold_left (open_data_ctor ~public adt) (env, 0) ctors in
  env

let finalize_check ~nonrec_scope ~public env x ~name args ctors =
  let px = Var.fresh ~name () in
  let adt_ctors = List.map T.CtorDeclExpr.to_ctor_decl ctors in
  let adt_effect =
    let scope = Env.scope env in
    if adt_positive ~scope ~nonrec_scope args adt_ctors then T.Pure
    else T.Impure
  in
  let arg_tps = List.map (fun (_, x) -> T.Type.t_var x) args in
  let info = {
    Module.adt_args   = args;
    Module.adt_proof  = PE_Var(px, arg_tps);
    Module.adt_ctors  = adt_ctors;
    Module.adt_type   = T.Type.t_apps (T.Type.t_var x) arg_tps;
    Module.adt_effect = adt_effect
  } in
  let env = Env.add_adt ~public env x info in
  let env = open_data ~public env info adt_ctors in
  let dd =
    T.DD_Data {
      tvar  = x;
      proof = px;
      args  = args;
      ctors = ctors;
      eff   = adt_effect
    } in
  (env, dd)
