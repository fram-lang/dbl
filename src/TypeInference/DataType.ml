(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

open Common

let check_ctor_decl ~data_targs env ({ data = ctor; _ } : S.ctor_decl) =
  Uniqueness.check_ctor_named_types data_targs ctor.cd_targs;
  let (env, ctor_targs) = Type.tr_named_type_args env ctor.cd_targs in
  { T.ctor_name        = ctor.cd_name;
    T.ctor_targs;
    T.ctor_named       = List.map (Type.tr_named_scheme env) ctor.cd_named;
    T.ctor_arg_schemes = List.map (Type.tr_scheme env) ctor.cd_arg_schemes
  }

let check_ctor_decls ~data_targs env ctors =
  List.map (check_ctor_decl ~data_targs env) ctors

let open_data_ctor adt (env, n) ({ data = ctor; _ } : S.ctor_decl) =
  let env = Env.add_ctor env ~public:ctor.cd_public ctor.cd_name n adt in
  (env, n+1)

let open_data env (dd : S.data_def) adt =
  let (DD_Data(_, _, _, ctors)) = dd.data in
  List.fold_left (open_data_ctor adt) (env, 0) ctors
  |> fst

let check_data_def_main env (dd : S.data_def) kind =
  let (DD_Data(_, _, args, ctors)) = dd.data in
  let (env, args) = Type.tr_named_type_args env args in
  let kind' =
    T.Kind.k_arrows
      (List.map (fun (_, x) -> T.TVar.kind x) args)
      T.Kind.k_type in
  begin match Unification.unify_kind kind kind' with
  | true -> ()
  | false -> assert false
  end;
  Uniqueness.check_ctor_uniqueness ctors;
  let ctors = check_ctor_decls ~data_targs:args env ctors in
  (args, ctors)

let finalize_data_def env (dd : S.data_def) x args ctors =
  let (DD_Data(public, name, _, _)) = dd.data in
  let px = Var.fresh ~name () in
  let info = {
    Module.adt_proof = { T.pos = dd.pos; T.data = T.EVar px };
    Module.adt_args  = args;
    Module.adt_ctors = ctors;
    Module.adt_type  =
      T.Type.t_apps (T.Type.t_var x)
        (List.map (fun (_, x) -> T.Type.t_var x) args)
  } in
  let env = Env.add_data env x info in
  let env = open_data env dd info in
  let dd = {
    T.dd_tvar  = x;
    T.dd_proof = px;
    T.dd_args  = args;
    T.dd_ctors = ctors
  } in
  (env, dd)

let check_data_def env (dd : S.data_def) =
  let (DD_Data(public, name, _, _)) = dd.data in
  let kind = T.Kind.fresh_uvar () in
  let (args, ctors) = check_data_def_main env dd kind in
  let (env, x) = Env.add_tvar ~pos:dd.pos env ~public name kind in
  finalize_data_def env dd x args ctors

let prepare_rec_data_def env (dd : S.data_def) =
  let (DD_Data(public, name, args, _)) = dd.data in
  let (_, args) = Type.tr_named_type_args env args in
  let kind =
    T.Kind.k_arrows
      (List.map (fun (_, x) -> T.TVar.kind x) args)
      T.Kind.k_type in
  let (env, x) = Env.add_tvar ~pos:dd.pos env ~public name kind in
  (env, (x, dd))

let finalize_rec_data_def env (x, dd) =
  let (args, ctors) = check_data_def_main env dd (T.TVar.kind x) in
  finalize_data_def env dd x args ctors

let check_rec_data_defs env dds =
  let (env, dds) = List.fold_left_map prepare_rec_data_def env dds in
  List.fold_left_map finalize_rec_data_def env dds
