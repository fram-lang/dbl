(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

open Common

type ctor_decl_list = (S.is_public * T.ctor_decl) list

let kind args =
  T.Kind.k_arrows
    (List.map (fun (_, x) -> T.TVar.kind x) args)
    T.Kind.k_type

let check_ctor_decl ~data_targs env ({ data = ctor; _ } : S.ctor_decl) =
  Uniqueness.check_ctor_named_types data_targs ctor.cd_targs;
  let (env, ctor_targs) = Type.tr_named_type_args env ctor.cd_targs in
  let decl =
    { T.ctor_name        = ctor.cd_name;
      T.ctor_targs;
      T.ctor_named       = List.map (Type.tr_named_scheme env) ctor.cd_named;
      T.ctor_arg_schemes = List.map (Type.tr_scheme env) ctor.cd_arg_schemes
    } in
  (ctor.cd_public, decl)

let check_ctor_decls ~data_targs env ctors =
  Uniqueness.check_ctor_uniqueness ctors;
  List.map (check_ctor_decl ~data_targs env) ctors

let open_data_ctor adt (env, n) (public, (ctor : T.ctor_decl)) =
  let env = Env.add_ctor env ~public ctor.ctor_name n adt in
  (env, n+1)

let open_data env adt ctors =
  List.fold_left (open_data_ctor adt) (env, 0) ctors
  |> fst

let finalize_check env x ~name args ctors =
  let px = Var.fresh ~name () in
  let info = {
    Module.adt_proof = { T.pos = Position.nowhere; T.data = T.EVar px };
    Module.adt_args  = args;
    Module.adt_ctors = List.map snd ctors;
    Module.adt_type  =
      T.Type.t_apps (T.Type.t_var x)
        (List.map (fun (_, x) -> T.Type.t_var x) args)
  } in
  let env = Env.add_data env x info in
  let env = open_data env info ctors in
  let dd =
    T.DD_Data {
      tvar  = x;
      proof = px;
      args  = args;
      ctors = info.adt_ctors
    } in
  (env, dd)
