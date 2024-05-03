(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of datatype definitions from Unif to Core *)

open Common

(** Translate a constructor declaration *)
let tr_ctor_decl env (ctor : S.ctor_decl) =
  let (env, tvars) =
    List.fold_left_map Env.add_named_tvar env ctor.ctor_targs in
  { T.ctor_name      = ctor.ctor_name;
    T.ctor_tvars     = tvars;
    T.ctor_arg_types =
      List.map (fun (_, sch) -> Type.tr_scheme env sch) ctor.ctor_named @
      List.map (Type.tr_scheme env) ctor.ctor_arg_schemes
  }

(** Translate a list of constructor declarations *)
let tr_ctor_decls env decls =
  List.map (tr_ctor_decl env) decls

let prepare_data_def env (dd : S.data_def) =
  let (env, x) = Env.add_tvar env dd.dd_tvar in
  (env, (x, dd))

let finalize_data_def env (x, (dd : S.data_def)) =
  let (env, args) = List.fold_left_map Env.add_named_tvar env dd.dd_args in
  let ctors = tr_ctor_decls env dd.dd_ctors in
  T.DD_Data {
    tvar  = x;
    proof = dd.dd_proof;
    args  = args;
    ctors = ctors
  }

let tr_data_defs env dds =
  let (env, dds) = List.fold_left_map prepare_data_def env dds in
  (env, List.map (finalize_data_def env) dds)
