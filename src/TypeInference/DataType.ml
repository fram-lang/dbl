(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

let check_ctor_decl env (ctor : S.ctor_decl) =
  match ctor.data with
  | CtorDecl(name, tps) ->
    { T.ctor_name      = name;
      T.ctor_arg_types = List.map (Type.tr_ttype env) tps
    }

let check_ctor_decls env ctors =
  List.map (check_ctor_decl env) ctors

let open_data_ctor tp proof (env, n) (ctor : T.ctor_decl) =
  let ctor_info =
    { Env.ci_name      = ctor.ctor_name;
      Env.ci_index     = n;
      Env.ci_proof     = proof;
      Env.ci_arg_types = ctor.ctor_arg_types;
      Env.ci_type      = tp
    }
  in
  let env = Env.add_ctor env ctor.ctor_name ctor_info in
  (env, n+1)

let open_data env tp proof ctors =
  List.fold_left (open_data_ctor tp proof) (env, 0) ctors
  |> fst
