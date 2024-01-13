(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Checking and processing algebraic data types (ADTs) *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

let check_ctor_decl env (ctor : S.ctor_decl) =
  match ctor.data with
  | CtorDecl(name, schs) ->
    { T.ctor_name        = name;
      T.ctor_arg_schemes = List.map (Type.tr_scheme env) schs
    }

let check_ctor_decls env ctors =
  List.map (check_ctor_decl env) ctors

let open_data_ctor adt (env, n) (ctor : T.ctor_decl) =
  let env = Env.add_ctor env ctor.ctor_name n adt in
  (env, n+1)

let open_data env (adt : Env.adt_info) =
  List.fold_left (open_data_ctor adt) (env, 0) adt.adt_ctors
  |> fst
