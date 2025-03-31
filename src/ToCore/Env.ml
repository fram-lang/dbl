(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the translation *)

open Common

type t =
  { tvar_map  : T.TVar.ex S.TVar.Map.t
  }

let initial =
  { tvar_map =
      S.BuiltinType.all
      |> List.map (fun (name, x) -> (x, List.assoc name T.BuiltinType.all))
    |> List.to_seq |> S.TVar.Map.of_seq;
  }

let add_tvar env x =
  let (Ex k) = tr_kind (S.TVar.kind x) in
  let y = T.TVar.Ex (T.TVar.fresh k) in
  { tvar_map = S.TVar.Map.add x y env.tvar_map
  }, y

let add_tvars env xs =
  List.fold_left_map add_tvar env xs

let add_named_tvar env (_, x) =
  add_tvar env x

let add_named_tvars env xs =
  List.fold_left_map add_named_tvar env xs

let lookup_tvar env x =
  try S.TVar.Map.find x env.tvar_map with
  | Not_found ->
    InterpLib.InternalError.report
      ~reason:"unbound type variable"
      ~provided:(S.TVar.to_sexpr x)
      ()
