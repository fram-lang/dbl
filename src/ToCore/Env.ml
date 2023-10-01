(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Environment of the translation *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

type t =
  { tvar_map : T.TVar.ex S.TVar.Map.t
  }

let empty =
  { tvar_map = S.TVar.Map.empty
  }

let add_tvar env x =
  let (Ex k) = tr_kind (S.TVar.kind x) in
  let y = T.TVar.Ex (T.TVar.fresh k) in
  { tvar_map = S.TVar.Map.add x y env.tvar_map
  }, y

let lookup_tvar env x =
  try S.TVar.Map.find x env.tvar_map with
  | Not_found ->
    failwith "Internal error: unbound type variable"
