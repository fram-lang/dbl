(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value

type value_box = value option ref

type t = value_box Var.Map.t

let empty = Var.Map.empty

let extend env x v =
  Var.Map.add x (ref (Some v)) env

let extend_box env x =
  let box = ref None in
  (Var.Map.add x box env, box)

let update_box box v =
  match !box with
  | None -> box := Some v
  | Some _ -> failwith "Runtime error: recursive value updated twice"

let lookup env x =
  match !(Var.Map.find x env) with
  | Some v -> v
  | None   -> failwith "Runtime error: non-productive recursive definition"
