(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value

type t = value Var.Map.t ref

let empty = ref Var.Map.empty

let extend env x v =
  ref (Var.Map.add x v !env)

let lookup env x =
  Var.Map.find x !env

let begin_fix  env = ref !env
let update_fix env fix = fix := !env

