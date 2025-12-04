(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Detection of cycles in named parameter resolution. *)

type t = int Var.Map.t

let empty = Var.Map.empty

let add_var pcyc ?(size=0) x =
  match Var.Map.find_opt x pcyc with
  | None -> Some (Var.Map.add x size pcyc)
  | Some old_size when size < old_size ->
    Some (Var.Map.add x size pcyc)
  | Some _ ->
    None
