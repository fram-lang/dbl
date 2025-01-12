(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Trees of modules for pretty-printing of types. *)

type t = unit

type pp_result =
  | Found   of string
  | Anon    of string * Position.t option
  | Unbound of string

let empty = ()

let add ?public ?pos pp_tree x uid = ()

let lookup pp_tree x =
  Unbound ("<Unbound" ^ UID.to_string x ^ ">")
