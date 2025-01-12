(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Trees of modules for pretty-printing of types. *)

type pp_module = unit

type t = unit

type pp_result =
  | Found   of string
  | Anon    of string * Position.t option
  | Unbound of string

let empty = ()

let add ~public ?pos pp_tree x uid = ()

let add_anon ?pos ?name pp_tree uid = ()

let enter_module pp_tree = ()

let leave_module ~public pp_tree name = ((), ())

let open_module ~public pp_tree pp_module = ()

let lookup pp_tree x =
  Unbound ("<Unbound" ^ UID.to_string x ^ ">")
