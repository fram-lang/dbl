(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility function for extracting information about types *)

open Common
(*
(** Check if type is a neutral type starting with type variable *)
val is_tvar_neutral : T.typ -> bool
*)
(** Extract the head type variable of the self type of given method scheme *)
val method_owner_of_scheme : pos:Position.t -> env:Env.t -> T.scheme -> T.tvar
