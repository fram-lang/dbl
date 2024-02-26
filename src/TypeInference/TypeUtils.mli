(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility function for extracting information about types *)

(* 2024: Piotr Polesiuk: initial implementation *)

open Common

(** Extract the head type variable of the self typ of given method scheme *)
val method_owner_of_scheme : pos:Position.t -> env:Env.t -> T.scheme -> T.tvar
