(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility function for extracting information about types *)

open Common

(** Check if type is a neutral type starting with type variable *)
val is_tvar_neutral : T.typ -> bool

(** Extract the head type variable of the self type of given method scheme *)
val method_owner_of_scheme : pos:Position.t -> env:Env.t -> T.scheme -> T.tvar

(** Introduce all variables and named parameters, bound by given scheme, to the
  environment. Returns extended environment, list of type variables, list of 
  introduced named parameters, and the type of the scheme body. *)
val open_scheme : pos:Position.t -> Env.t -> T.scheme ->
  Env.t * T.named_tvar list * (T.name * T.var * T.scheme) list * T.typ
