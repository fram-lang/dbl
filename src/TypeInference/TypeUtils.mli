(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility function for extracting information about types *)

(* 2024: Piotr Polesiuk: initial implementation *)

open Common

(** Extract the head type variable of the self type of given method scheme *)
val method_owner_of_scheme : pos:Position.t -> env:Env.t -> T.scheme -> T.tvar

(** Get type instantiation hints, by matching the method scheme with the
  actual type of the "self" value. Returned hints are a partial map from
  type variables bound by the scheme to types. It assumes that the scheme
  is valid for given self type, i.e., is a function scheme, where the
  argument is monomorphic neutral type with the same head as the self type. *)
val method_inst_hints : T.scheme -> T.typ -> T.typ T.TVar.Map.t
