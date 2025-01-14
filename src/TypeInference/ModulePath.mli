(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving module paths *)

open Common

(** Lookup for given module in the environment *)
val lookup_module : Env.t -> S.module_name S.path -> Module.closed Module.t

(** Lookup for given variable in the environment *)
val lookup_type : Env.t -> S.tvar S.path -> T.typ

(** Lookup for given variable in the environment *)
val lookup_var : Env.t -> S.var S.path -> Module.var_info

(** Lookup for given implicit in the environment *)
val lookup_implicit : Env.t -> S.iname S.path -> T.var * T.scheme

(** Lookup for given constructor in the environment. Returns the index of the
  constructor and the ADT. *)
val lookup_ctor : Env.t -> S.ctor_name S.path -> int * Module.adt_info

(** Lookup for ADT in the environment. The path to constructor indicates the
  module, where the ADT should be found. If the path is empty, the ADT is
  searched in the whole environment. *)
val lookup_adt :
  Env.t -> S.ctor_name S.path -> T.tvar -> Module.adt_info option
