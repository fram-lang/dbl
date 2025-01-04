(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving module paths *)

open Common

(** Lookup for given module in the environment *)
val lookup_module : Env.t -> S.module_name S.path -> Module.t

(** Lookup for given variable in the environment *)
val lookup_var : Env.t -> S.var S.path -> Module.var_info

(** Lookup for given implicit in the environment *)
val lookup_implicit : Env.t -> S.iname S.path -> T.var * T.scheme
