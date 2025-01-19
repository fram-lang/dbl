(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Resolving module paths *)

open Common

(** Lookup for given module in the environment *)
val lookup_module :
  'st Env.t -> S.module_name S.path -> closed Module.t

(** Lookup for given variable in the environment *)
val lookup_type : 'st Env.t -> S.tvar S.path -> T.typ

(** Lookup for given variable in the environment *)
val lookup_var : 'st Env.t -> S.var S.path -> T.poly_expr * T.scheme

(** Lookup for given implicit in the environment *)
val lookup_implicit : 'st Env.t -> S.iname S.path -> T.poly_expr * T.scheme

(** Lookup for given constructor in the environment. Returns the index of the
  constructor and the ADT. *)
val lookup_ctor : 'st Env.t -> S.ctor_name S.path -> int * Module.adt_info

(** Lookup for ADT in the environment. The path to constructor indicates the
  module, where the ADT should be found. If the path is empty, the ADT is
  searched in the whole environment. *)
val lookup_adt :
  'st Env.t -> S.ctor_name S.path -> T.tvar -> Module.adt_info option

(* ========================================================================== *)
(** Lookup for a type in the environment. Returns [None] if the type is not
  bound. For other exceptional situations raises an error. *)
val try_lookup_tvar :
  pos:Position.t -> 'st Env.t -> S.tvar -> T.typ option

(** Lookup for a type in the module. Returns [None] if the type is not bound.
  For other exceptional situations raises an error. *)
val try_lookup_tvar_in_module :
  pos:Position.t -> 'st Env.t -> closed Module.t -> S.tvar -> T.typ option

(** Lookup for a value in the environment. Returns [None] if the value is not
  bound. For other exceptional situations raises an error. *)
val try_lookup_val :
  pos:Position.t -> 'st Env.t -> Name.t -> (T.poly_expr * T.scheme) option

(** Lookup for a value in the module. Returns [None] if the value is not bound.
  For other exceptional situations raises an error. *)
val try_lookup_val_in_module :
  pos:Position.t -> 'st Env.t -> closed Module.t -> Name.t ->
    (T.poly_expr * T.scheme) option

(** Lookup for the implicit value and extract a variable and its scheme.
  Returns [None] if the implicit is not bound. For other exceptional situations
  raises an error. *)
val try_lookup_implicit :
  pos:Position.t -> 'st Env.t -> S.iname -> (T.var * T.scheme) option

(** Lookup for implicit "label" variable and extract its unif representation.
  Returns [None] if the label is not bound. For other exceptional situations
  raises an error. *)
val try_lookup_the_label :
  pos:Position.t -> 'st Env.t -> (T.var * T.scheme) option

(** Lookup for a method and extract a variable and its scheme.
  Returns [None] if the method is not bound. For other exceptional situations
  raises an error. *)
val try_lookup_method :
  pos:Position.t -> 'st Env.t -> Name.method_owner -> S.method_name ->
    (T.var * T.scheme) option
