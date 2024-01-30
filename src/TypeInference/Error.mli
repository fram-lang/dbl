(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to type-inference. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

(** Abstract representation of error *)
type t

(** Report fatal error and abort the compilation *)
val fatal : t -> 'a

(** Report non-fatal error *)
val report : t -> unit

(** Report a warning *)
val warn : t -> unit

val kind_mismatch : pos:Position.t -> T.kind -> T.kind -> t

val type_not_function : pos:Position.t -> env:Env.t -> T.kind -> t

val unbound_var : pos:Position.t -> S.var -> t
val unbound_implicit : pos:Position.t -> S.iname -> t
val unbound_constructor : pos:Position.t -> S.ctor_name -> t
val unbound_type_var : pos:Position.t -> S.tvar -> t
val unbound_named_param : pos:Position.t -> S.var -> t
val unbound_the_label : pos:Position.t -> t

val expr_type_mismatch   : pos:Position.t -> env:Env.t -> T.typ -> T.typ -> t
val expr_effect_mismatch :
  pos:Position.t -> env:Env.t -> T.effect -> T.effect -> t

val pattern_type_mismatch : pos:Position.t -> env:Env.t -> T.typ -> T.typ -> t
val pattern_annot_mismatch :
  pos:Position.t -> env:Env.t -> T.scheme -> T.scheme -> t

val func_effect_mismatch :
  pos:Position.t -> env:Env.t -> T.effect -> T.effect -> t

val func_not_pure : pos:Position.t -> t
val impure_handler : pos:Position.t -> t

val expr_not_function     : pos:Position.t -> env:Env.t -> T.typ -> t
val expr_not_function_ctx : pos:Position.t -> env:Env.t -> T.typ -> t

val expr_not_handler : pos:Position.t -> env:Env.t -> T.typ -> t
val expr_not_handler_ctx : pos:Position.t -> env:Env.t -> T.typ -> t

val empty_match_on_non_adt : pos:Position.t -> env:Env.t -> T.typ -> t
val empty_match_on_nonempty_adt : pos:Position.t -> env:Env.t -> T.typ -> t

val type_escapes_its_scope : pos:Position.t -> env:Env.t -> T.tvar -> t

val ungeneralizable_implicit : pos:Position.t -> S.iname -> t
val non_polymorphic_pattern : pos:Position.t -> t

val looping_named_param : pos:Position.t -> T.name -> t

val named_param_type_mismatch :
  pos:Position.t -> env:Env.t -> T.name -> T.typ -> T.typ -> t

val ctor_redefinition :
  pos:Position.t -> ppos:Position.t -> S.ctor_name -> t

val type_inst_redefinition :
  pos:Position.t -> ppos:Position.t -> S.tname -> t

val inst_redefinition :
  pos:Position.t -> ppos:Position.t -> S.name -> t

val multiple_named_type_args :
  pos:Position.t -> ppos:Position.t -> S.tname -> t

val ctor_type_arg_same_as_data_arg : pos:Position.t -> S.tname -> t

val multiple_inst_patterns :
  pos:Position.t -> ppos:Position.t -> S.name -> t

val multiple_name_binders :
  pos1:Position.t -> pos2:Position.t -> T.name -> t

val ctor_arity_mismatch :
  pos:Position.t -> S.ctor_name -> int -> int -> t

val redundant_named_type : pos:Position.t -> T.tname -> t
val redundant_named_parameter : pos:Position.t -> T.name -> t
val redundant_named_pattern : pos:Position.t -> T.name -> t
