(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Reporting errors related to type-inference. *)

open Common

(** Abstract representation of errors *)
type t

(** Report fatal error and abort the compilation *)
val fatal : t -> 'a

(** Report non-fatal error *)
val report : t -> unit

(** Report a warning *)
val warn : t -> unit

val check_unify_result :
  ?is_fatal:bool -> pos:Position.t -> Unification.result ->
  on_error:(pos:Position.t -> t) -> unit

val kind_mismatch : pos:Position.t -> T.kind -> T.kind -> t
val named_type_kind_mismatch :
  pos:Position.t -> S.tvar -> T.kind -> T.kind -> t

val effect_arrow_kind : pos:Position.t -> t

val kind_annot_mismatch : pos:Position.t -> T.kind -> T.kind -> t

val type_not_function : pos:Position.t -> T.kind -> t

val unbound_var : S.var S.path -> t
val unbound_implicit : S.iname S.path -> t
val unbound_ctor : S.ctor_name S.path -> t
val unbound_type : S.tvar S.path -> t
val unbound_module : S.module_name S.path -> t
val unbound_the_label : pos:Position.t -> t 

val unbound_method :
  pos:Position.t -> pp:PPTree.t -> Name.method_owner -> S.method_name -> t

val expr_type_mismatch : pos:Position.t -> pp:PPTree.t -> T.typ -> T.typ -> t

val pattern_type_mismatch :
  pos:Position.t -> pp:PPTree.t -> T.typ -> T.typ -> t
val pattern_annot_mismatch :
  pos:Position.t -> pp:PPTree.t -> T.scheme -> T.scheme -> t

val return_type_mismatch :
  pos:Position.t -> pp:PPTree.t -> T.typ -> T.typ -> t
val finally_type_mismatch :
  pos:Position.t -> pp:PPTree.t -> T.typ -> T.typ -> t

val repl_show_type_mismatch :
  pos:Position.t -> pp:PPTree.t -> self_tp:T.typ -> T.typ -> t

val func_not_total : pos:Position.t -> t
val handler_not_total : pos:Position.t -> t
val expr_not_total : pos:Position.t -> t

val invalid_rec_def : pos:Position.t -> t
val non_productive_rec_def : pos:Position.t -> t

val expr_not_function     : pos:Position.t -> pp:PPTree.t -> T.typ -> t
val expr_not_function_ctx : pos:Position.t -> pp:PPTree.t -> T.typ -> t

val expr_not_handler : pos:Position.t -> pp:PPTree.t -> T.typ -> t
val expr_not_handler_ctx : pos:Position.t -> pp:PPTree.t -> T.typ -> t

val expr_not_label : pos:Position.t -> pp:PPTree.t -> T.typ -> t
val wrong_label_type : pos:Position.t -> pp:PPTree.t -> T.typ -> t

val polymorphic_label : pos:Position.t -> t

val method_of_bound_tvar : pos:Position.t -> pp:PPTree.t -> T.scheme -> t
val method_of_unknown_type : pos:Position.t -> pp:PPTree.t -> T.scheme -> t
val method_of_polymorphic_type : pos:Position.t -> pp:PPTree.t -> T.scheme -> t
val non_arrow_method : pos:Position.t -> pp:PPTree.t -> T.scheme -> t

val method_call_on_unknown_type : pos:Position.t -> t

val ctor_pattern_on_non_adt : pos:Position.t -> pp:PPTree.t -> T.typ -> t
val empty_match_on_non_adt : pos:Position.t -> pp:PPTree.t -> T.typ -> t
val empty_match_on_nonempty_adt : pos:Position.t -> pp:PPTree.t -> T.typ -> t

val ctor_not_in_type :
  pos:Position.t -> pp:PPTree.t -> S.ctor_name -> T.typ -> t

val ungeneralizable_type_param :
  pos:Position.t -> decl_pos:Position.t -> T.tname -> t
val ungeneralizable_named_param :
  pos:Position.t -> decl_pos:Position.t -> pp:PPTree.t -> Name.t -> t

val rejected_type_param_used :
  pos:Position.t -> decl_pos:Position.t -> T.tname -> t
val rejected_named_param_used :
  pos:Position.t -> decl_pos:Position.t -> pp:PPTree.t -> Name.t -> t

val non_polymorphic_pattern : pos:Position.t -> t

val polymorphic_optional_parameter : pos:Position.t -> t

val anonymous_type_pattern : pos:Position.t -> t

val looping_named_param : pos:Position.t -> pp:PPTree.t -> Name.t -> t

val named_param_type_mismatch :
  pos:Position.t -> pp:PPTree.t -> Name.t -> T.typ -> T.typ -> t

val ctor_redefinition :
  pos:Position.t -> ppos:Position.t -> S.ctor_name -> t

val cannot_resolve_named_param : pos:Position.t -> S.var -> t
val cannot_resolve_implicit : pos:Position.t -> S.iname -> t
val cannot_resolve_method :
  pos:Position.t -> pp:PPTree.t -> Name.method_owner -> S.method_name -> t
val cannot_resolve_method_constr :
  pos:Position.t -> pp:PPTree.t -> S.method_name -> T.scheme -> t

val type_already_provided : pos:Position.t -> npos:Position.t -> S.tvar -> t
val named_param_already_provided :
  pos:Position.t -> npos:Position.t -> pp:PPTree.t -> Name.t -> t
val method_already_provided :
  pos:Position.t -> npos:Position.t -> S.method_name -> t
val named_param_provided_as_optional : pos:Position.t -> S.var -> t
val ambiguous_method_inst : pos:Position.t -> S.method_name -> t
val module_inst_after_method_inst : pos:Position.t -> t

val unknown_named_type_pattern : pos:Position.t -> S.tvar -> t
val unknown_named_pattern : pos:Position.t -> pp:PPTree.t -> Name.t -> t
val multiple_named_patterns :
  pos:Position.t -> ppos:Position.t -> pp:PPTree.t -> Name.t -> t
val method_pattern_not_allowed : pos:Position.t -> t
val open_pattern_not_allowed : pos:Position.t -> t

val duplicate_type_in_pattern :
  pos:Position.t -> ppos:Position.t -> S.tvar -> t
val duplicate_val_in_pattern :
  pos:Position.t -> ppos:Position.t -> pp:PPTree.t -> Name.t -> t
val duplicate_module_in_pattern :
  pos:Position.t -> ppos:Position.t -> S.module_name -> t

val multiple_named_type_args :
  pos:Position.t -> ppos:Position.t -> S.tvar -> t

val multiple_named_args :
  pos:Position.t -> ppos:Position.t -> pp:PPTree.t -> Name.t -> t

val generalized_type_clash : pos:Position.t -> S.tvar -> t
val generalized_name_clash : pos:Position.t -> pp:PPTree.t -> Name.t -> t

val ctor_type_arg_same_as_data_arg : pos:Position.t -> S.tvar -> t

val ctor_arity_mismatch :
  pos:Position.t -> S.ctor_name S.path -> int -> int -> t

val redundant_named_type : pos:Position.t -> S.tvar -> t
val redundant_named_parameter :
  pos:Position.t -> pp:PPTree.t -> Name.t -> t
val redundant_method_parameter : pos:Position.t -> S.method_name -> t
