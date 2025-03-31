(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Utility functions for expressions. *)

open Common

(** Unit value *)
val mk_unit : T.expr

(** Build a sequence of functions *)
val mk_fns : (T.var * T.scheme) list -> T.expr -> T.expr

(** Build a sequence of functions that take named parameters *)
val mk_named_fns : (T.name * T.var * T.scheme) list -> T.expr -> T.expr

(** Build a sequence of type functions. *)
val mk_tfuns : T.tvar list -> T.expr -> T.expr

(** Build a sequence of named type functions. *)
val mk_named_tfuns : T.named_tvar list -> T.expr -> T.expr

(** Build a sequence of applications. *)
val mk_apps : T.expr -> T.expr list -> T.expr

(** Build a sequence of type applications. *)
val mk_tapps : T.expr -> T.typ list -> T.expr

(** Build an explicit instantiation of a polymorphic expression. *)
val mk_inst : T.expr -> T.typ list -> T.expr list -> T.expr

(** Build instantiation of a large scheme to a simple scheme *)
val mk_linst : T.expr -> T.typ list -> T.expr

(** Generalize extra type and named parameters in an expression of a given
  scheme. *)
val generalize :
  T.named_tvar list -> (T.name * T.var * T.scheme) list ->
    T.expr -> T.scheme -> T.expr * T.scheme

(** Build a generalization of effect variables together with constraints. *)
val generalize_constr : T.tvar list -> T.constr list -> T.expr -> T.expr

(** Build a function that represents a constructor of a given type, assuming
  that it has given list of constructors. *)
val mk_ctor :
  prf:T.expr -> idx:int -> T.typ -> T.ctor_decl list -> T.expr * T.scheme

(** Build a body of a match clause, i.e, a function that takes given type
  parameters, value parameters, and additional unit value, and runs the
  given expression. *)
val mk_clause_body :
  T.tvar list -> (T.var * T.scheme) list -> T.expr -> T.expr

(** Build a handle expression *)
val mk_handle : T.tvar -> T.var -> T.typ -> T.expr -> T.expr -> T.expr

(** Build a first-class handler expression *)
val mk_handler :
  eff_var:T.tvar -> lbl_var:T.var ->
  delim_tp:T.typ -> delim_eff:T.effct ->
  cap_tp:T.typ -> in_tp:T.typ -> in_eff:T.effct ->
  cap_body:T.expr ->
  ret_var:T.var -> ret_body:T.expr ->
  fin_var:T.var -> fin_body:T.expr ->
    unit -> T.expr

(** Context with less-polymorphic versions of recursive definitions. *)
type rec_ctx

(** Build [rec_ctx] from list of all recursive definitions. These definitions
  consist of less-polymorphic variables (those that should be introduced in
  [ERecCtx] construct), more-polymorphic variables (those that are already in
  the context), and the less-polymorphic schemes of recursive functions. *)
val mk_rec_ctx :
  evs:T.tvar list -> cs:T.constr list ->
  targs:T.named_tvar list -> named:(T.name * T.var * T.scheme) list ->
    (T.var * T.var * T.scheme) list -> rec_ctx

(** Update a body of a recursive definitions, by adding extra variable
  bindings described by [rec_ctx] at [ERecCtx] constructs. *)
val update_rec_body : rec_ctx:rec_ctx -> T.expr -> T.expr
