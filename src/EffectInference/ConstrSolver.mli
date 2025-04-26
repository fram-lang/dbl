(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraint solver. *)

open Common

(** Partially solve subeffecting constraint and add it to the environment. *)
val add_constraint : origin:origin -> Env.t -> T.effct -> T.effct -> unit

(** Leave scope of [tvars] type variables. Use this function only in
  type-checking mode. *)
val leave_scope : env0:Env.t -> tvars:T.tvar list -> Constr.t list -> unit

(** Leave scope of [tvars] type variables, returning a type scheme. The [env0]
  is an outer environment. *)
val leave_scope_with_scheme :
  env0:Env.t -> tvars:T.tvar list -> Constr.t list -> T.scheme -> unit

(** Same as [leave_scope_with_scheme], but take a list of type schemes. *)
val leave_scope_with_schemes :
  env0:Env.t -> tvars:T.tvar list -> Constr.t list -> T.scheme list -> unit

(** Leave scope of [tvars] type variables, returning a type and effect. The
  [env0] is an outer environment. *)
val leave_scope_with_type_eff :
  env0:Env.t -> tvars:T.tvar list -> Constr.t list -> T.typ -> T.ceffect ->
    unit

(** Leave scope of [tvars] type variables, returning a list of constructors.
  The [env0] is an outer environment. *)
val leave_scope_with_ctors :
  env0:Env.t -> tvars:T.tvar list -> Constr.t list -> T.ctor_decl list ->
    unit

(** Leave scope of [tvars] type variables, returning an entity that may
  contains given list of generalizable variable defined at the leaved scope.
  The [env0] is an outer environment. *)
val leave_scope_with_gvars :
  env0:Env.t -> tvars:T.tvar list -> Constr.t list -> T.GVar.Set.t -> unit

(** Leave scope and generalize generalizable variables. Returns generalized
  variables promoted to type variables, and the list of constraints that
  could not be propagated, and should be generalized. There should be no
  regular variables defined at the leaved scope. *)
val generalize_with_scheme :
  env0:Env.t -> Constr.t list -> T.scheme -> T.tvar list * T.constr list

(** Same as [generalize_with_scheme], but takes multiple type schemes. *)
val generalize_with_schemes :
  env0:Env.t -> Constr.t list -> T.scheme list -> T.tvar list * T.constr list

(** Solve all constraints collected in given environment. *)
val final_solve : Env.t -> unit
