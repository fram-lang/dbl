(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of patterns to the internal representation. *)

open Common

(** Partial environment: the variables bound by the pattern. *)
module PEnv : sig
  type t

  (** Add all variables from the partial environment to the environment.
    Variables are refreshed in order to have correct scope information.
    Returns the new environment, the list of refreshed type variables, and the
    list of variables together with their type schemes. *)
  val open_penv : Env.t -> t -> Env.t * T.tvar list * (T.var * T.scheme) list

  (** Get the type variables introduced by the pattern. Returns the list of
    type variables in the same order as those returned by [open_penv], but
    without refreshing them. *)
  val tvars : t -> T.tvar list

  (** Get the variables introduced by the pattern. Returns the list of
    variables in the same order as those returned by [open_penv], but
    without refreshing them. *)
  val vars : t -> T.var list

  (** Check if the partial environment introduces any existential types. *)
  val has_existential : t -> bool
end

(** Internal representation of polymorphic patterns. *)
type t =
  | PWildcard
  | PAs       of t * T.var
  | PCtor     of
      { name  : string;
        idx   : int;
        proof : T.expr;
        ctors : T.ctor_decl list; (** Full list of constructors of this ADT *)
        tvars : T.tvar list;
        named : (T.name * t) list;
        args  : t list
      }
  | POr       of t * t

(** Translate a pattern of a given type *)
val check_type : Env.t -> S.pattern -> T.typ -> t * PEnv.t

(** Translate a polymorphic pattern of a given scheme *)
val check_scheme : Env.t -> S.pattern -> T.scheme -> t * PEnv.t
