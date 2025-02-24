(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for recursive definitions *)

open Common
open TypeCheckFix

(** The result of checking a block of mutually-recursive definitions. *)
type 'st rec_result =
  { rec_env    : ('st, sec) opn Env.t;
    (** The environment after checking the definitions. *)

    rec_dds    : T.data_def list;
    (** The data definitions in the block. *)

    rec_targs  : T.named_tvar list;
    (** Type parameters common to all definitions. *)

    rec_named  : (T.name * T.var * T.scheme_expr) list;
    (** Named parameters common to all definitions. *)

    rec_fds    : T.rec_def list;
    (** The recursive definitions in the block. *)

    rec_eff    : T.effct;
    (** The effect of the definitions. It might be impure, because of
      generating fresh labels. *)

    rec_constr : Constr.t list
    (** The constraints that were generated. *)
  }

(** Check a block of mutually-recursive definitions. *)
val check_rec_defs : tcfix:tcfix -> pos:Position.t ->
  ('st, sec) opn Env.t -> S.def list -> 'st rec_result
