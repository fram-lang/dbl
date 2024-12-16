(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Signatures of mutually recursive functions defined across several
  modules. *)
(*
open Common

(** Type of continuation used to type-checking of definitions. It is defined
  as record, in order to allow it to be polymorphic in a direction of type
  checking. *)
type def_cont =
  { run : 'dir.
      Env.t -> ImplicitEnv.t -> (T.typ, 'dir) request -> T.effrow ->
        T.expr * (T.typ, 'dir) response * ret_effect
  }

(** Unfortunately, OCaml does not support mutually recursive modules defined
  in several files. For this reason, some functions of the type-checker takes
  as a parameter a module with mutually recursive functions. The variant of
  these functions with this additional module parameter can be defined in
  separate modules, but at some point, they should be packed into single
  recursive module, passed as the first parameter for them. See
  [TypeInference.Main] for this recursive definition. *)
module type TCFix = sig
  (** Infer type of an expression. The effect of an expression is always in
    the check mode. However, pure expressions may returns an information that
    they are pure (see [ret_effect] type). *)
  val infer_expr_type :
    Env.t -> S.expr -> T.effrow -> T.expr * T.typ * ret_effect

  (** Check type and effect of an expression. Returns also information about
    the purity of an expression. *)
  val check_expr_type :
    Env.t -> S.expr -> T.typ -> T.effrow -> T.expr * ret_effect

  (** Check type and effect of a single definition. It uses bidirectional
    type checking, and pass the extended environment to the body-generating
    continuation. *)
  val check_def :
    Env.t -> ImplicitEnv.t -> S.def ->
      (T.typ, 'dir) request -> T.effrow -> def_cont ->
        T.expr * (T.typ, 'dir) response * ret_effect

  (** Check type and effect of a block of definitions. It uses bidirectional
    type checking, and pass the extended environment to the body-generating
    continuation. *)
  val check_defs :
    Env.t -> ImplicitEnv.t -> S.def list ->
      (T.typ, 'dir) request -> T.effrow -> def_cont ->
        T.expr * (T.typ, 'dir) response * ret_effect
end

type tcfix = (module TCFix)
*)
