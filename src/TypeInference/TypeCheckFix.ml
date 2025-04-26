(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Signatures of mutually recursive functions defined across several
  modules. *)

open Common
open BiDirectional

(** Result of bidirectional type checking of expressions. *)
type 'dir expr_result =
  { er_expr   : T.expr;
      (** Translated expression. *)

    er_type   : (T.typ, 'dir) response;
      (** Type of the expression. *)

    er_effect : T.effct;
      (** Effect of the expression. *)

    er_constr : Constr.t list;
      (** Constraints generated during type checking. *)
  }

(** Get the type of the expression from the type-checking result in a [infer]
  mode. *)
let expr_result_type (er : infer expr_result) : T.typ =
  let (Infered tp) = er.er_type in tp

(** Type of continuation used to type-checking of definitions. It is defined
  as record, in order to allow it to be polymorphic in a direction of type
  checking. *)
type 'st def_cont =
  { run : 'dir.
      ('st, sec) opn Env.t -> (T.typ, 'dir) request -> 'dir expr_result
  }

(** Unfortunately, OCaml does not support mutually recursive modules defined
  in several files. For this reason, some functions of the type-checker take
  as a parameter a module with mutually recursive functions. The variant of
  these functions with this additional module parameter can be defined in
  separate modules, but at some point, they should be packed into single
  recursive module, passed as the first parameter for them. See
  [TypeInference.Main] for this recursive definition. *)
module type TCFix = sig
  (** Infer the type of an expression. When the expression is applied to some
    arguments, the [?app_type] parameter, if provided, specifies the type of
    the application. *)
  val infer_expr_type :
    ?app_type:T.typ -> 'st Env.t -> S.expr -> infer expr_result

  (** Check the type of an expression. *)
  val check_expr_type :
    'st Env.t -> S.expr -> T.typ -> check expr_result

  (** Check the type of a single definition. It uses bidirectional type
    checking, and passes the extended environment to the body-generating
    continuation. *)
  val check_def :
    ('st, sec) opn Env.t -> S.def -> (T.typ, 'dir) request ->
      'st def_cont -> 'dir expr_result

  (** Check the type of a block of definitions. It uses bidirectional type
    checking, and passes the extended environment to the body-generating
    continuation. *)
  val check_defs :
    ('st, sec) opn Env.t -> S.def list -> (T.typ, 'dir) request ->
      'st def_cont -> 'dir expr_result
end

type tcfix = (module TCFix)
