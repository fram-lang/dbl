(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for match clauses and related constructs *)

open Common
open TypeCheckFix

(** Check pattern-matching clauses
  In [check_match_clauses env tp cls res_tp res_eff] the parameters have the
  following meaning:
  - [env]     -- an environment
  - [tp]      -- type of the matched expression
  - [cls]     -- list of clauses
  - [res_tp]  -- returned type
  - [res_eff] -- returned effect *)
val check_match_clauses : tcfix:tcfix ->
  Env.t -> T.typ -> S.match_clause list -> T.typ -> T.effrow ->
    T.match_clause list * ret_effect

(* ------------------------------------------------------------------------- *)
(** Check return clauses of handler.
  In [check_return_clauses env rcs res_tp res_eff] the meaning of the
  parameters is the following.
  - [env]     -- the environment.
  - [rcs]     -- list of clauses. If this list is empty, then the default
      identity clause is created.
  - [res_tp]  -- the expected of the return clause
  - [ref_eff] -- the expected effect of the return clause

  This function returns triple: a variable bound by the return clause,
  its type, and the body of the clause (including pattern-matching). *)
val check_return_clauses : tcfix:tcfix ->
  Env.t -> S.match_clause list -> T.typ -> T.effrow ->
    T.var * T.typ * T.expr

(** Check finally clauses of handler.
  In [check_finally_clauses env fcs hexpr htp req eff] the meaning of the
  parameters is the following.
  - [env]   -- the environment.
  - [fcs]   -- list of clauses. If this list is empty, then the equivalent of
      the default identity clause is created.
  - [hexpr] -- the handler expression, to be wrapped around the finally
      clauses.
  - [htp]   -- the type of the handler expression
  - [req]   -- type request of the bidirectional type-checking.
  - [eff]   -- the expected effect of the clauses.

  This function returns a triple with the same meaning as the triple returned
  by [tr_expr] function. Handlers are always impure. *)
val check_finally_clauses : tcfix:tcfix ->
  Env.t -> S.match_clause list -> T.expr -> T.typ ->
    (T.typ, 'dir) request -> T.effrow ->
      T.expr * (T.typ, 'dir) response * ret_effect
