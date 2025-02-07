(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for match clauses and related constructs *)

open Common
open TypeCheckFix

(** Check pattern-matching clauses
  In [check_match_clauses env tp cls res_tp] the parameters have the
  following meaning:
  - [env]     -- an environment
  - [tp]      -- type of the matched expression
  - [cls]     -- list of clauses
  - [res_tp]  -- returned type *)
val check_match_clauses : tcfix:tcfix ->
  'st Env.t -> T.typ -> S.match_clause list -> T.typ ->
    T.match_clause list * T.effct * Constr.t list

(** Check return clauses of the handler, taking default identity clause if
  given clause list is empty.
  In [tr_opt_clauses ~tcfix ~pos env tp_in cls rtp_req] the parameters have
  the following meaning
  - [env]     -- an environment
  - [tp_in]   -- type of the matched expression
  - [cls]     -- list of clauses: empty list means a single identity clause
  - [rtp_req] -- request for bidirectional type-checking of returned type

  Returns a pair: a variable bound by this return clause, and the translated
  body of the clause. *)
val tr_return_clauses : tcfix:tcfix -> pos:Position.t ->
  'st Env.t -> T.typ -> S.match_clause list -> (T.typ, 'rd) request ->
    T.var * 'rd expr_result

(** Same as [tr_return_clauses], but for finally clauses. *)
val tr_finally_clauses : tcfix:tcfix -> pos:Position.t ->
  'st Env.t -> T.typ -> S.match_clause list -> (T.typ, 'rd) request ->
    T.var * 'rd expr_result
