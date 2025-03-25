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
(** Check pattern-matching clauses, taking default identity clause if given
  clause list is empty. This function is used for checking return and finally
  clauses of a handler.
  In [tr_opt_clauses ~tcfix ~pos env mtp_req cls rtp_req eff] the parameters
  have the following meaning
  - [env]     -- an environment
  - [mtp_req] -- request for bidirectional type-checking of matched expression
  - [cls]     -- list of clauses: empty list means single identity clause
  - [rtp_req] -- request for bidirectional type-checking of returned type
  - [eff]     -- returned effect

  The [on_error] parameter is called, when requested types (both in
  check-mode) do not match in case of empty clause list.

  Returns the tuple: a variable bound by this return/finally clause,
  bidirectional response of its type, the body of the clause, and response
  with the type of the body. *)
val tr_opt_clauses : tcfix:tcfix -> pos:Position.t ->
  Env.t -> (T.typ, 'md) request ->
  S.match_clause list -> (T.typ, 'rd) request -> T.effrow ->
  on_error:(pos:Position.t -> Error.t) ->
    T.var * (T.typ, 'md) response * T.expr * (T.typ, 'rd) response
