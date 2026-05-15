(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Simple incremental SAT-solver *)

type 'a t

(** Create a new solver *)
val create : unit -> 'a t

(** Add a formula in CNF with attached value. *)
val add_cnf : 'a t -> 'a -> (PropVar.t * bool) list list -> unit

(** Add formula implication with attached value. *)
val add_imply : 'a t -> 'a -> Formula.t -> Formula.t -> unit

(** Result of a solver *)
type 'a solve_result =
  | Ok
  | Error of 'a

(** Partially solve collected clauses, trying to quickly fail if the clause
  set is not satisfiable *)
val solve_partial : 'a t -> 'a solve_result

(** Solve all the collected clauses *)
val solve_all : 'a t -> 'a solve_result

(** Pretty print all clauses of the SAT-solver as S-expressions. *)
val clauses_to_sexpr : 'a t -> SExpr.t list
