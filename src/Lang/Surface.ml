(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Surface language: result of parsing and input for type inference *)

(* Author: Piotr Polesiuk, 2023 *)

include SyntaxNode.Export

(** Variables *)
type var = string

(** Expressions *)
type expr = expr_data node
and expr_data =
  | EUnit
    (** Unit expression *)

  | EVar  of var
    (** Variable *)

  | EFn   of var * expr
    (** Lambda abstraction *)

  | EApp  of expr * expr
    (** Application *)

  | EDefs of def list * expr
    (** Local definitions *)

  | EHandle of var * expr * h_expr
    (** Effect handler *)

  | ERepl of (unit -> expr)
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression. *)

  | EReplExpr of expr * expr
    (** Print type, evaluate, and print the first expression, then continue
      to the second one. *)

(** Local definitions *)
and def = def_data node
and def_data =
  | DLetV of var * expr
    (** Let-expression that is subject to generalization *)

  | DLetE of var * expr
    (** Let-expression with possibly impure expression. It does not generalize
      anything *)

(** Handler expressions *)
and h_expr = h_expr_data node
and h_expr_data =
  | HEffect of var * var * expr
    (** Handler of a single operation *)

(** Program *)
type program = expr

(** Check whether given expression is a value *)
let rec is_value (e : expr) =
  match e.data with
  | EUnit | EVar _ | EFn _ -> true
  | EApp _ | EDefs _ | EHandle _ | ERepl _ | EReplExpr _ -> false
