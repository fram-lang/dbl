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

  | ELetV of var * expr * expr
    (** Let-expression that is subject to generalization *)

  | ELetE of var * expr * expr
    (** Let-expression with possibly impure expression. It does not generalize
      anything *)

  | ERepl of (unit -> expr)
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression. *)

  | EReplExpr of expr * expr
    (** Print type, evaluate, and print the first expression, then continue
      to the second one. *)

(** Program *)
type program = expr

(** Check whether given expression is a value *)
let rec is_value (e : expr) =
  match e.data with
  | EUnit | EVar _ | EFn _ -> true
  | EApp _ | ELetV _ | ELetE _ | ERepl _ | EReplExpr _ -> false
