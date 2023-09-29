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

(** Program *)
type program = expr

(** Check whether given expression is a value *)
let rec is_value (e : expr) =
  match e.data with
  | EUnit | EVar _ | EFn _ -> true
  | EApp _ | ELetV _ | ELetE _ -> false
