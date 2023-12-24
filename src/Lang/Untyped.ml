(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Untyped language. In A-normal form. *)

(* Author: Piotr Polesiuk, 2023 *)

(** Variables *)
type var = Var.t

(** Expressions *)
type expr =
  | EValue of value
    (** Value, i.e., trivial computation *)

  | ELet of var * expr * expr
    (** Let-expression *)

  | EApp of value * value
    (** Function application *)

  | EHandle of var * expr * h_expr
    (** Handler *)

  | ERepl of (unit -> expr)
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Values *)
and value =
  | VUnit
    (** Unit value *)

  | VVar of var
    (** Variable *)

  | VFn of var * expr
    (** Function *)

  | VCtor of int * value list
    (** Fully applied constructor of ADT *)

(** Handler expressions *)
and h_expr =
  | HEffect of var * var * expr
    (** Handler of effectful functional operation. It stores formal parameter,
      resumption formal parameter, and the body. *)

type program = expr
