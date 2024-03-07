(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Untyped language. In A-normal form. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

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

  | EMatch of value * clause list
    (** Pattern-matching *)

  | ELabel of var * expr
    (** Generating fresh runtime label *)

  | EShift of value * var * expr
    (** Shift-0 operator at given runtime label *)

  | EReset of value * expr * var * expr
    (** Shift-0 operator at given runtime label and with a return clause *)

  | ERepl of (unit -> expr)
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Values *)
and value =
  | VNum of int
    (** Integer literal *)

  | VStr of string
    (** String literal *)

  | VVar of var
    (** Variable *)

  | VFn of var * expr
    (** Function *)

  | VCtor of int * value list
    (** Fully applied constructor of ADT *)

  | VExtern of string

(** Pattern-matching clause *)
and clause = var list * expr

type program = expr
