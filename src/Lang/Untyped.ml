(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Untyped language. Similarly to Core, it is in a slightly relaxed A-normal
  form. *)

(** Variables *)
type var = Var.t

(** Literals *)
type lit =
  | LNum of int
    (** Integer literal *)

  | LNum64 of int64
    (** 64 bit integer literal *)

  | LStr of string
    (** String literal *)

(** Expressions *)
type expr =
  | EValue of value
    (** Value, i.e., trivial computation *)

  | ELet of var * expr * expr
    (** Let-expression *)

  | ELetRec of (var * expr) list * expr
    (** Mutually recursive let-definitions *)

  | EFn of var * expr
    (** Function *)

  | EApp of expr * value
    (** Function application *)

  | ECtor of int * value list
    (** Fully applied constructor of ADT *)

  | EMatch of value * clause list
    (** Pattern-matching *)

  | ELabel of var * expr
    (** Generating fresh runtime label *)

  | EShift of value * var list * var * expr
    (** Shift-0 operator at given runtime label (the first parameter). The
      second parameter is a list of variables that represents values stored at
      the delimiter. The third parameter is a continuation variable. *)

  | EReset of value * value list * expr * var * expr
    (** Reset-0 operator at given runtime label (the first parameter) and
      which stores a list of values (the second parameter). The last two
      parameters represent a return clause *)

  | ERepl of (unit -> expr)
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the string produced
      by the first expression and then continue with the last expression. *)
  | EReplDir of (unit -> unit) * expr

(** Trivial values, with minimal cost of copying. *)
and value =
  | VLit of lit
    (** Literal *)

  | VVar of var
    (** Variable *)

  | VExtern of string
    (** Externally defined value *)

(** Pattern-matching clause *)
and clause = var list * expr

type program = expr
