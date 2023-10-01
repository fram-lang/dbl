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

(** Values *)
and value =
  | VUnit
    (** Unit value *)

  | VVar of var
    (** Variable *)

  | VFn of var * expr
    (** Function *)

type program = expr
