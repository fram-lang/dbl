(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Surface language: result of parsing and input for type inference *)

(* Author: Piotr Polesiuk, 2023 *)

include SyntaxNode.Export

(** Type variables *)
type tvar = string

(** Variables *)
type var = string

(** Names of implicit parameters *)
type name = string

(** Name of a ADT constructor *)
type ctor_name = string

(** Type expressions *)
type type_expr = type_expr_data node
and type_expr_data =
  | TWildcard
    (** A placeholder for a fresh unification variable *)

  | TVar of tvar
    (** A (non-unification) type variable *)

  | TPureArrow of type_expr * type_expr
    (** Pure function: a function without effects, that always terminates *)

  | TArrow of type_expr * type_expr * type_expr
    (** Effectful function: the last parameter is an effect *)

  | TEffect of type_expr list * type_expr option
    (** Effect: list of simple effect optionally closed by another effect *)

(** Declaration of constructor of ADT *)
type ctor_decl = ctor_decl_data node
and ctor_decl_data =
  | CtorDecl of ctor_name * type_expr list
    (** Declaration of constructor of ADT *)

(** Expressions *)
type expr = expr_data node
and expr_data =
  | EUnit
    (** Unit expression *)

  | EVar  of var
    (** Variable *)

  | EName of name
    (** Implicit parameter *)

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
  | DLet of var * expr
    (** Let definition *)

  | DLetName of name * expr
    (** Providing a named implicit *)

  | DImplicit of name
    (** Declaration of implicit *)

  | DData of tvar * ctor_decl list
    (** Definition of ADT *)

(** Handler expressions *)
and h_expr = h_expr_data node
and h_expr_data =
  | HEffect of var * var * expr
    (** Handler of a single operation *)

(** Program *)
type program = expr
