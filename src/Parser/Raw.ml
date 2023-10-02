(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Raw language: result of yacc-generated. It is later post-parsed by
  [Desugar] in order to obtain the program in Surface language. *)

(* Author: Piotr Polesiuk, 2023 *)

include SyntaxNode.Export

(** Variables *)
type var = string

(** Expressions *)
type expr = expr_data node
and expr_data =
  | EUnit
    (** Unit value *)

  | EParen of expr
    (** Parentheses *)

  | EVar  of var
    (** Variable *)

  | EFn   of var * expr
    (** Lambda-abstraction *)

  | EApp  of expr * expr
    (** Function application *)

  | EDefs of def list * expr
    (** Local definitions *)

(** Definitions *)
and def = def_data node
and def_data =
  | DLet of var * expr
    (** Let-definition *)

(** Program *)
type program = def list node

(** REPL command *)
type repl_cmd =
  | REPL_Exit
    (** Exit the REPL *)

  | REPL_Expr of expr
    (** Evaluate given expression *)
  
  | REPL_Def  of def
    (** Provide a new definition in a REPL session *)
