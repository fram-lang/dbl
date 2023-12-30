(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Raw language: result of yacc-generated. It is later post-parsed by
  [Desugar] in order to obtain the program in Surface language. *)

(* Author: Piotr Polesiuk, 2023 *)

include SyntaxNode.Export

(** Type variables *)
type tvar = string

(** Variables *)
type var = string

(** Names of implicit parameters *)
type name = string

(** Names of constructors of ADTs *)
type ctor_name = string

(** Type expressions *)
type type_expr = type_expr_data node
and type_expr_data =
  | TWildcard
    (** A placeholder for a fresh unification variable *)

  | TVar of tvar
    (** Type variable *)

  | TPureArrow of type_expr * type_expr
    (** Pure function: a function without effects, that always terminates *)
  
  | TArrow of type_expr * type_expr * type_expr
    (** Effectful function: the last parameter is an effect *)

  | TEffect of type_expr list * type_expr option
    (** Effect: list of simple effect optionally closed by another effect *)

(** Expressions *)
type expr = expr_data node
and expr_data =
  | EWildcard
    (** Wild-card patterns -- matches everything *)

  | EUnit
    (** Unit value *)

  | EParen of expr
    (** Parentheses *)

  | EVar  of var
    (** Variable *)

  | EName of name
    (** Named implicit parameter *)

  | ECtor of ctor_name
    (** ADT constructor *)

  | EFn   of expr list * expr
    (** Lambda-abstraction *)

  | EApp  of expr * expr
    (** Function application *)

  | EDefs of def list * expr
    (** Local definitions *)

  | EMatch of expr * match_clause list
    (** Pattern-matching *)

  | EHandle of var * expr * h_expr
    (** Effect handler *)

  | ERecord of field list
    (** Record-like expression, e.g., explicit instantiation of named
      parameters *)

(** Pattern-matching clauses *)
and match_clause = match_clause_data node
and match_clause_data =
  | Clause of expr * expr

(** Handler expressions *)
and h_expr = h_expr_data node
and h_expr_data =
  | HEffect of var * var * expr
    (** Handler of a single operation *)

(** Field of record-like expression *)
and field = field_data node
and field_data =
  | FldName of name
    (** Single named implicit parameter *)

  | FldNameVal of name * expr
    (** Named implicit parameter together with a value *)

(** Definitions *)
and def = def_data node
and def_data =
  | DLet of expr * expr
    (** Let-definition *)

  | DImplicit of name
    (** Declaration of implicit parameter *)

  | DData of tvar * ctor_decl list
    (** Definition of ADT *)

(** Declaration of a constructor *)
and ctor_decl = ctor_decl_data node
and ctor_decl_data =
  | CtorDecl of ctor_name * type_expr list
    (** Declaration of a constructor *)

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
