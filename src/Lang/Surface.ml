(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Surface language: result of parsing and input for type inference *)

(* Author: Piotr Polesiuk, 2023,2024 *)

include SyntaxNode.Export

(** Type variables *)
type tvar = string

(** Variables *)
type var = string

(** Names of implicit parameters *)
type name = string

(** Name of a ADT constructor *)
type ctor_name = string

(** Identifier *)
type ident =
  | IdVar  of var
  | IdName of name

(** Explicit instantiation. Parametrized by expression representation *)
type 'e inst_data =
  | IName of name * 'e
    (** Explicit instantiation of named implicit parameter *)

(** Type expressions *)
type type_expr = type_expr_data node
and type_expr_data =
  | TWildcard
    (** A placeholder for a fresh unification variable *)

  | TVar of tvar
    (** A (non-unification) type variable *)

  | TPureArrow of scheme_expr * type_expr
    (** Pure function: a function without effects, that always terminates *)

  | TArrow of scheme_expr * type_expr * type_expr
    (** Effectful function: the last parameter is an effect *)

  | TEffect of type_expr list * type_expr option
    (** Effect: list of simple effect optionally closed by another effect *)

  | TApp of type_expr * type_expr
    (** Type application *)

(** Type-scheme expressions *)
and scheme_expr = {
  sch_pos : Position.t;
    (** Location of the scheme expression *)

  sch_tvars : type_arg list;
    (** Type parameters *)

  sch_implicit : implicit_decl list;
    (** Named parameters *)

  sch_body : type_expr
    (** Body of the scheme *)
}

(** Declaration of implicit/named parameter *)
and implicit_decl = scheme_expr inst_data node

(** Type formal parameter *)
and type_arg = type_arg_data node
and type_arg_data =
  | TA_Var of tvar
    (** Type variable *)

(** Declaration of constructor of ADT *)
type ctor_decl = ctor_decl_data node
and ctor_decl_data =
  | CtorDecl of
    ctor_name * type_arg list * implicit_decl list * scheme_expr list
    (** Declaration of constructor of ADT *)

(** Patterns *)
type pattern = pattern_data node
and pattern_data =
  | PWildcard
    (** Wildcard pattern -- it matches everything *)

  | PVar of var
    (** Pattern that binds a variable *)

  | PName of name
    (** Pattern that binds a named implicit *)

  | PCtor of ctor_name node * inst_pattern list * pattern list
    (** ADT constructor pattern *)

  | PAnnot of pattern * scheme_expr
    (** Scheme annotation *)

(** Pattern for named parameter *)
and inst_pattern = pattern inst_data node

(** Formal argument *)
type arg =
  | ArgAnnot of pattern * scheme_expr
    (** Argument with scheme annotation *)

  | ArgPattern of pattern
    (** Argument with pattern-matching *)

(** Implicit formal argument *)
type inst_arg = arg inst_data node

(** Polymorphic expressions *)
type poly_expr = poly_expr_data node
and poly_expr_data =
  | EVar  of var
    (** Variable *)

  | EName of name
    (** Implicit parameter *)

  | ECtor of ctor_name
    (** ADT constructor *)

(** Expressions *)
type expr = expr_data node
and expr_data =
  | EUnit
    (** Unit expression *)

  | EPoly of poly_expr * inst list
    (** Polymorphic expression with patrtial explicit instantiation, possibly
      empty *)

  | EFn   of arg * expr
    (** Lambda abstraction *)

  | EApp  of expr * expr
    (** Application *)

  | EDefs of def list * expr
    (** Local definitions *)

  | EMatch of expr * match_clause list
    (** Pattern-matching *)

  | EHandle of pattern * expr * h_expr
    (** Effect handler *)

  | ERepl of (unit -> expr)
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression. *)

  | EReplExpr of expr * expr
    (** Print type, evaluate, and print the first expression, then continue
      to the second one. *)

(** Explicit instantiation of polymorphic expression *)
and inst = expr inst_data node

(** Local definitions *)
and def = def_data node
and def_data =
  | DLetId of ident * expr
    (** Let definition: monomorphic or polymorphic, depending on effect *)

  | DLetFun of ident * type_arg list * inst_arg list * expr
    (** Polymorphic function definition *)

  | DLetPat  of pattern * expr
    (** Let definition combinded with pattern-matching. Always monomorphic *)

  | DImplicit of name
    (** Declaration of implicit *)

  | DData of tvar * type_arg list * ctor_decl list
    (** Definition of ADT *)

(** Pattern-matching clauses *)
and match_clause = match_clause_data node
and match_clause_data =
  | Clause of pattern * expr

(** Handler expressions *)
and h_expr = h_expr_data node
and h_expr_data =
  | HEffect of var * var * expr
    (** Handler of a single operation *)

(** Program *)
type program = expr
