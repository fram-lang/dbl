(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Raw language: result of yacc-generated. It is later post-parsed by
  [Desugar] in order to obtain the program in Surface language. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

include SyntaxNode.Export

(** Type variables *)
type tvar = string

(** Variables *)
type var = string

(** Names of implicit parameters *)
type iname = string

(** Name of a named parameter *)
type name = Lang.Surface.name =
  | NLabel
  | NVar      of var
  | NImplicit of iname

(** Names of constructors of ADTs *)
type ctor_name = string

(** Names of methods *)
type method_name = string

(** Field of record-like, e.g., scheme name parameters, or explicit
  instantiation *)
type ('tp, 'e) field_data =
  | FldAnonType of 'tp
    (** Anonymous type *)

  | FldEffect
    (** Effect associated with effect handler *)

  | FldEffectVal of 'tp
    (** Effect associated with effect handler, together with its value *)

  | FldType of tvar
    (** Named type *)

  | FldTypeVal of tvar * 'tp
    (** Named type with a value *)

  | FldName of name
    (** Single named parameter *)

  | FldNameVal of name * 'e
    (** Named implicit parameter together with a value *)

  | FldNameAnnot of name * 'tp
    (** type-annotated implicit parameter *)

(** Type expressions *)
type type_expr = type_expr_data node
and type_expr_data =
  | TWildcard
    (** A placeholder for a fresh unification variable *)

  | TParen of type_expr
    (** Parentheses *)

  | TVar of tvar
    (** Type variable *)

  | TArrow of type_expr * type_expr
    (** Arrow type. The second parameter might have an effect. *)

  | TEffect of type_expr list * type_expr option
    (** Effect: list of simple effect optionally closed by another effect *)

  | TApp of type_expr * type_expr
    (** Type application *)

  | TRecord of ty_field list
    (** Record-like type: left-hand-side of a type-scheme *)

  | TTypeLbl of type_expr
    (** Label of anonymous type parameter of ADT *)

  | TEffectLbl of type_expr
    (** Label of effect type parameter of ADT *)

(** Field of record-like type *)
and ty_field = (type_expr, type_expr) field_data node

(** Declaration of a constructor *)
and ctor_decl = ctor_decl_data node
and ctor_decl_data =
  | CtorDecl of ctor_name * type_expr list
    (** Declaration of a constructor *)

(** Definition of ADT *)
type data_def = data_def_data node
and data_def_data =
  | DD_Data of type_expr * ctor_decl list

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

  | EImplicit of iname
    (** Named implicit parameter *)

  | ECtor of ctor_name
    (** ADT constructor *)

  | EFn   of expr list * expr
    (** Lambda-abstraction *)

  | EApp  of expr * expr list
    (** Function application *)

  | EDefs of def list * expr
    (** Local definitions *)

  | EMatch of expr * match_clause list
    (** Pattern-matching *)

  | EHandler of expr
    (** First-class handler *)

  | EEffect of expr list * expr * expr
    (** Handler of a single operation *)

  | ERecord of field list
    (** Record-like expression, e.g., explicit instantiation of named
      parameters *)

  | EMethod of expr * method_name
    (** Call of a method *)

  | EAnnot of expr * type_expr
    (** Type annotation *)

(** Pattern-matching clauses *)
and match_clause = match_clause_data node
and match_clause_data =
  | Clause of expr * expr

(** Field of record-like expression *)
and field = (type_expr, expr) field_data node

(** Definitions *)
and def = def_data node
and def_data =
  | DLet of expr * expr
    (** Let-definition *)

  | DImplicit of iname
    (** Declaration of implicit parameter *)

  | DData of data_def
    (** Definition of ADT *)

  | DDataRec of data_def list
    (** Definition of mutually recursive ADTs *)

  | DLabel of expr
    (** Creating a new label *)

  | DHandle of expr * expr * h_clause list
    (** Effect handler *)

  | DHandleWith of expr * expr * h_clause list
    (** Effect handler, with first-class handler *)

  | DMethod of expr * expr
    (** Method definition *)

(** Additional clauses of handlers *)
and h_clause = h_clause_data node
and h_clause_data =
  | HCReturn  of expr * expr
    (** Return clause *)

  | HCFinally of expr * expr
    (** Finally clause *)

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
