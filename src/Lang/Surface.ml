(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Surface language: result of parsing and input for type inference *)

include SyntaxNode.Export

(** Type variables *)
type tvar = string

(** Variables *)
type var = string

(** name of a implicit parameter *)
type iname = string

(** Name of a ADT constructor *)
type ctor_name = string

(** Name of a method *)
type method_name = string

(** Name of a module *)
type module_name = string

(** Module path to an identifier of type 'a *)
type 'a path =
  | NPName of 'a
  | NPSel  of module_name * 'a path

(** Extract the base name from a path *)
let rec path_name = function
  | NPName x    -> x
  | NPSel(_, p) -> path_name p

(** Name of a named type parameter *)
type tname =
  | TNAnon
  | TNEffect
  | TNVar of tvar

(** Name of a named parameter *)
type name =
  | NLabel
  | NVar      of var
  | NImplicit of iname

(** Identifier, i.e., object that can be bound in patterns *)
type ident =
  | IdLabel
  | IdVar      of var
  | IdImplicit of iname
  | IdMethod   of method_name
    (** Methods must have an arrow type, where the argument type is a type
      of "self" variable. *)

(** Type expressions *)
type type_expr = type_expr_data node
and type_expr_data =
  | TWildcard
    (** A placeholder for a fresh unification variable *)

  | TVar of tvar path
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

  sch_targs : named_type_arg list;
    (** Type parameters *)

  sch_named : named_scheme list;
    (** Named parameters *)

  sch_body : type_expr
    (** Body of the scheme *)
}

(** Declaration of implicit/named parameter *)
and named_scheme = (name * scheme_expr) node

(** Type formal parameter *)
and type_arg = type_arg_data node
and type_arg_data =
  | TA_Effect
    (** Effect variable *)

  | TA_Var of tvar
    (** Type variable *)

and named_type_arg = (tname * type_arg) node

(** Declaration of constructor of ADT *)
type ctor_decl = ctor_decl_data node
and ctor_decl_data =
  | CtorDecl of
    ctor_name * named_type_arg list * named_scheme list * scheme_expr list
    (** Declaration of constructor of ADT *)

(** Definition of ADT *)
type data_def = data_def_data node
and data_def_data =
  | DD_Data of tvar * named_type_arg list * ctor_decl list

(** Patterns *)
type pattern = pattern_data node
and pattern_data =
  | PWildcard
    (** Wildcard pattern -- it matches everything *)

  | PId of ident
    (** Pattern that binds an identifier*)

  | PCtor of ctor_name path node * named_type_arg list *
             named_pattern list * pattern list
    (** ADT constructor pattern *)

  | PAnnot of pattern * scheme_expr
    (** Scheme annotation *)

(** Pattern for named parameter *)
and named_pattern = (name * pattern) node

(** Formal argument *)
type arg =
  | ArgAnnot of pattern * scheme_expr
    (** Argument with scheme annotation *)

  | ArgPattern of pattern
    (** Argument with pattern-matching *)

(** Named formal argument *)
type named_arg = (name * arg) node

(** Explicit type instantiation *)
type type_inst = (tname * type_expr) node

(** Polymorphic expressions *)
type poly_expr = poly_expr_data node
and poly_expr_data =
  | EVar      of var path
    (** Variable *)

  | EImplicit of iname path
    (** Implicit parameter *)

  | ECtor     of ctor_name path
    (** ADT constructor *)

  | EMethod   of expr * method_name
    (** Call of a method *)

(** Expressions *)
and expr = expr_data node
and expr_data =
  | EUnit
    (** Unit expression. Used only as the expression after the last definition
      in a program. *)

  | ENum of int
    (** Integer literal *)

  | EStr of string
    (** String literal *)

  | EPoly of poly_expr * type_inst list * inst list
    (** Polymorphic expression with partial explicit instantiation, possibly
      empty *)

  | EFn   of arg * expr
    (** Lambda abstraction *)

  | EApp  of expr * expr
    (** Application *)

  | EDefs of def list * expr
    (** Local definitions *)

  | EMatch of expr * match_clause list
    (** Pattern-matching *)

  | EHandler of expr
    (** First-class handler *)

  | EEffect of arg * expr
    (** Effectful operation. The only argument is a continuation. Other
      arguments should be bound using regular lambda abstractions ([EFn]). *)

  | EExtern of string
    (** Externally defined value *)

  | EAnnot of expr * type_expr
    (** Type annotation *)

  | ERepl of def Seq.t
    (** REPL. It is a lazy sequence of definitions provided by a user. *)

(** Explicit instantiation of named parameters in polymorphic expression *)
and inst = (name * expr) node

(** Local definitions *)
and def = def_data node
and def_data =
  | DLetId of ident * expr
    (** Let definition: monomorphic or polymorphic, depending on effect *)

  | DLetFun of ident * named_type_arg list * named_arg list * expr
    (** Polymorphic function definition *)

  | DLetPat  of pattern * expr
    (** Let definition combined with pattern-matching. Always monomorphic *)

  | DLabel   of type_arg option * pattern
    (** Creating a new label. Optional type argument binds newly created
      effect. *)

  | DHandlePat of (* Effect handler combined with pattern matching *)
    { label   : expr option;
      (** Effect label of the handled effect. [None] means that handler is
        lexical and generates its own label. *)

      effect : type_arg option;
      (** Optional name for the handled effect *)

      cap_pat : pattern;
      (** Pattern matched against the effect capability *)

      capability : expr;
      (** An expression providing capability to this handler *)

      ret_clauses : match_clause list;
      (** List of return clauses. Empty list means the default clause. *)

      fin_clauses : match_clause list
      (** List of finally clauses. Empty list means the default clause. *)
    }

  | DImplicit of iname * named_type_arg list * scheme_expr
    (** Declaration of implicit. The second parameter is a list of types
      that may differ between different uses of the implicit *)

  | DData of data_def
    (** Definition of non-recursive ADT *)

  | DDataRec of data_def list
    (** Definition of mutually recursive ADTs *)

  | DModule of module_name * def list
    (** Definition of a module *)

  | DOpen of module_name path
    (** Opening a module *)

  | DReplExpr of expr
    (** Print type, evaluate, and print the expression, provided by a user in
      REPL. *)

  | DPub of def
    (** Mark a definition as public *)

(** Pattern-matching clauses *)
and match_clause = match_clause_data node
and match_clause_data =
  | Clause of pattern * expr

(** Program *)
type program = expr
