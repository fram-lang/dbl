(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Raw language: result of yacc-generated. It is later post-parsed by
  [Desugar] in order to obtain the program in Surface language. *)

include SyntaxNode.Export

(** Type variables *)
type tvar = string

(** Variables *)
type var = string

(** Names of implicit parameters *)
type iname = string

(** Names of methods **)
type method_name = string

(** Names of operators *)
type op_name = string

(** Name of a named parameter *)
type name = Lang.Surface.name =
  | NVar         of var
  | NOptionalVar of var
  | NImplicit    of iname
  | NMethod      of method_name

(** Names of constructors of ADTs *)
type ctor_name =
  | CNUnit
  | CNNil
  | CNId  of string
  | CNBOp of op_name
  | CNUOp of op_name

(** Names of modules *)
type module_name = string

type 'a path = 'a path_data node
and 'a path_data = 'a Lang.Surface.path_data =
  | NPName of 'a
  | NPSel  of module_name path * 'a

(** Kind expressions *)
type kind_expr = kind_expr_data node
and kind_expr_data = Lang.Surface.kind_expr_data =
  | KWildcard
    (** A placeholder for a fresh kind unification variable *)

  | KArrow of kind_expr * kind_expr
    (** Arrow kind *)

  | KType
    (** Type kind *)

  | KEffect
    (** Effect kind*)

(** Field of record-like, e.g., scheme name parameters, or explicit
  instantiation *)
type ('tp, 'e) field_data =
  | FldAnonType of 'tp
    (** Anonymous type *)

  | FldType of tvar * kind_expr option
    (** Named type, possibly kind-annotated *)

  | FldTypeVal of tvar * 'tp
    (** Named type with a value *)

  | FldName of name
    (** Single named parameter *)

  | FldNameVal of name * 'e
    (** Named implicit parameter together with a value *)

  | FldNameAnnot of name * 'tp
    (** type-annotated implicit parameter *)

  | FldModule of module_name path
    (** Module grouping named parameters *)

  | FldOpen
    (** Introduce everything from a scheme into the environment *)

(** Type expressions *)
type type_expr = type_expr_data node
and type_expr_data =
  | TWildcard
    (** A placeholder for a fresh unification variable *)

  | TParen of type_expr
    (** Parentheses *)

  | TVar of tvar path * kind_expr option
    (** Type variable *)

  | TArrow of type_expr * type_expr
    (** Arrow type. The second parameter might have an effect. *)

  | TEffect of type_expr list
    (** Effect: list of simple effect *)

  | TApp of type_expr * type_expr
    (** Type application *)

  | TRecord of ty_field list
    (** Record-like type: left-hand-side of a type-scheme *)

  | TTypeLbl of type_expr
    (** Label of anonymous type parameter of ADT *)

(** Field of record-like type *)
and ty_field = (type_expr, type_expr) field_data node

(** Declaration of a constructor *)
and ctor_decl = ctor_decl_data node
and ctor_decl_data =
  | CtorDecl of ctor_name * type_expr list
    (** Declaration of a constructor *)

(** Attributes *)
type attribute = attribute_data node
and attribute_data = Attribute of string * string list 
  (* Atributes constructor with expicit name *)

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

  | EBOpID of op_name
    (** identifier of binary operator *)

  | EUOpID of op_name
    (** identifier of unary operator *)

  | EImplicit of iname
    (** Named implicit parameter *)

  | ECtor of string
    (** ADT constructor *)

  | ENum of int
    (** Integer literal *)

  | ENum64 of int64
    (** 64 bit integer literal *)

  | EStr of string
    (** String literal *)

  | EChr of char
    (** Char literal *)

  | EFn   of expr list * expr
    (** Lambda-abstraction *)

  | EApp  of expr * expr list
    (** Function application *)

  | EDefs of def list * expr
    (** Local definitions *)

  | EMatch of expr * match_clause list
    (** Pattern-matching *)

  | EHandler of expr * h_clause list
    (** First-class handler *)

  | EEffect of
    { label      : expr option;
      args       : expr list;
      resumption : expr option;
      body       : expr
    }
    (** Handler of a single operation *)

  | ERecord of field list
    (** Record-like expression, e.g., explicit instantiation of named
      parameters *)

  | EMethod of expr * method_name
    (** Call of a method *)

  | EMethodCall of expr * method_name node * expr list
    (** Call of a method via the >. operator *)

  | EExtern of string
    (** Externally defined value *)

  | EAnnot of expr * type_expr
    (** Type annotation *)

  | EIf of expr * expr * expr option
    (** If-then-else expression *)

  | ESelect of module_name path * expr
    (** Selection from a module *)
    
  | EBOp of  expr * op_name node * expr
    (** Binary operator *)

  | EUOp of op_name node * expr
    (** Unary operator*)

  | EList of expr list
    (** List-like expression *)

  | EPub of expr
    (** Public modifier in patterns *)

(** Pattern-matching clauses *)
and match_clause = match_clause_data node
and match_clause_data =
  | Clause of expr * expr

(** Field of record-like expression *)
and field = (type_expr, expr) field_data node

(** Definitions *)
and def = (attribute list * def_data) node
and def_data =
  | DLet of expr * expr
    (** Let-definition *)

  | DParam of field
    (** Declaration of a parameter *)

  | DRecord of type_expr * ty_field list
    (** Definition of record-like type *)

  | DData of type_expr * ctor_decl list
    (** Definition of ADT *)

  | DLabel of expr * type_expr option
    (** Creating a new label *)
  
  | DType of type_expr * type_expr
    (** Type alias *)

  | DHandle of expr * type_expr option * expr * h_clause list

  | DHandleWith of expr * type_expr option * expr
    (** Effect handler, with first-class handler *)

  | DMethod of expr * expr
    (** Method definition *)

  | DModule of module_name * def list
    (** Definition of a module *)

  | DOpen of module_name path
    (** Opening a module *)

  | DRec of def list
    (** Block of mutually recursive definitions *)

(** Additional clauses of handlers *)
and h_clause = h_clause_data node
and h_clause_data =
  | HCReturn  of expr * expr
    (** Return clause *)

  | HCFinally of expr * expr
    (** Finally clause *)

(** Path to an imported module tagged as absolute or relative *)
type import_path =
  | IPAbsolute of module_name list * module_name
  | IPRelative of module_name list * module_name

(** Base name of imported module *)
let import_path_name = function
  | IPAbsolute(_, n) | IPRelative(_, n) -> n

(** Module import *)
type import = import_data node
and import_data =
  | IImportAs of import_path * module_name
    (** Import as given module name *)
  | IImportOpen of import_path
    (** Import and immediately open the module *)

(** Program *)
type program = def list node

(** REPL command *)
type repl_cmd =
  | REPL_Exit
    (** Exit the REPL *)

  | REPL_Expr of expr
    (** Evaluate given expression *)
  
  | REPL_Defs of def list
    (** Provide a new group of definitions in a REPL session *)

  | REPL_Import of import
    (** Import a module *)
