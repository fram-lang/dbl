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
type 'a path = 'a path_data node
and 'a path_data =
  | NPName of 'a
  | NPSel  of module_name path * 'a

(** Extract the base name from a path *)
let path_name (path : _ path) =
  match path.data with
  | NPName x | NPSel(_, x) -> x

(** Name of a named type parameter *)
type tname =
  | TNAnon
  | TNVar of tvar

(** Name of a named parameter *)
type name =
  | NVar         of var
  | NOptionalVar of var
  | NImplicit    of iname
  | NMethod      of method_name 

(** Visibility of definition *)
type is_public = bool

(** Identifier, i.e., object that can be bound in patterns *)
type ident =
  | IdVar      of var
  | IdImplicit of iname
  | IdMethod   of method_name
    (** Methods must have an arrow type, where the argument type is a type
      of "self" variable. *)

(** Kind expressions *)
type kind_expr = kind_expr_data node
and kind_expr_data =
  | KWildcard
    (** A placeholder for a fresh kind unification variable *)

  | KArrow of kind_expr * kind_expr
    (** Arrow kind *)

  | KType
    (** Type kind *)

  | KEffect
    (** Effect kind *)

(** Type expressions *)
type type_expr = type_expr_data node
and type_expr_data =
  | TWildcard
    (** A placeholder for a fresh unification variable *)

  | TVar of tvar path
    (** A (non-unification) type variable *)

  | TEffect of type_expr list
    (** Effect: list of simple effects *)

  | TPureArrow of scheme_expr * type_expr
    (** Pure function: a function without effects, that always terminates *)

  | TArrow of scheme_expr * type_expr * type_expr
    (** Effectful function: the last parameter is an effect *)

  | THandler of (** First-class handler *)
    { effect   : tvar;
        (** Effect variable bound by this handler (it bound in [cap_type],
          [in_type], and [in_eff] *)

      cap_type : type_expr;
        (** Type of a capability provided by this handler *)

      in_type  : type_expr;
        (** Inner type of a handler: a type of expression that can be run
          inside this handler *)

      in_eff   : type_expr;
        (** Inner effect of a handler *)

      out_type : type_expr;
        (** Outer type of a handler: a type of the whole handler expression
        *)

      out_eff  : type_expr
        (** Outer effect of a handler *)
    }

  | TE_Label of (** First-class label *)
    { effect    : type_expr;
        (** Effect of this label *)

      delim_tp  : type_expr;
        (** Type of the delimiter *)

      delim_eff : type_expr
        (** Effect of the delimiter *)
    }

  | TApp of type_expr * type_expr
    (** Type application *)

(** Type-scheme expressions *)
and scheme_expr = {
  sch_pos   : Position.t;
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
  | TA_Var of is_public * tvar * kind_expr
    (** Type variable *)
  
  | TA_Wildcard
    (** Wildcard *)

and named_type_arg = (tname * type_arg) node

(** Declaration of constructor of ADT *)
type ctor_decl = ctor_decl_data node
and ctor_decl_data = {
  cd_name        : ctor_name;
  cd_targs       : named_type_arg list;
  cd_named       : named_scheme list;
  cd_arg_schemes : scheme_expr list
}

(** Patterns *)
type pattern = pattern_data node
and pattern_data =
  | PWildcard
    (** Wildcard pattern -- it matches everything *)

  | PId of is_public * ident
    (** Pattern that binds an identifier*)

  | PCtor of ctor_name path * named_pattern list * pattern list
    (** ADT constructor pattern *)

  | PAnnot of pattern * scheme_expr
    (** Scheme annotation *)

(** Set of named subpatterns of constructor *)
and ctor_pattern_named =
  | CNParams of named_type_arg list * named_pattern list
    (** Named type parameters and named patterns of a constructor *)
  | CNModule of is_public * module_name
    (** Bind all named parameters under the specified module name *)

(** Pattern for a named parameter *)
and named_pattern = named_pattern_data node
and named_pattern_data =
  | NP_Type   of named_type_arg
    (** Type parameter *)
  | NP_Val    of name * pattern
    (** Value parameter *)
  | NP_Module of module_name
    (** Bind everything into a module *)
  | NP_Open
    (** Introduce everything into the environment *)

(** Formal argument *)
type arg =
  | ArgAnnot of pattern * scheme_expr
    (** Argument with scheme annotation *)

  | ArgPattern of pattern
    (** Argument with pattern-matching *)

(** Named formal argument *)
type named_arg = (name * arg) node

(** Polymorphic expressions *)
type poly_expr = poly_expr_data node
and poly_expr_data =
  | EVar      of var path
    (** Variable *)

  | EImplicit of iname path
    (** Implicit parameter *)

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

  | ENum64 of int64
    (** 64 bit integer literal *)

  | EStr of string
    (** String literal *)

  | EChr of char
    (** Char literal *)

  | EPoly of poly_expr * inst list
    (** Polymorphic expression with partial explicit instantiation, possibly
      empty *)

  | EFn   of arg * expr
    (** Lambda abstraction *)

  | EApp  of expr * expr
    (** Application to an expression *)

  | EAppFn of expr * named_pattern list * expr
    (** Application to explicitly polymorphic function *)

  | EDefs of def list * expr
    (** Local definitions *)

  | EMatch of expr * match_clause list
    (** Pattern-matching *)

  | EHandler of expr * match_clause list * match_clause list
    (** First-class handler, with return and finally clauses. For each of these
      clause lists, empty list means the default identity clause *)

  | EEffect of expr option * arg * expr
    (** Effectful operation. The only argument is a continuation. Other
      arguments should be bound using regular lambda abstractions ([EFn]).
      The first parameter is an optional label. *)

  | EExtern of string
    (** Externally defined value *)

  | EAnnot of expr * type_expr
    (** Type annotation *)

  | ERepl of def list Seq.t
    (** REPL. It is a lazy sequence of groups of definitions provided by a
      user. Each group is treated as a monolith: if one of them fails, the
      other have no effect. *)

(** Explicit instantiation of named parameters in polymorphic expression *)
and inst = inst_data node
and inst_data =
  | IType   of tvar * type_expr
    (** Explicit instantiation of a type variable *)
  | IVal    of name * expr
    (** Explicit instantiation of a value-level name *)
  | IModule of module_name path
    (** Explicit instantiation, that takes values from given module *)
  | IOpen
    (** Explicit instantiation, that takes values from the environment *)

(** Local definitions *)
and def = def_data node
and def_data =
  | DLetId of is_public * ident * expr
    (** Let definition: monomorphic or polymorphic, depending on effect *)

  | DLetFun of is_public * ident * named_type_arg list * named_arg list * expr
    (** Polymorphic function definition *)

  | DLetPat  of pattern * expr
    (** Let definition combined with pattern-matching. Always monomorphic *)

  | DMethodFn of is_public * var * method_name
    (** Declaration of function that should be interpreted as a method *)

  | DLabel   of type_arg * pattern
    (** Creating a new label. *)

  | DHandlePat of pattern * type_arg * expr
    (** Effect handler combined with pattern matching. In
      [DHandlePat(pat, eff, body)] the meaning of parameters is the following.
      - [pat]  -- Pattern matched against the effect capability.
      - [eff]  -- A name for the handled effect.
      - [body] -- An expression that should evaluate to a first-class handler,
          providing the capability of the handled effect. *)

  | DTypeParam of tname * kind_expr
    (** Declaration of type parameter *)

  | DValParam of name * scheme_expr
    (** Declaration of value parameter *)

  | DData of (** Definition of non-recursive ADT *)
    { public_tp    : is_public;
        (** A flag indicating that the type is public *)

      public_ctors : is_public;
        (** A flag indication that the constructors are visible *)

      args         : named_type_arg list;
        (** List of type parameters *)

      ctors        : ctor_decl list
        (** List of constructors *)
    }

  | DBlock of def list
    (** Block of definitions that share parameter declarations *)

  | DRec of def list
    (** Mutually recursive definitions *)

  | DModule of is_public * module_name * def list
    (** Definition of a module *)

  | DOpen of is_public * module_name path
    (** Opening a module *)

  | DReplExpr of expr
    (** Print type, evaluate, and print the expression, provided by a user in
      REPL. *)

(** Pattern-matching clauses *)
and match_clause = match_clause_data node
and match_clause_data =
  | Clause of pattern * expr

(** Program *)
type program = expr
