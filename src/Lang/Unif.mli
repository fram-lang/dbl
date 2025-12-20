(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Unif language: result of type-inference.
  The main feature of the Unif language is a support for type-unification *)

(** Kind unification variable *)
type kuvar

(** Kinds.

  This is an abstract type. Use [Kind.view] to view it. *)
type kind = UnifCommon.Kind.t

(** Unification variables *)
type uvar

(** Type variables *)
type tvar = UnifCommon.TVar.t

(** Type aliases.

  Type aliases are similar to type variables, but we cannot substitute for
  them. *)
type ty_alias

(** Name of a named type parameter *)
type tname = UnifCommon.Names.tname =
  | TNAnon
    (** Anonymous parameter *)

  | TNVar of string
    (** Regular named type parameter *)

(** Named type parameter *)
type named_tvar = tname * tvar

(** Name of a named parameter *)
type name = UnifCommon.Names.name =
  | NVar      of string
    (** Regular named parameter *)

  | NOptionalVar of string
    (** Optional named parameter *)

  | NImplicit of string
    (** Implicit parameter *)

  | NMethod   of string
    (** Name of methods **)

(* ========================================================================= *)

(** Effects.

  There are two effects in Unif. We only distinguish pure and impure
  computations. Pure computations doesn't use any control-effects or
  recursion, therefore they always terminate. *)
type effct = Pure | Impure

(** Types.

  This is an abstract type. Use [Type.view] to view it. *)
type typ

(** Polymorphic type scheme *)
type scheme = {
  sch_targs : named_tvar list;
    (** universally quantified type variables *)

  sch_named : named_scheme list;
    (** Named parameters *)
  
  sch_body  : typ
    (** Body of the type scheme *)
}

(** Scheme with name *)
and named_scheme = name * scheme

(** Declaration of an ADT constructor *)
type ctor_decl = {
  ctor_name        : string;
    (** Name of the constructor *)

  ctor_targs       : named_tvar list;
    (** Existential type variables of the constructor *)

  ctor_named       : named_scheme list;
    (** Named parameters of the constructor *)

  ctor_arg_schemes : scheme list
    (** Type schemes of the regular parameters *)
}

(** Type substitution *)
type subst

(* ========================================================================= *)
(** AST node

  In Unif, AST node contains additional information about location and the
  context of the pretty-printer. *)
type 'a node = {
  pos  : Position.t;
    (** Location in the source code *)

  pp   : PPTree.t;
    (** Context of the pretty-printer *)

  data : 'a
    (** Payload of the node *)
}

(* ========================================================================= *)
(** Type expressions

  Type expressions are used in a syntax, in contrast to types that are uses
  in a type inference. Type expressions contain additional information about
  location in the source code and effects. *)
type type_expr = type_expr_data node
and type_expr_data =
  | TE_Type of typ
    (** Placeholder for a type. It is used for representing a variable or a
      type inferred during type inference, usually an unification variable *)

  | TE_Effect of type_expr list
    (** Effect. It is represented as a list of simpler effects. Empty list
      is a pure effect. *)

  | TE_PureArrow of scheme_expr * type_expr
    (** Pure arrow *)

  | TE_Arrow of scheme_expr * type_expr * type_expr
    (** Impure arrow. The last parameter is an effect *)

  | TE_Handler of (** First-class handler *)
    { eff_var  : tvar;
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
    { eff       : type_expr;
        (** Effect of this label *)

      delim_tp  : type_expr;
        (** Type of the delimiter *)

      delim_eff : type_expr
        (** Effect of the delimiter *)
    }

  | TE_App of type_expr * type_expr
    (** Type application *)

  | TE_Option of type_expr
    (** Implicitly introduced Option type (in optional parameter) *)

(** Type-scheme expressions *)
and scheme_expr = {
  se_pos   : Position.t;
    (** Location of the scheme expression *)

  se_pp    : PPTree.t;
    (** Context of the pretty-printer *)

  se_targs : named_tvar list;
    (** Type parameters *)

  se_named : named_scheme_expr list;
    (** Named parameters *)

  se_body  : type_expr
    (** Body of the scheme *)
}

(** Named scheme expression *)
and named_scheme_expr = name * scheme_expr

(** Syntactic version of declaration of an ADT constructor *)
type ctor_decl_expr = {
  cde_name        : string;
    (** Name of the constructor *)

  cde_targs       : named_tvar list;
    (** Existential type variables of the constructor *)

  cde_named       : named_scheme_expr list;
    (** Named parameters of the constructor *)

  cde_arg_schemes : scheme_expr list
    (** Type schemes of the regular parameters *)
}

(* ========================================================================= *)

(** Variable *)
type var = Var.t

(** Data-like definition (ADT or label) *)
type data_def =
  | DD_Data of (** Algebraic datatype *)
    { tvar   : tvar;
        (** Type variable that represents this ADT. *)

      proof  : var;
        (** An irrelevant variable that stores the proof that this ADT has
          the following constructors. *)

      args   : named_tvar list;
        (** List of type parameters of this ADT. *)

      ctors  : ctor_decl_expr list;
        (** List of constructors. *)

      eff    : effct
        (** An effect indicating if the type is positively recursive
          (in particular, not recursive at all). Positively recursive
          types can be deconstructed in a pure way. *)
    }

  | DD_Label of (** Label *)
    { tvar      : tvar;
        (** Type variable that represents effect of this label *)

      var       : var;
        (** Regular variable that would store the label *)

      delim_tp  : typ;
        (** Type of the delimiter *)

      annot     : type_expr;
        (** Annotation of the label *)
    }

(* ========================================================================= *)

(** Proofs of ADT shapes. In Unif, we have a separate syntactic category for
  proofs of ADT shapes, but in the later intermediate languages they will be
  translated to just regular expressions. Proofs are always generated by the
  type-checker and are never written by the user, so they should always be
  correct and therefore we do not store any additional information in them *)
type proof_expr =
  | PE_Unit
    (** Proof that Unit is an ADT with only one constructor *)

  | PE_Bool 
    (** Proof that Bool is an ADT with True and False constructors *)

  | PE_Option of typ
    (** Proof that Option is an ADT with None and Some constructors. It
      expects a single type: the type of the Some parameter. *)

  | PE_Var of var * typ list
    (** Variable generated at the ADT definition, applied to the parameters
      of the ADT. *)

(** Pattern *)
type pattern = pattern_data node
and pattern_data =
  | PWildcard
    (** Wildcard pattern -- it matches everything *)

  | PAs of pattern * var
    (** Pattern that binds a variable and continues with a subpattern *)

  | PCtor of
    string * int * proof_expr * tvar list * pattern list * pattern list
    (** ADT constructor pattern. It stores a name, constructor index,
      proof that matched type is an ADT, existential type binders, patterns
      for named parameters, and patterns for regular parameters. *)

  | PAnnot of pattern * scheme_expr
    (** Scheme annotated pattern *)

(** Polymorphic expression *)
type poly_expr = poly_expr_data node
and poly_expr_data =
  | EVar of var
    (** Variable *)

  | ECtor of named_tvar list * proof_expr * int
    (** ADT constructor. The first parameter is a list of type parameters of
      the ADT, that are prepended to the scheme of the constructor. The
      remaining parameters are the shape proof and the index of the
      constructor. *)

  | EPolyFun of named_tvar list * (name * var * scheme_expr) list * expr
    (** Polymorphic lambda abstraction. Always pure *)

  | EGen of named_tvar list * (name * var * scheme_expr) list * poly_expr
    (** Polymorphic lambda abstraction that generalizes extra parameters.
      Always pure. In the scheme of this expression, the type parameters
      and named parameters are prepended to the type parameters and named
      parameters of the body, respectively. *)

(** Polymorphic function. The scheme should be known from the context. *)
and poly_fun = poly_fun_data node
and poly_fun_data =
  | PF_Fun of tvar list * var list * expr
    (** Polymorphic lambda abstraction. Always pure *)

  | PF_Hole of poly_fun option BRef.t
    (** Placeholder for a polymorphic function, to be filled by constraint
      solving. *)

(** Expression *)
and expr = expr_data node
and expr_data =
  | EInst of poly_expr * type_expr list * poly_fun list
    (** Instantiation of polymorphic expression *)

  | ENum of int
    (** Integer literal *)

  | ENum64 of int64
    (** 64 bit integer literal *)

  | EStr of string
    (** String literal *)

  | EChr of char
    (** Character literal *)

  | EFn of var * scheme_expr * expr * effct
    (** Effect-annotated lambda-abstraction. *)

  | EAppPoly of expr * poly_fun
    (** Function application to polymorphic expression *)

  | EAppMono of expr * expr
    (** Function application to regular, possibly effectful expression *)

  | ELetPoly of var * poly_expr * expr
    (** Let-definition with polymorphic expression *)

  | ELetMono of var * expr * expr
    (** Monomorphic let-definition *)

  | ELetRec of (** Mutually recursive let-definitions *)
    { targs : named_tvar list;
      (** Type parameters common to all definitions *)

      named : (name * var * scheme_expr) list;
      (** Named parameters common to all definitions *)

      defs  : rec_def list;
      (** Mutually recursive definitions *)

      body  : expr
      (** Body of the let-rec *)
    }

  | ERecCtx of expr
    (** Context for mutually recursive definitions. It is used to bind
      less polymorphic variables in mutually recursive definitions.
      It should appear only in the recursive definitions under some impure
      function. *)

  | EData of data_def list * expr
    (** Definition of mutually recursive ADTs. *)

  | ETypeAlias of ty_alias * type_expr * expr
    (** Definition of a type alias. *)

  | EMatchEmpty of proof_expr * expr * typ * effct
    (** Pattern-matching of an empty type. The first parameter is an
      irrelevant expression that is a witness that the type of the second
      parameter is an empty ADT. The last parameter is an effect of the whole
      expression. *)

  | EMatch of expr * match_clause list * typ * effct
    (** Pattern-matching. It stores type and effect of the whole expression.
      *)

  | EMatchPoly of poly_expr * pattern * expr * typ * effct
    (** Pattern-matching of polymorphic expression with a single match-clause.
      *)

  | EHandle of tvar * var * expr * expr
    (** Handling construct. In [EHandle(a, x, e1, e2)] the meaning of
      parameters is the following.
      - [a]  -- effect variable that represent introduced effect
      - [x]  -- capability variable
      - [e1] -- expression that should evaluate to first-class handler
      - [e2] -- body of the handler *)

  | EHandler of (** First class handler *)
    { label     : var;
      (** Variable, that binds the runtime-label *)

      eff_var   : tvar;
      (** Effect variable *)

      delim_tp  : typ;
      (** Type of the delimiter *)

      cap_type  : typ;
      (** Type of the capability *)

      cap_body  : expr;
      (** An expression that should evaluate to effect capability. It can use
        [label] and [effect]. It should be pure and have type [cap_type]. *)

      ret_var   : var;
      (** An argument to the return clause *)

      body_tp   : typ;
      (** Type of the handled expression. It is also a type of an argument
        [ret_var] to the return clause *)

      ret_body  : expr;
      (** Body of the return clause *)

      fin_var   : var;
      (** An argument to the finally clause. It has type [delim_tp] *)

      fin_body  : expr;
      (** Body of the finally clause *)
    }

  | EEffect of expr * var * expr * typ
    (** Capability of effectful functional operation. It stores dynamic label,
      continuation variable binder, body, and the type of the whole
      expression. *)

  | EExtern of string * typ
    (** Externally defined value *)

  | EAnnot of expr * type_expr
    (** Expression explicitly annotated with a type *)

  | ERepl of (unit -> expr) * typ
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      *)

  | EReplExpr of (** Single expression typed in REPL *)
    { body   : expr;
      (** The REPL prints the type of this expression, evaluates it,
        passes to the [to_str] function, prints the result, and continues to
        the [rest] expression. *)

      to_str : expr;
      (** The expression that should evaluate to a function that converts the
        result of [body] to string. The returned string is printed by the
        REPL. *)

      rest   : expr
      (** The expression to continue the REPL. *)
    }

  | EReplDir of { cont : (unit -> unit) ; rest : expr }
  
    

(** Definition of recursive value *)
and rec_def =
  { rd_pos      : Position.t;
    (** Position of the definition *)

    rd_pp       : PPTree.t;
    (** Context of the pretty-printer *)

    rd_poly_var : var;
    (** More polymorphic variable that represents the definition. It is bound
      in the body of let-rec. *)

    rd_var      : var;
    (** Less polymorphic variable that represents the definition. It is bound
      locally by the nearest [ERecCtx] in the mutually recursive definitions.
      *)

    rd_scheme   : scheme_expr;
    (** Scheme of the definition, or more precisely, the scheme of [rd_var].
      It doesn't contain common parameters. *)

    rd_body     : poly_fun;
    (** Body of the definition *)
  }

(** Clause of a pattern matching *)
and match_clause = pattern * expr

(** Program *)
type program = expr

(* ========================================================================= *)
(** Operations on kind unification variables *)
module KUVar : sig

  (** Check for equality *)
  val equal : kuvar -> kuvar -> bool

  (** Set the value of a kind unification variable. Returns [true] if
    successful, [false] if it would break non-effect constraints, i.e. if it
    would result in an effect kind on the right-hand-side of some arrow kind.
    *)
  val set : kuvar -> kind -> bool
end

(* ========================================================================= *)
(** Operations on kinds *)
module Kind : sig
  (** View of kinds *)
  type kind_view =
    | KType
      (** Kind of all types *)

    | KEffect
      (** Kind of all effects. *)
    
    | KUVar of kuvar
      (** Unification variable *)

    | KArrow of kind * kind
      (** Arrow kind *)

  (** Kind of all types *)
  val k_type : kind

  (** Kind of all effects. *)
  val k_effect : kind

  (** Arrow kind. Returns [None] if the right-hand-side kind is an effect
    kind. *)
  val k_arrow : kind -> kind -> kind option

  (** Create an arrow kind with multiple parameters. Returns [None] if the
    right-hand-side kind is an effect kind (unless the list of parameter kinds
    is empty). *)
  val k_arrows : kind list -> kind -> kind option

  (** Create an arrow kind with multiple parameters, assuming that the
    right-hand-side kind is not an effect kind. *)
  val k_noneff_arrows : kind list -> kind -> kind

  (** Create a fresh unification kind variable. *)
  val fresh_uvar : unit -> kind

  (** Reveal a top-most constructor of a kind *)
  val view : kind -> kind_view

  (** Check if given kind contains given unification variable *)
  val contains_uvar : kuvar -> kind -> bool
end

(* ========================================================================= *)
(** Operations on type variables *)
module TVar : sig
  (** Kind of a type variable *)
  val kind : tvar -> kind

  (** Create a fresh type variable of given kind. There are two unique
    identifiers that can be optionally provided (if omitted, they will be the
    same as the freshly generated unique identifier of the variable):
    - [method_ns] will be used to determine the method namespace associated
      with the type variable;
    - [pp_uid] will be used by the pretty-printer to identify the type
      variable. *)
  val fresh :
    ?method_ns:UID.t -> ?pp_uid:PPTree.uid -> scope:Scope.t -> kind -> tvar

  (** Compare two type variables *)
  val compare : tvar -> tvar -> int

  (** Check type variables for equality *)
  val equal : tvar -> tvar -> bool

  (** Get the unique identifier *)
  val uid : tvar -> UID.t

  (** Get the method namespace identifier *)
  val method_ns : tvar -> UID.t

  (** Get the unique identifier for pretty-printing *)
  val pp_uid : tvar -> PPTree.uid

  (** Get the scope of a type variable *)
  val scope : tvar -> Scope.t

  (** Check if a type variable can be used in a given scope *)
  val in_scope : tvar -> Scope.t -> bool

  (** Finite sets of type variables *)
  module Set : Set.S with type elt = tvar

  (** Finite map from type variables *)
  module Map : Map.S with type key = tvar
end

(* ========================================================================= *)
(** Operations on type aliases *)
module TyAlias : sig
  (** Create fresh type alias. Optionally, a unique identifier used by the
    pretty-printer can be provided. If omitted, it will be the same as the
    freshly generated unique identifier of the alias. *)
  val fresh : ?pp_uid:PPTree.uid -> scope:Scope.t -> unit -> ty_alias

  (** Check type aliases for equality *)
  val equal : ty_alias -> ty_alias -> bool

  (** Get the unique identifier for pretty-printing *)
  val pp_uid : ty_alias -> PPTree.uid

  (** Finite map from type aliases *)
  module Map : Map.S with type key = ty_alias
end

(* ========================================================================= *)
(** Operations on unification variables *)
module UVar : sig
  type t = uvar

  (** Get the kind of given unification variable *)
  val kind : t -> kind

  (** Check unification variables for equality *)
  val equal : t -> t -> bool

  (** Get a unique identifier *)
  val uid : t -> UID.t

  (** Get a scope of a unification variable *)
  val scope : t -> Scope.t

  (** Set a unification variable without checking any constraints. *)
  val raw_set : t -> typ -> unit

  (** Promote unification variable to fresh type variable *)
  val fix : t -> tvar

  (** Update the scope of a unification variable. Its scope is set to the
    intersection of its current scope and the given scope. *)
  val shrink_scope : t -> Scope.t -> unit

  (** Check if a unification variable can be used in a given scope *)
  val in_scope : t -> Scope.t -> bool

  (** Get position of variable's first use *)
  val pos : t -> Position.t

  (** Set of unification variables *)
  module Set : Set.S with type elt = t

  (** Finite maps with unification variables as keys *)
  module Map : Map.S with type key = t
end

(* ========================================================================= *)
(** Operations on parameter names *)
module Name : sig
  (** Check names for equality *)
  val equal : name -> name -> bool

  (** Find value associated to given name on a list *)
  val assoc : name -> (name * 'a) list -> 'a option

  (** Finite sets of names *)
  module Set : Set.S with type elt = name

  (** Finite maps with names as keys *)
  module Map : Map.S with type key = name
end

(* ========================================================================= *)
(** Operations on effects *)
module Effect : sig
  (** Join of two effects *)
  val join : effct -> effct -> effct

  (** Join of multiple effects *)
  val joins : effct list -> effct
end

(* ========================================================================= *)
(** Operations on types *)
module Type : sig
  (** View of a type *)
  type type_view =
    | TEffect
      (** Something of effect kind. In Unif all effects are equal *)

    | TUVar of uvar
      (** Unification variable *)

    | TVar of tvar
      (** Regular type variable *)

    | TArrow of scheme * typ * effct
      (** Effect annotated arrow *)

    | THandler of tvar * typ * typ * typ
      (** First class handler. In [THandler(a, tp, itp, otp)]:
        - [a] is a variable bound in [tp], [itp], and [ieff];
        - [tp] is a type of provided capability;
        - [itp] is a typ of handled expression.
        - [otp] s a type of the whole handler. *)
  
    | TLabel of typ
      (** Type of a first-class label. It stores the type of the delimiter. *)

    | TApp of typ * typ
      (** Type application *)

    | TAlias   of ty_alias * typ
      (** Type alias. It contains the unfolded type. *)

  (** Head of a neutral type *)
  type neutral_head =
    | NH_UVar of uvar
      (** Unification variable *)

    | NH_Var  of tvar
      (** Regular variable *)

  (** Weak head normal form of a type *)
  type whnf =
    | Whnf_Effect
      (** Effect *)

    | Whnf_Neutral of neutral_head * typ list
      (** Neutral type. Its parameters are in reversed order. *)

    | Whnf_Arrow of scheme * typ * effct
      (** Effect annotated arrow *)

    | Whnf_Handler   of tvar * typ * typ * typ
      (** Handler type *)

    | Whnf_Label of typ
      (** Label type *)

  (** Effect *)
  val t_effect : typ

  (** Unit type *)
  val t_unit : typ

  (** Bool type *)
  val t_bool : typ

  (** Option type *)
  val t_option : typ -> typ

  (** Unification variable *)
  val t_uvar : uvar -> typ

  (** Regular type variable *)
  val t_var : tvar -> typ

  (** Pure arrow type *)
  val t_pure_arrow : scheme -> typ -> typ

  (** Pure arrow that takes multiple arguments *)
  val t_pure_arrows : scheme list -> typ -> typ

  (** Arrow type *)
  val t_arrow : scheme -> typ -> effct -> typ

  (** Type of first-class handlers *)
  val t_handler : tvar -> typ -> typ -> typ -> typ

  (** Type of first-class label *)
  val t_label : typ -> typ

  (** Type application *)
  val t_app : typ -> typ -> typ

  (** Type application to multiple arguments *)
  val t_apps : typ -> typ list -> typ

  (** Type alias. It contains the unfolded type. *)
  val t_alias : ty_alias -> typ -> typ

  (** Fresh unification variable (packed as type) *)
  val fresh_uvar : pos:Position.t -> scope:Scope.t -> kind -> typ

  (** Reveal a top-most constructor of a type *)
  val view : typ -> type_view

  (** Compute weak head normal form of a type *)
  val whnf : typ -> whnf

  (** Get the kind of given type *)
  val kind : typ -> kind

  (** Check if given unification variable appears in given type. It is
    equivalent to [UVar.mem u (uvars tp)], but faster. *)
  val contains_uvar : uvar -> typ -> bool

  (** Set of unification variables in given type *)
  val uvars : typ -> UVar.Set.t

  (** Extend given set of unification variables by unification variables
    from given type. Equivalent to [uvars tp UVar.Set.empty] *)
  val collect_uvars : typ -> UVar.Set.t -> UVar.Set.t

  (** Apply substitution to a type *)
  val subst : subst -> typ -> typ

  (** Ensure that given type fits given scope. It changes scope constraints
    of unification variables accordingly. On error, it returns escaping type
    variable. *)
  val shrink_scope : scope:Scope.t -> typ -> (typ, tvar) result

  (** Compute size of a type. Unification variables have size one, so
    this function is not stable under instantiation of unification variables.
  *)
  val size : typ -> int
end

(* ========================================================================= *)
(** Operations on type schemes *)
module Scheme : sig
  (** Create a monomorphic type-scheme *)
  val of_type : typ -> scheme

  (** Convert scheme to monomorphic type. Returns [None] when the scheme is
    polymorphic. *)
  val to_type : scheme -> typ option

  (** Check if given type-scheme is monomorphic *)
  val is_monomorphic : scheme -> bool

  (** Set of unification variables in given scheme *)
  val uvars : scheme -> UVar.Set.t

  (** Extend given set of unification variables by unification variables
    from given scheme. Equivalent to [uvars sch UVar.Set.empty] *)
  val collect_uvars : scheme -> UVar.Set.t -> UVar.Set.t

  (** Apply substitution to a scheme *)
  val subst : subst -> scheme -> scheme
end

(* ========================================================================= *)
(** Operations on named type schemes *)
module NamedScheme : sig
  (** Apply substitution to a named scheme *)
  val subst : subst -> named_scheme -> named_scheme
end

(* ========================================================================= *)
(** Operations on constructor declarations *)
module CtorDecl : sig
  (** Extend given set of unification variables by unification variables
    from given constructor. *)
  val collect_uvars : ctor_decl -> UVar.Set.t -> UVar.Set.t

  (** Set of unification variables in given scheme *)
  val subst : subst -> ctor_decl -> ctor_decl

  (** Get the index of a constructor with a given name *)
  val find_index : ctor_decl list -> string -> int option

  (** Check if given constructor is positive, i.e., if all type variables on
    non positive positions and all scopes of unification variables fit in
    [nonrec_scope], assuming that the constructor fits in [scope]. The [args]
    parameter is a list of arguments of the datatype. *)
  val is_positive :
    scope:Scope.t -> args:named_tvar list -> nonrec_scope:Scope.t ->
      ctor_decl -> bool
end

(* ========================================================================= *)
(** Type substitutions *)
module Subst : sig
  (** Empty substitution. The [scope] argument should be the scope of the types
    that will be substituted. *)
  val empty : scope:Scope.t -> subst

  (** Extend substitution. It is rather parallel extension than composition *)
  val add_type : subst -> tvar -> typ -> subst

  (** Extend substitution with a renaming of type variable. Equivalent to
    [add_type sub x (Type.t_var y)] *)
  val rename_tvar : subst -> tvar -> tvar -> subst
end

(* ========================================================================= *)
(** Built-in types *)
module BuiltinType : sig
  (** Int type *)
  val tv_int : tvar

  (** Int type *)
  val tv_int64 : tvar

  (** String type *)
  val tv_string : tvar

  (** Char type *)
  val tv_char : tvar

  (** Unit type *)
  val tv_unit : tvar

  (** Bool type *)
  val tv_bool : tvar

  (** Option type *)
  val tv_option : tvar

  (** IO effect *)
  val tv_io : tvar

  (** List of all built-in types with their names *)
  val all : (string * tvar) list
end

(* ========================================================================= *)
(** Type expressions *)
module TypeExpr : sig
  (** Convert to regular type *)
  val to_type : type_expr -> typ
end

(* ========================================================================= *)
(** Scheme expressions *)
module SchemeExpr : sig
  (** Create a monomorphic scheme expression *)
  val of_type_expr : type_expr -> scheme_expr

  (** Convert to monomorphic type expression. Returns [None] when the scheme
    is polymorphic. *)
  val to_type_expr : scheme_expr -> type_expr option

  (** Convert to type-scheme *)
  val to_scheme : scheme_expr -> scheme

  (** Convert scheme to scheme expression. *)
  val of_scheme : pos:Position.t -> pp:PPTree.t -> scheme -> scheme_expr

  (** Substitute in a scheme expression *)
  val subst : subst -> scheme_expr -> scheme_expr
end

(* ========================================================================= *)
(** Named scheme expressions *)
module NamedSchemeExpr : sig
  (** Convert to named scheme *)
  val to_named_scheme : named_scheme_expr -> named_scheme
end

(* ========================================================================= *)
(** Syntactic constructor declarations *)
module CtorDeclExpr : sig
  (** Convert to constructor declarations *)
  val to_ctor_decl : ctor_decl_expr -> ctor_decl
end

(* ========================================================================= *)
(** Proofs of ADT shapes *)
module ProofExpr : sig
  (** Apply substitution to a proof *)
  val subst : subst -> proof_expr -> proof_expr
end

(* ========================================================================= *)
(** Renaming of variables *)
module Ren : sig
  type t

  (** Empty renaming *)
  val empty : scope:Scope.t -> t

  (** Extend renaming with a renaming of a type variable *)
  val add_tvar : t -> tvar -> tvar -> t

  (** Extend renaming with a renaming of a regular variable *)
  val add_var : t -> var -> var -> t

  (** Rename type variable binder *)
  val rename_tvar : t -> tvar -> tvar

  (** Rename named type variable binder *)
  val rename_named_tvar : t -> named_tvar -> named_tvar

  (** Rename named type variable binders *)
  val rename_named_tvars : t -> named_tvar list -> named_tvar list

  (** Rename type *)
  val rename_type : t -> typ -> typ

  (** Rename type scheme *)
  val rename_scheme : t -> scheme -> scheme

  (** Rename scheme expression *)
  val rename_scheme_expr : t -> scheme_expr -> scheme_expr

  (** Rename variables in pattern *)
  val rename_pattern : t -> pattern -> pattern
end

(* ========================================================================= *)
(** Pretty-printing *)
module Pretty : sig
  (** Context of pretty-printing *)
  type ctx

  (** Create an empty context. *)
  val empty_context : unit -> ctx

  (** Pretty-print kind *)
  val pp_kind : ctx -> kind -> string

  (** Pretty-print type variable *)
  val pp_tvar : ctx -> PPTree.t -> tvar -> string

  (** Pretty-print type *)
  val pp_type : ctx -> PPTree.t -> typ -> string

  (** Pretty-print type scheme *)
  val pp_scheme : ctx -> PPTree.t -> scheme -> string

  (** Pretty-print additional information about printing context, e.g.,
    locations of binders of anonymous types. *)
  val additional_info : ctx -> string
end
