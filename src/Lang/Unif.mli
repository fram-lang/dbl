(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Unif language: result of type-inference.
  The main feature of the Unif language is a support for type-unification *)

include module type of SyntaxNode.Export

(** Kind unification variable *)
type kuvar

(** Kinds.

  This is an abstract type. Use [Kind.view] to view it. *)
type kind

(** Unification variables *)
type uvar

(** Type variables *)
type tvar

(** Scope of a type *)
type scope

(** Name of a named type parameter *)
type tname =
  | TNAnon
    (** Anonymous parameter *)

  | TNVar of string
    (** Regular named type parameter *)

(** Named type parameter *)
type named_tvar = tname * tvar

(** Name of a named parameter *)
type name =
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
  computations. Pure computations doesn't use any control-effects and
  recursion, therefore they always terminate. *)
type effect = Pure | Impure

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

  | TE_App of type_expr * type_expr
    (** Type application *)

  | TE_Option of type_expr
    (** Implicitly introduced Option type (in optional parameter) *)

(** Type-scheme expressions *)
and scheme_expr = {
  se_pos   : Position.t;
    (** Location of the scheme expression *)

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
        (** Type variable, that represents this ADT. *)

      proof  : var;
        (** An irrelevant variable that stores the proof that this ADT has
          the following constructors. *)

      args   : named_tvar list;
        (** List of type parameters of this ADT. *)

      ctors  : ctor_decl_expr list;
        (** List of constructors. *)

      effect : effect
        (** An effect indicating if the type is strictly positively recursive
          (in particular, not recursive at all). Strictly positively recursive
          types can be deconstructed in a pure way. *)
    }

  | DD_Label of (** Label *)
    { tvar      : tvar;
        (** Type variable that represents effect of this label *)

      var       : var;
        (** Regular variable that would store the label *)

      delim_tp  : typ
        (** Type of the delimiter *)
    }

(* ========================================================================= *)

(** Pattern *)
type pattern = pattern_data node
and pattern_data =
  | PWildcard of scheme
    (** Wildcard pattern -- it matches everything *)

  | PAs of pattern * var * scheme
    (** Pattern that binds a variable of given scheme, and continues with
      a subpattern *)

  | PCtor of string * int * expr * tvar list * pattern list * pattern list
    (** ADT constructor pattern. It stores a name, constructor index,
      proof that matched type is an ADT, existential type binders, patterns
      for named parameters, and patterns for regular parameters. *)

  | PAnnot of pattern * scheme_expr
    (** Scheme annotated pattern *)

(** Polymorphic expression *)
and poly_expr = poly_expr_data node
and poly_expr_data =
  | EOptionPrf
    (** The proof that Option is an ADT with None and Some constructors.
      It expects a single type: the type of the Some parameter. *)

  | EVar of var
    (** Variable *)

  | EPolyFun of tvar list * (var * scheme) list * expr
    (** Polymorphic lambda abstraction. Always pure *)

  | EHole of poly_expr option BRef.t
    (** Placeholder for a polymorphic expression, to be filled by constraint
      solving. *)

(** Expression *)
and expr = expr_data node
and expr_data =
  | EUnitPrf
    (** The proof that Unit is an ADT with only one constructor *)

  | EInst of poly_expr * type_expr list * poly_expr list
    (** Instantiation of polymorphic expression *)

  | ENum of int
    (** Integer literal *)

  | ENum64 of int64
    (** 64 bit integer literal *)

  | EStr of string
    (** String literal *)

  | EChr of char
    (** String literal *)

  | EFn of var * scheme * expr * effect
    (** Effect-annotated lambda-abstraction *)

  | EAppPoly of expr * poly_expr
    (** Function application to polymorphic expression *)

  | EAppMono of expr * expr
    (** Function application to regular, possibly effectful expression *)

  | ELetPoly of var * poly_expr * expr
    (** Polymorphic let-definition *)

  | ELetMono of var * expr * expr
    (** Monomorphic let-definition *)

  | ELetRec of rec_def list * expr
    (** Mutually recursive let-definitions *)

  | ECtor of expr * int * typ list * poly_expr list * poly_expr list
    (** Fully-applied constructor of ADT. The meaning of the parameters
      is the following.
      - Computationally irrelevant proof that given that the type of the
        whole expression is an ADT.
      - An index of the constructor.
      - Existential type parameters of the constructor.
      - Named parameters of the constructor.
      - Regular parameters of the constructor. *)

  | EData of data_def list * expr
    (** Definition of mutually recursive ADTs. *)

  | EMatchEmpty of expr * expr * typ * effect
    (** Pattern-matching of an empty type. The first parameter is an
      irrelevant expression, that is a witness that the type of the second
      parameter is an empty ADT. The last parameter is an effect of the whole
      expression. *)

  | EMatch of expr * match_clause list * typ * effect
    (** Pattern-matching. It stores type and effect of the whole expression.
      *)

  | EMatchPoly of poly_expr * pattern * expr * typ * effect
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

      effect    : tvar;
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

  | EAnnot of expr * type_expr * type_expr
    (** Expression explicitly annotated with a type and an effect. *)

  | ERepl of (unit -> expr) * typ
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      *)

  | EReplExpr of expr * expr
    (** Print the type of the first expression, evaluate and print the first
      expression, then continue to the second expression. *)

(** Definition of recursive value *)
and rec_def = var * scheme * poly_expr

(** Clause of a pattern matching *)
and match_clause = pattern * expr

(** Program *)
type program = expr

(* ========================================================================= *)
(** Operations on kind unification variables *)
module KUVar : sig

  (** Check for equality *)
  val equal : kuvar -> kuvar -> bool

  (** Set a unification variable. *) 
  val set : kuvar -> kind -> unit
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

  (** Arrow kind. *)
  val k_arrow : kind -> kind -> kind

  (** Create an arrow kind with multiple parameters. *)
  val k_arrows : kind list -> kind -> kind

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

  (** Create fresh type variable of given kind *)
  val fresh : kind -> tvar

  (** Check type variables for equality *)
  val equal : tvar -> tvar -> bool

  (** Get the unique identifier *)
  val uid : tvar -> UID.t

  (** Finite sets of type variables *)
  module Set : Set.S with type elt = tvar

  (** Finite map from type variables *)
  module Map : Map.S with type key = tvar

  (** Finite partial permutations of type variables *)
  module Perm : Perm.S with type key = tvar and module KeySet = Set
end

(* ========================================================================= *)
(** Operations on scopes of types *)
module Scope : sig
  (** Initial scope *)
  val initial : scope

  (** Extend scope with a variable *)
  val add : scope -> tvar -> scope

  (** Extend scope with a named type variable *)
  val add_named : scope -> named_tvar -> scope

  (** Check if a variable is a member of a scope *)
  val mem : scope -> tvar -> bool

  (** Apply permutation to a scope *)
  val perm : TVar.Perm.t -> scope -> scope

  (** Increase a level of given scope *)
  val incr_level : scope -> scope

  (** Get a level of given scope *)
  val level : scope -> int
end

(* ========================================================================= *)
(** Operations on unification variables *)
module UVar : sig
  type t = uvar

  (** Check unification variables for equality *)
  val equal : t -> t -> bool

  (** Get a unique identifier *)
  val uid : t -> UID.t

  (** Get a scope of a unification variable *)
  val scope : t -> scope

  (** Get a level of an unification variable *)
  val level : t -> int

  (** Set a unification variable, without checking any constraints. It returns
    expected scope of set type. The first parameter is a permutation attached
    to the unification variable. *)
  val raw_set : TVar.Perm.t -> t -> typ -> scope

  (** Promote unification variable to fresh type variable *)
  val fix : t -> tvar

  (** Shrink scope of given unification variable to given level, leaving only
    those variables which satisfy given predicate. *)
  val filter_scope : t -> int -> (tvar -> bool) -> unit

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
  val join : effect -> effect -> effect

  (** Join of multiple effects *)
  val joins : effect list -> effect
end

(* ========================================================================= *)
(** Operations on types *)
module Type : sig
  (** View of a type *)
  type type_view =
    | TEffect
      (** Something of effect kind. In Unif all effects are equal *)

    | TUVar of TVar.Perm.t * uvar
      (** Unification variable, associated with a delayed partial permutation,
      of type variables. Mappings of variables not in scope of the
      unification variable might be not present in the permutation. *)

    | TVar of tvar
      (** Regular type variable *)

    | TArrow of scheme * typ * effect
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

  (** Head of a neutral type *)
  type neutral_head =
    | NH_UVar of TVar.Perm.t * uvar
      (** Unification variable *)

    | NH_Var  of tvar
      (** Regular variable *)

  (** Weak head normal form of a type *)
  type whnf =
    | Whnf_Effect
      (** Effect *)

    | Whnf_Neutral of neutral_head * typ list
      (** Neutral type. Its parameters are in reversed order. *)

    | Whnf_Arrow of scheme * typ * effect
      (** Effect annotated arrow *)
  
    | Whnf_Handler   of tvar * typ * typ * typ
      (** Handler type *)

    | Whnf_Label of typ
      (** Label type *)

  (** Effect *)
  val t_effect : typ

  (** Unit type *)
  val t_unit : typ

  (** Option type *)
  val t_option : typ -> typ

  (** Unification variable *)
  val t_uvar : TVar.Perm.t -> uvar -> typ

  (** Regular type variable *)
  val t_var : tvar -> typ

  (** Pure arrow type *)
  val t_pure_arrow : scheme -> typ -> typ

  (** Pure arrow, that takes multiple arguments *)
  val t_pure_arrows : scheme list -> typ -> typ

  (** Arrow type *)
  val t_arrow : scheme -> typ -> effect -> typ

  (** Type of first-class handlers *)
  val t_handler : tvar -> typ -> typ -> typ -> typ

  (** Type of first-class label *)
  val t_label : typ -> typ

  (** Type application *)
  val t_app : typ -> typ -> typ

  (** Type application to multiple arguments *)
  val t_apps : typ -> typ list -> typ

  (** Fresh unification variable (packed as type) *)
  val fresh_uvar : scope:scope -> kind -> typ

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
  val try_shrink_scope : scope:scope -> typ -> (unit, tvar) result
end

(* ========================================================================= *)
(** Operations on type schemes *)
module Scheme : sig
  (** Create a monomorphic type-scheme *)
  val of_type : typ -> scheme

  (** Convert scheme to monomorphic type. Returns [None], when the scheme is
    polymorphic. *)
  val to_type : scheme -> typ option

  (** Check if given type-scheme is monomorphic *)
  val is_monomorphic : scheme -> bool

  (** Set of unification variables in given scheme *)
  val uvars : scheme -> UVar.Set.t

  (** Extend given set of unification variables by unification variables
    from given scheme. Equivalent to [uvars sch UVar.Set.empty] *)
  val collect_uvars : scheme -> UVar.Set.t -> UVar.Set.t

  (** Make sure that type variables bound by this scheme are fresh *)
  val refresh : scheme -> scheme

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

  (** Check if given constructor is strictly positive, i.e., if all type
    variables on non-strictly positive positions and all scopes of unification
    variables fit in [nonrec_scope]. *)
  val strictly_positive : nonrec_scope:scope -> ctor_decl -> bool
end

(* ========================================================================= *)
(** Type substitutions *)
module Subst : sig
  (** Empty substitution *)
  val empty : subst

  (** Extend substitution with a renaming. The new version of a variable
    must be fresh enough. *)
  val rename_to_fresh : subst -> tvar -> tvar -> subst

  (** Extend substitution. It is rather parallel extension than composition *)
  val add_type : subst -> tvar -> typ -> subst
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
