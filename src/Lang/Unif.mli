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

  | TNEffect
    (** Effect variable associated with effect handler *)

  | TNVar of string
    (** Regular named type parameter *)

(** Named type parameter *)
type named_tvar = tname * tvar

(** Name of a named parameter *)
type name =
  | NLabel
    (** Dynamic label of a handler *)

  | NVar      of string
    (** Regular named parameter *)

  | NOptionalVar of string
    (** Optional named parameter *)

  | NImplicit of string
    (** Implicit parameter *)

  | NMethod   of string
    (** Name of methods **)

(** Types.

  This is an abstract type. Use [Type.view] to view it. *)
type typ

(** Effects. They are represented as types effect kind. Always ground. *)
type effect = typ

(** Effect rows. *)
type effrow = typ

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

(** Type substitution *)
type subst

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

(** Variable *)
type var = Var.t

(** Data-like definition (ADT or label) *)
type data_def =
  | DD_Data of (** Algebraic datatype *)
    { tvar  : tvar;
        (** Type variable, that represents this ADT. *)

      proof : var;
        (** An irrelevant variable that stores the proof that this ADT has
          the following constructors. *)

      args  : named_tvar list;
        (** List of type parameters of this ADT. *)

      ctors : ctor_decl list
        (** List of constructors. *)
    }

  | DD_Label of (** Label *)
    { tvar      : tvar;
        (** Type variable that represents effect of this label *)

      var       : var;
        (** Regular variable that would store the label *)

      delim_tp  : typ;
        (** Type of the delimiter *)

      delim_eff : effrow
        (** Effect of the delimiter *)
    }

(* ========================================================================= *)

(** Pattern *)
type pattern = pattern_data node
and pattern_data =
  | PWildcard
    (** Wildcard pattern -- it matches everything *)

  | PVar of var * scheme
    (** Pattern that binds a variable of given scheme *)

  | PCtor of string * int * expr * ctor_decl list * tvar list * pattern list
    (** ADT constructor pattern. It stores a name, constructor index,
      proof that matched type is an ADT, full list of constructor of an ADT,
      existential type binders, and patterns for parameters. *)

(** Expression *)
and expr = expr_data node
and expr_data =
  | EUnitPrf
    (** The proof that Unit is an ADT with only one constructor *)

  | ENum of int
    (** Integer literal *)

  | ENum64 of int64
    (** 64 bit integer literal *)

  | EStr of string
    (** String literal *)

  | EChr of char
    (** String literal *)

  | EVar of var
    (** Variable *)

  | EPureFn of var * scheme * expr
    (** Pure lambda-abstraction *)

  | EFn of var * scheme * expr
    (** Impure lambda-abstraction *)

  | ETFun of tvar * expr
    (** Type function *)

  | EApp of expr * expr
    (** Function application *)

  | ETApp of expr * typ
    (** Type application *)

  | ELet of var * scheme * expr * expr
    (** Let-definition *)

  | ELetRec of rec_def list * expr
    (** Mutually recursive let-definitions *)

  | ECtor of expr * int * typ list * expr list
    (** Fully-applied constructor of ADT. The meaning of the parameters
      is the following.
      - Computationally irrelevant proof that given that the type of the
        whole expression is an ADT.
      - An index of the constructor.
      - Existential type parameters of the constructor.
      - Regular parameter of the constructor, including named and implicit. *)

  | EData of data_def list * expr
    (** Definition of mutually recursive ADTs. *)

  | EMatchEmpty of expr * expr * typ * effrow
    (** Pattern-matching of an empty type. The first parameter is an
      irrelevant expression, that is a witness that the type of the second
      parameter is an empty ADT *)

  | EMatch of expr * match_clause list * typ * effrow
    (** Pattern-matching. It stores type and effect of the whole expression. *)

  | EHandle of (** Handler *)
    { label : expr;
      (** Label of the handler *)

      effect : effect;
      (** Effect handled by the label *)

      cap_var : var;
      (** Variable that binds effect capability *)

      body : expr;
      (** Handled expression *)

      capability : expr;
      (** An expression providing capability to this handler *)

      ret_var : var;
      (** An argument to the return clause *)

      ret_body : expr;
      (** A body of the return clause *)

      result_tp : typ;
      (** The type of the whole handler *)

      result_eff : effrow }
      (** The effect of the whole handler *)

  | EHandler of tvar * var * typ * effrow * expr
    (** First-class handler. In [EHandler(a, lx, tp, eff, h)] the meaning of
      parameter is the following:
      - [a] -- binder of an effect variable (of kind [effect]). The variable
        is bound in other arguments of [EHandler] expression.
      - [lx] -- label variable bound by the handler. Its type should be
        [TLabel(Effect.singleton a, tp, eff)].
      - [tp] -- type of the delimiter ([EHandle]).
      - [eff] -- effect of the delimiter.
      - [h] -- body of the handler. *)

  | EEffect of expr * var * expr * typ
    (** Capability of effectful functional operation. It stores dynamic label,
      continuation variable, body, and the type of the whole expression. *)

  | EExtern of string * typ
    (** Externally defined value *)

  | ERepl of (unit -> expr) * typ * effrow
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      This constructor stores also type and effect of a REPL expression. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Definition of recursive value *)
and rec_def = var * scheme * expr

(** Clause of a pattern matching *)
and match_clause = pattern * expr

(** Program *)
type program = expr

(* ========================================================================= *)
(** Operations on kind unification variables *)
module KUVar : sig

  (** Check for equality *)
  val equal : kuvar -> kuvar -> bool

  (** Set a unification variable. Returns false if given kind does not
    satisfy non-effect constraints. *)
  val set : kuvar -> kind -> bool

  (** Set a unification variable. This function can be called only on
    non-effect kinds *)
  val set_safe : kuvar -> kind -> unit
end

(* ========================================================================= *)
(** Operations on kinds *)
module Kind : sig
  (** View of kinds *)
  type kind_view =
    | KType
      (** Kind of all types *)

    | KEffect
      (** Kind of all (closed) effects. Closed effects cannot contain
        unification variables. *)
    
    | KEffrow
      (** Kind of all effect rows. *)

    | KUVar of kuvar
      (** Unification variable *)

    | KArrow of kind * kind
      (** Arrow kind *)

  (** Kind of all types *)
  val k_type : kind

  (** Kind of all (closed) effects. Closed effects cannot contain unification
    variables. *)
  val k_effect : kind

  (** Kind of effect rows. Rows contain at most one unification variable or
    type application. *)
  val k_effrow : kind

  (** Arrow kind. The result kind (the second parameter) must be non-effect
    kind. *)
  val k_arrow : kind -> kind -> kind

  (** Create an arrow kind with multiple parameters. *)
  val k_arrows : kind list -> kind -> kind

  (** Create a fresh unification kind variable. The optional [non_effect]
    flag indicates whether this kind cannot be instantiated to effect kind.
    The default value is false, i.e., this kind can be an effect kind. *)
  val fresh_uvar : ?non_effect:bool -> unit -> kind

  (** Reveal a top-most constructor of a kind *)
  val view : kind -> kind_view

  (** Check if given kind contains given unification variable *)
  val contains_uvar : kuvar -> kind -> bool

  (** Check if given kind cannot be effect kind, even if it is a unification
    variable. The main purpose of this function is to use it in assert
    statements. *)
  val non_effect : kind -> bool

  (** Check whether given kind is an effect kind (but not unification
    variable). *)
  val is_effect : kind -> bool

  (** Add non-effect constraint to given kind. Returns true on success.
    Returns false if given kind is an effect kind. *)
  val set_non_effect : kind -> bool
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

  (** Substitute in name *)
  val subst : subst -> name -> name

  (** Finite sets of names *)
  module Set : Set.S with type elt = name

  (** Finite maps with names as keys *)
  module Map : Map.S with type key = name
end

(* ========================================================================= *)
(** Operations on types *)
module Type : sig
  (** End of an effect *)
  type effrow_end =
    | EEClosed
      (** Closed effect row *)
    
    | EEUVar of TVar.Perm.t * uvar
      (** Open effect row with unification variable at the end. The unification
        variable is associated with a delayed partial permutation of type
        variables. *)

    | EEVar of tvar
      (** Open effect row with type variable at the end *)

    | EEApp of typ * typ
      (** Type application of an [effrow] kind *)

  (** View of a type *)
  type type_view =
    | TUVar of TVar.Perm.t * uvar
      (** Unification variable, associated with a delayed partial permutation,
      of type variables. Mappings of variables not in scope of the
      unification variable might be not present in the permutation. *)

    | TVar of tvar
      (** Regular type variable *)

    | TEffect of TVar.Set.t
      (** (Ground) effect *)

    | TEffrow of TVar.Set.t * effrow_end
      (** Effect: a set of simple effect variables together with a way of
        closing an effect *)

    | TPureArrow of scheme * typ
      (** Pure arrow, i.e., type of function that doesn't perform any effects
        and always terminate *)

    | TArrow of scheme * typ * effrow
      (** Impure arrow *)
  
    | THandler of tvar * typ * typ * effrow
      (** First class handler. In [THandler(a, tp, tp0, eff0)]:
        - [a] is a variable bound in [tp], [tp0], and [eff0];
        - [tp] is a type of provided capability;
        - [tp0] and [eff0] are type and effects of the whole delimiter. *)
  
    | TLabel of effect * typ * effrow
      (** Type of first-class label. It stores the effect of the label and
        type and effect of the delimiter. *)

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
    | Whnf_Neutral of neutral_head * typ list
      (** Neutral type. Its parameters are in reversed order. *)

    | Whnf_Effect  of effect
      (** Effect *)

    | Whnf_Effrow of effrow
      (** Effect rows *)

    | Whnf_PureArrow of scheme * typ
      (** Pure arrow *)
  
    | Whnf_Arrow of scheme * typ * effrow
      (** Impure arrow *)
  
    | Whnf_Handler of tvar * typ * typ * effrow
      (** Handler type *)

    | Whnf_Label of effect * typ * effrow
      (** Label type *)

  (** Unit type *)
  val t_unit : typ

  (** Unification variable *)
  val t_uvar : TVar.Perm.t -> uvar -> typ

  (** Regular type variable *)
  val t_var : tvar -> typ

  (** Pure arrow type *)
  val t_pure_arrow : scheme -> typ -> typ

  (** Pure arrow, that takes multiple arguments *)
  val t_pure_arrows : scheme list -> typ -> typ

  (** Arrow type *)
  val t_arrow : scheme -> typ -> effrow -> typ

  (** Type of first-class handlers *)
  val t_handler : tvar -> typ -> typ -> effrow -> typ

  (** Type of first-class label *)
  val t_label : effect -> typ -> effrow -> typ

  (** Create an effect *)
  val t_effect : TVar.Set.t -> effect

  (** Create an effect row *)
  val t_effrow : TVar.Set.t -> effrow_end -> effrow

  (** Create a closed effect row *)
  val t_closed_effrow : TVar.Set.t -> effrow

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

  (** Reveal a representation of an effect: a set of effect variables. *)
  val effect_view : effect -> TVar.Set.t

  (** Reveal a representation of an effect row: a set of effect variables
    together with a way of closing an effect row. *)
  val effrow_view : effrow -> TVar.Set.t * effrow_end

  (** Get the kind of given type *)
  val kind : typ -> kind

  (** Returns subtype of given type, where all closed rows on negative
    positions are opened by a fresh unification variable. It works only
    for proper types (of kind type) *)
  val open_down : scope:scope -> typ -> typ

  (** Returns supertype of given type, where all closed rows on positive
    positions are opened by a fresh unification variable. It works only
    for proper types (of kind type) *)
  val open_up : scope:scope -> typ -> typ

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
(** Operations on effects *)
module Effect : sig
  (** View of effect row *)
  type row_view =
    | RPure
      (** Pure effect row *)

    | RUVar of TVar.Perm.t * uvar
      (** Row unification variable (with delayed permutation) *)

    | RVar  of tvar
      (** Row variable *)

    | RApp  of typ * typ
      (** Type application of an [effrow] kind *)

    | RCons of tvar * effrow
      (** Consing an simple effect variable to an effect row *)

  (** Pure effect row *)
  val pure : effrow

  (** Effect row with single IO effect *)
  val io : effrow

  (** Create a row that consists of single effect *)
  val singleton_row : tvar -> effrow

  (** Consing a single effect variable to an effect row *)
  val cons : tvar -> effrow -> effrow

  (** Consing an effect to an effect row *)
  val cons_eff : effect -> effrow -> effrow

  (** Row-like view of an effect row *)
  val view : effrow -> row_view

  (** View the end of given effect row. *) 
  val view_end : effrow -> Type.effrow_end

  (** Check if an effect row is pure *)
  val is_pure : effrow -> bool
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
(** Operations on type schemes *)
module Scheme : sig
  (** Create a monomorphic type-scheme *)
  val of_type : typ -> scheme

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

  (** List of all built-in types with their names *)
  val all : (string * tvar) list
end
