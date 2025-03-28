(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The ConE (constraints on effects) language: the result of the
  effect-inference phase. *)

(** Formulas used for effect guards *)
type formula = IncrSAT.Formula.t

(** Kinds (shared with Unif) *)
type kind = UnifCommon.Kind.t

(** Name of type variable *)
type tname = UnifCommon.Names.tname

(** Name of named parameter *)
type name = UnifCommon.Names.name

(** Type variables *)
type tvar

(** Named type variable *)
type named_tvar = tname * tvar

(** Generalizable effect variable *)
type gvar

(** Composable effects *)
type effct

(** Computation effect of an expression. Purity implies termination. *)
type ceffect =
  | Pure
  | Impure of effct

(** Abstract type that represents a type. Use [Type.view] to access the
  top-most constructor. *)
type typ

(** Polymorphic type scheme *)
type scheme =
  { sch_targs : named_tvar list;
    (** Universally quantified type variables *)

    sch_named : named_scheme list;
    (** Schemes of named parameters *)

    sch_body  : typ
    (** Scheme body *)
  }

(** Named type scheme *)
and named_scheme = name * scheme

(** Declaration of ADT constructor *)
type ctor_decl =
  { ctor_name        : string;
      (** Name of the constructor *)

    ctor_targs       : named_tvar list;
      (** Existential type variables of the constructor *)

    ctor_named       : named_scheme list;
      (** Named parameters of the constructor *)

    ctor_arg_schemes : scheme list
      (** Type schemes of the regular parameters *)
  }

(** Subeffecting constraint, stating that the first effect is a subeffect
  of the second. *)
type constr = effct * effct

(** Substitutions of types for type variables *)
type subst

(* ========================================================================= *)

(** Variable *)
type var = Var.t

(** Data-like definition (ADT or label) *)
type data_def =
  | DD_Data of (** Algebraic datatype *)
    { tvar  : tvar;
        (** Type variable that represents the ADT *)

      proof : var;
        (** An irrelevant variable that stores the proof that this ADT has
          the following constructors *)

      args  : named_tvar list;
        (** List of type parameters of the ADT *)

      ctors : ctor_decl list;
        (** List of constructors of the ADT *)

      positive : bool
        (** A flag indicating if the type is positively recursive (in
          particular, not recursive at all) and therefore can be deconstructed
          without performing NTerm effect. *)
    }

  | DD_Label of (** First-class label *)
    { tvar      : tvar;
        (** Type variable that represents effect of this label *)

      var       : var;
        (** Regular variable that would store the label *)

      delim_tp  : typ;
        (** Type of the delimiter *)

      delim_eff : effct
        (** Effect of the delimiter *)
    }

(** Expressions *)
type expr =
  | EUnitPrf
    (** ADT-shape proof for unit type *)

  | EOptionPrf
    (** ADT-shape proof for option type *)

  | ENum of int
    (** Integer literal *)

  | ENum64 of int64
    (** 64 bit integer literal *)

  | EStr of string
    (** String literal *)

  | EChr of char
    (** Character literal *)

  | EVar of var
    (** Variable *)

  | EFn of var * scheme * expr
    (** Lambda abstraction *)

  | ETFun of tvar * expr
    (** Type abstraction *)

  | ECAbs of constr list * expr
    (** Constraint abstraction *)

  | EApp of expr * expr
    (** Function application *)

  | ETApp of expr * typ
    (** Type application *)

  | ECApp of expr
    (** Instantiation of constraint abstraction *)

  | ELet of var * expr * expr
    (** Let-binding *)

  | ELetPure of var * expr * expr
    (** Let-binding of a pure expression *)

  | ELetRec of rec_def list * expr
    (** Recursive let-binding *)

  | ERecCtx of expr
    (** Context for mutually recursive definitions. It is always impure, and
      from that point recursive definitions become visible. It is also used
      to mark the place where less polymorphic variables should be let-bound
      during the translation from Unif. *)

  | EData of data_def list * expr
    (** Mutually recursive datatype definitions *)

  | ECtor of expr * int * typ list * expr list
    (** Fully applied ADT constructor. The first argument is the ADt shape
      proof, and the second argument is the index of the constructor. The
      remaining arguments are the constructor arguments. *)

  | EMatch of expr * expr * match_clause list * typ * ceffect
    (** Pattern matching. The first parameter is the proof that the type of
      the matched value is an ADT *)

  | EShift of expr * var * expr * typ
    (** Shift-0 operator parametrized by runtime tag, binder for continuation
      variable, body, and the type of the whole expression. *)

  | EReset of expr * expr * var * expr
    (** Reset-0 operator parametrized by runtime tag, body, and the return
      clause *)

  | EExtern of string * typ
    (** Externally defined value *)

  | ERepl of (unit -> expr) * typ * ceffect
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Recursive definition *)
and rec_def =
  { rd_var    : var;
      (** Variable that stores recursive value. *)

    rd_body   : expr;
      (** Body of the recursive definition. It should already abstract
        [rd_evars] and [rd_constr]. *)

    rd_evars  : tvar list;
      (** Effect variables abstracted in the recursive value. *)

    rd_constr : constr list;
      (** Constraints on effect variables *)

    rd_scheme : scheme
      (** Scheme of the body. *)
  }

(** Pattern-matching clause *)
and match_clause =
  { cl_tvars : tvar list;
    (** Type variables introduced by the clause *)

    cl_vars  : var list;
    (** Variables introduced by the clause *)

    cl_body  : expr
    (** Body of the clause *)
  }

(** Programs *)
type program = expr

(* ========================================================================= *)
(** Operations on type variables *)
module TVar : sig
  (** Kind of a type variable *)
  val kind : tvar -> kind

  (** Create fresh type variable of the effect kind. *)
  val fresh_eff : scope:Scope.t -> tvar

  (** Fresh type variable, that uses metadata (ppuid, kind) from the given
    Unif type variable *)
  val clone_unif : scope:Scope.t -> UnifCommon.TVar.t -> tvar

  (** Fresh type variable that uses metadata from the given type variable *)
  val clone : scope:Scope.t -> tvar -> tvar

  (** Check equality of type variables *)
  val equal : tvar -> tvar -> bool

  (** Check if type variable belongs to the given scope. *)
  val in_scope : tvar -> Scope.t -> bool

  (** Finite sets of type variables *)
  module Set : Set.S with type elt = tvar

  (** Finite map from type variables *)
  module Map : Map.S with type key = tvar

  (** Pretty-print type variable as S-expression *)
  val to_sexpr : tvar -> SExpr.t
end

(* ========================================================================= *)
(** Operations on generalizable variables *)
module GVar : sig
  (** Get the scope of a generalizable variable *)
  val scope : gvar -> Scope.t

  (** Check if generalizable variable belongs to the given scope. *)
  val in_scope : gvar -> Scope.t -> bool

  (** Update a scope of generalizable variable. The new scope should be
    an ancestor of the current one. Additionally, the list [tvars] of "bound"
    variables might be passed (all of them must have effect kind). The
    generalizable variable is substituted with effect [Y,X1?p1, ..., Xn?pn]
    where [Y] is a fresh generalizable variable at the new scope, [X1...Xn]
    are those "bound" variables that are in the current scope of a
    generalizable variable. Each of them appears conditionally as described
    by formulas [p1,...,pn], that are pairwise distinct fresh propositional
    variables. Function returns [Y] variable, which might be the same as
    the current one, when there are no bound variables to add. *)
  val update_scope : scope:Scope.t -> tvars:tvar list -> gvar -> gvar

  (** Globally substitute an effect for a given generalizable variable.
    The effect should belongs to the scope of the variable. This operation
    can be done only once for each generalizable variable. *)
  val set : gvar -> effct -> unit

  (** Promote to regular type variable. *)
  val fix : gvar -> tvar

  (** Pretty-print the generalizable variable as S-expression. *)
  val to_sexpr : gvar -> SExpr.t

  (** Finite sets of generalizable variables *)
  module Set : Set.S with type elt = gvar

  (** Finite maps from generalizable variables *)
  module Map : Map.S with type key = gvar
end

(* ========================================================================= *)
(** Operations on effects *)
module Effct : sig
  (** Pure effect. It still allows non-termination. *)
  val pure : effct

  (** Effect that contains a single effect variable. *)
  val var : tvar -> effct

  (** Effect that contains a single generalizable variable *)
  val gvar : gvar -> effct

  (** Join of two effects *)
  val join : effct -> effct -> effct

  (** Create effect that is equal to the given effect, when the formula is
    satisfied, and equal to the pure effect when the formula is not satisfied.
    *)
  val guard : effct -> formula -> effct

  (** Remove all components of the effect that do not belong to the given
    scope. *)
  val filter_to_scope : scope:Scope.t -> effct -> effct

  (** Generate a fresh generalizable variable and convert it into an effect. *)
  val fresh_gvar : scope:Scope.t -> effct

  (** Reveal the representation of the effect: two list of atomic effects
    with predicates that states if the atomic effect belongs to the effect. *)
  val view : effct -> (tvar * formula) list * (gvar * formula) list

  (** Lookup a type variable in the effect. Returns formula that indicates if
    a type variable is included in the effect. *)
  val lookup_tvar : effct -> tvar -> formula

  (** Lookup a generalizable variable in the effect. Returns formula that
    indicates if a generalizable variable is included in the effect. *)
  val lookup_gvar : effct -> gvar -> formula

  (** Collect all generalizable variables that do not belong to the given
    scope and add them to the given set. *)
  val collect_gvars : scope:Scope.t -> effct -> GVar.Set.t -> GVar.Set.t

  (** Apply the substitution to the effect. *)
  val subst : subst -> effct -> effct

  (** Pretty-print the effect as S-expression. *)
  val to_sexpr : effct -> SExpr.t
end

(* ========================================================================= *)
(** Operations on computation effects *)
module CEffect : sig
  (** Expected effect of the whole program *)
  val prog_effect : ceffect

  (** Join two computation effects. *)
  val join : ceffect -> ceffect -> ceffect

  (** Collect all generalizable variables that do not belong to the given
    scope and add them to the given set. *)
  val collect_gvars : scope:Scope.t -> ceffect -> GVar.Set.t -> GVar.Set.t
end

(* ========================================================================= *)
(** Operations on types *)
module Type : sig
  (** View of the type. *)
  type type_view =
    | TVar     of tvar
      (** Regular type variable *)

    | TArrow   of scheme * typ * ceffect
      (** Arrow type *)

    | TLabel   of effct * typ * effct
      (** Label type with the delimited effect, and the type type and effect
        of the delimiter. *)

    | THandler of (** Handler type *)
      { tvar    : tvar;
          (** Type variable that represents the handled effect. *)

        cap_tp  : typ;
          (** Capability type. *)

        in_tp   : typ;
          (** Type of the inner computation. *)

        in_eff  : effct;
          (** Effect of the inner computation. Usually it contains [tvar]. *)

        out_tp  : typ;
          (** Type of the result. *)

        out_eff : effct
          (** Effect of the result. *)
      }

    | TEffect  of effct
      (** Effect *)

    | TApp     of typ * typ
      (** Type application *)

  (** Regular type variable *)
  val t_var : tvar -> typ

  (** Arrow type *)
  val t_arrow : scheme -> typ -> ceffect -> typ

  (** Pure arrow *)
  val t_pure_arrow : scheme -> typ -> typ

  (** Sequence of pure arrows *)
  val t_pure_arrows : scheme list -> typ -> typ

  (** Handler type *)
  val t_handler : tvar -> typ -> typ -> effct -> typ -> effct -> typ

  (** Label type *)
  val t_label : effct -> typ -> effct -> typ

  (** Type application *)
  val t_app : typ -> typ -> typ

  (** Sequence of type applications *)
  val t_apps : typ -> typ list -> typ

  (** Effect *)
  val t_effect : effct -> typ

  (** Reveal the top-most constructor of the type. *)
  val view : typ -> type_view

  (** Convert the type (of the effect kind) to an effect. *)
  val to_effect : typ -> effct

  (** Collect all generalizable variables that do not belong to the given
    scope and add them to the given set. *)
  val collect_gvars : scope:Scope.t -> typ -> GVar.Set.t -> GVar.Set.t

  (** Apply the substitution to the type. *)
  val subst : subst -> typ -> typ

  (** Pretty-print type as S-expression *)
  val to_sexpr : typ -> SExpr.t
end

(* ========================================================================= *)
(** Operations on type schemes *)
module Scheme : sig
  (** Check if the scheme is monomorphic. *)
  val is_monomorphic : scheme -> bool

  (** Convert a type to a monomorphic scheme *)
  val of_type : typ -> scheme

  (** Convert scheme to monomorphic type. Returns [None], when the scheme is
    polymorphic. *)
  val to_type : scheme -> typ option

  (** Collect all generalizable variables that do not belong to the given
    scope and add them to the given set. *)
  val collect_gvars : scope:Scope.t -> scheme -> GVar.Set.t -> GVar.Set.t

  (** Collect all generalizable variables that do not belong to the given
    scope and add the to the given sets, depending on their polarity. The
    former set stores non-negative variables, while the latter stores
    non-positive variables. Invariant variables are added to both sets. *)
  val collect_gvars_p : scope:Scope.t -> scheme ->
    GVar.Set.t * GVar.Set.t -> GVar.Set.t * GVar.Set.t

  (** Apply the substitution to the scheme. *)
  val subst : subst -> scheme -> scheme

  (** Pretty-print type scheme as S-expression *)
  val to_sexpr : scheme -> SExpr.t
end

(* ========================================================================= *)
(** Operations on constructor declarations *)
module CtorDecl : sig
  (** Collect all generalizable variables that do not belong to the given
    scope and add them to the given set. *)
  val collect_gvars : scope:Scope.t -> ctor_decl -> GVar.Set.t -> GVar.Set.t

  (** Apply the substitution to the constructor declaration. *)
  val subst : subst -> ctor_decl -> ctor_decl
end

(* ========================================================================= *)
(** Operations on constraints *)
module Constr : sig
  (** Pretty-print constraint as S-expression *)
  val to_sexpr : constr -> SExpr.t
end

(* ========================================================================= *)
(** Operations on substitutions *)
module Subst : sig
  (** Empty substitution *)
  val empty : subst

  (** Add a renaming to the substitution. [rename sub x y] is equivalent to
    [add sub x (Type.t_var y)]. *)
  val rename : subst -> tvar -> tvar -> subst

  (** Add a type to the substitution. *)
  val add : subst -> tvar -> typ -> subst

  (** Create a new substitution that maps given type variables to the
    corresponding types. Both list must have equal lengths. *)
  val for_named_tvars : named_tvar list -> typ list -> subst
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

(** Produce S-expression representation of the program *)
val to_sexpr : program -> SExpr.t
