(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Core Language. *)

(** Type-level constructor of a kind of all types. The main purpose of this
  type is to encode kind-system in OCaml's GADT. Values of this types are
  never used. *)
type ktype = Dummy_Ktype

(** Type-level constructor of a kind of all effects. *)
type keffect = Dummy_Keffect

(** Kinds, self-indexed thanks to GADT *)
type 'k kind =
  | KType : ktype kind
    (** Kind of all types *)

  | KEffect : keffect kind
    (** Kind of all effects *)

  | KArrow : 'k1 kind * 'k2 kind -> ('k1 -> 'k2) kind
    (** Arrow kind *)

(** Type variable, indexed by its kind *)
type 'k tvar

(** Operations on type variables *)
module TVar : sig
  type 'k t = 'k tvar

  (** Create a fresh type variable of given kind *)
  val fresh : 'k kind -> 'k tvar

  (** Create an exact copy (with different UID) of a type variable *)
  val clone : 'k tvar -> 'k tvar

  (** Get the kind of given type variable *)
  val kind : 'k tvar -> 'k kind

  (** Existential version of type variable, where its kind is packed *)
  type ex = Ex : 'k tvar -> ex

  (** Finite maps from type variables *)
  module Map : Map1.S with type 'k key = 'k t
end

(** The flag indicating positivity of the recursion *)
type rflag =
  | Positive
    (** The type is positively recursive. Unfolding it does not perform
      non-termination effect. *)

  | General
    (** No assumptions about the positivity of the recursion. Unfolding
      introduces non-termination effect. *)

(** The flag indicating relevance of the value. *)
type relevance =
  | Relevant
    (** The value is relevant, i.e., it is a regular value. *)

  | Irrelevant
    (** The value is computationally irrelevant. The computation cannot make
      any decision based on it. *)

(** Types, indexed by a type-represented kind *)
type _ typ =
  | TEffPure : keffect typ
    (** Pure effect *)

  | TEffJoin : effct * effct -> keffect typ
    (** Join of two effects. Avoid using this constructor directly: Function
      [Effect.join] provides similar functionalily, but removes duplicates. *)

  | TVar  : 'k tvar -> 'k typ
    (** Type variable *)

  | TArrow  : ttype * ttype * effct -> ktype typ
    (** Arrow type *)

  | TForall : 'k tvar * ttype -> ktype typ
    (** Polymorphic type *)

  | TGuard : constr list * ttype -> ktype typ
    (** Type of constraint abstraction *)

  | TLabel : (** Type of the first class label *)
    { effct     : effct;
        (** The effect of this label *)

      tvars     : TVar.ex list;
        (** List of types stored at each delimiter of this label *)

      val_types : ttype list;
        (** List of types of values stored at each delimiter of this label *)

      delim_tp  : ttype;
        (** Type of the delimiter *)

      delim_eff : effct;
        (** Effect of the delimiter *)

      rflag : rflag
        (** A flag indicating if the effect is positively recursive, and
          therefore can be handled without performing NTerm effect. *)
    } -> ktype typ

  | TData    : ttype * rflag * ctor_type list -> ktype typ
    (** Proof of the shape of ADT.
      
      Algebraic data type (ADTs) are just abstract types, but each operation
      on them like constructors or pattern-matching requires additional
      computationally irrelevant parameter of type that describes the shape
      of ADTs. This approach simplifies many things, e.g., mutually recursive
      types are not recursive at all!

      The element of type [TData(tp, r, ctors)] is a witness that type [tp]
      has constructors [ctors]. When the positivity flag [r] is set to
      [Positive], the patter-matching is pure. Otherwise it introduces [NTerm]
      effect. *)

  | TApp     : ('k1 -> 'k2) typ * 'k1 typ -> 'k2 typ
    (** Type application *)

(** Proper types *)
and ttype = ktype typ

(** Effects *)
and effct = keffect typ

(** Constraints *)
and constr = keffect typ * keffect typ

(** ADT constructor type *)
and ctor_type = {
  ctor_name      : string;
    (** Name of the constructor *)

  ctor_tvars     : TVar.ex list;
    (** Existential parameters of the constructor *)

  ctor_arg_types : ttype list
    (** Types of constructor arguments *)
}

(** Variables *)
type var = Var.t

(** Data-like definition (ADT or label) *)
type data_def =
  | DD_Data of (** Algebraic datatype *)
    { tvar  : TVar.ex;
        (** Type variable, that represents this ADT. *)

      proof : var;
        (** An irrelevant variable that stores the proof that this ADT has
          the following constructors. *)

      args  : TVar.ex list;
        (** List of type parameters of this ADT. *)

      ctors : ctor_type list;
        (** List of constructors. *)

      rflag : rflag
        (** A flag indicating if the type is positively recursive (in
          particular, not recursive at all) and therefore can be deconstructed
          without performing NTerm effect. *)
    }

  | DD_Label of (** Label *)
    { tvar      : keffect tvar;
        (** Type variable that represents effect of this label *)

      var       : var;
        (** Regular variable that would store the label *)

      tvars     : TVar.ex list;
        (** List of existential types stored at each delimiter of this label. *)

      val_types : ttype list;
        (** List of types of values stored at each delimiter of this label. *)

      delim_tp  : ttype;
        (** Type of the delimiter *)

      delim_eff : effct;
        (** Effect of the delimiter *)

      rflag : rflag
        (** A flag indicating if the effect is positively recursive, and
          therefore can be handled without performing NTerm effect. *)
    }

(* ========================================================================= *)
(** Operations on kinds *)
module Kind : sig
  (** Existential version of kind, where its kind index is packed *)
  type ex = Ex : 'k kind -> ex

  (** Check for equality of kinds *)
  val equal : 'k1 kind -> 'k2 kind -> ('k1, 'k2) Eq.t
end

(* ========================================================================= *)
(** Operations on types *)
module Type : sig
  (** Get the kind of given type *)
  val kind : 'k typ -> 'k kind

  (** Create a sequence of pure arrows *)
  val t_pure_arrows : ttype list -> ttype -> ttype

  (** Create a sequence of universal types *)
  val t_foralls : TVar.ex list -> ttype -> ttype

  (** Unit type *)
  val t_unit : ttype

  (** Bool type *)
  val t_bool : ttype

  (** Option type *)
  val t_option : ttype -> ttype

  (** Existential version of type representation, where its kind is packed *)
  type ex = Ex : 'k typ -> ex
end

(* ========================================================================= *)
(** Operations on effects *)
module Effect : sig
  (** Join of two effects. Same as TEffJoin constructor, but removes
    duplicates. *)
  val join : effct -> effct -> effct

  (** Effect of possible non-termination *)
  val nterm : effct
end

(* ========================================================================= *)
(** Built-in types *)
module BuiltinType : sig
  (** List of all built-in types together with their names *)
  val all : (string * TVar.ex) list
end

(* ========================================================================= *)

(** Literals *)
type lit =
  | LNum of int
    (** Integer literal *)

  | LNum64 of int64
    (** 64 bit integer literal *)

  | LStr of string
    (** String literal *)

(** Expressions.

  Expressions are presented in slightly relaxed A-normal form. The main
  difference is that any expression can be used as the left-hand side of
  a function application, provided that it is pure. This approach still has
  most of the benefits of A-normal form, but it able to keep track of
  application to mutiple arguments. Moreover, irrelevant subexpressions
  doesn't need to be values. **)
type expr =
  | EValue of value
    (** value *)

  | ELet of var * expr * expr
    (** Let expression. Both subexpressions might be impure. *)

  | ELetPure of relevance * var * expr * expr
    (** Let expression, that binds pure expression. The variable might be
      marked as relevant or irrelevant. *)

  | ELetRec of (var * ttype * expr) list * expr
    (** Mutually recursive let-definitions. Recursive expressions must be
      pure and productive, i.e., they can recursively refer to themselves
      or other mutually recursive definitions only under [ERecCtx] marker. *)

  | ERecCtx of expr
    (** Marker for recursive context. It has [NTerm] effect, and from this
      point, all recursive calls are allowed. It is used to ensure that
      recursive definitions are productive. *)

  | EFn  of var * ttype * expr
    (** Lambda-abstraction *)

  | ETFun : 'k tvar * expr -> expr
    (** Type function *)

  | ECAbs of constr list * expr
    (** Constraint abstraction *)

  | EApp of expr * value
    (** Function application. The subexpression must be pure. *)

  | ETApp : expr * 'k typ -> expr
    (** Type application. Always pure. *)

  | ECApp of expr
    (** Instantiation of a constraint abstraction. Always pure. *)

  | EData of data_def list * expr
    (** Mutually recursive datatype definitions *)

  | ECtor of expr * int * Type.ex list * value list
    (** Fully-applied constructor of an ADT. The meaning of the parameters
      is the following.
      - Computationally irrelevant proof that given that the type of the
        whole expression is an ADT.
      - An index of the constructor.
      - Existential type parameters of the constructor.
      - Regular parameters of the constructor. *)

  | EMatch  of expr * value * match_clause list * ttype * effct
    (** Shallow pattern matching. The first parameter is the proof that the
      type of the matched value is an ADT *)

  | EShift of value * TVar.ex list * var list * var * expr * ttype
    (** Shift-0 operator parametrized by runtime tag, binders of existential
      types and values stored at the delimiter, binder for continuation
      variable, body, and the type of the whole expression. *)

  | EReset of value * Type.ex list * value list * expr * var * expr
    (** Reset-0 operator parametrized by runtime tag, list of types and values
      stored at this delimiter, body, and the return clause *)

  | ERepl of (unit -> expr) * ttype * effct
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      The second and third parameters are type and effect of an expression
      returned by the function. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Values.

  Values are basic expressions, that are always pure, and can be duplicated at
  minimal cost. Note that the notion of value does not correspond to values in
  the reduction semantics. For instance, lambda-abstraction is not a value.
  However, one could say that it is not a value, because the closure needs to be
  allocated. *)
and value =
  | VLit of lit
    (** Literal *)

  | VVar of var
    (** Variable *)

  | VExtern of string * ttype
    (** Externally defined function *)

(** Pattern-matching clause *)
and match_clause = {
  cl_tvars : TVar.ex list;
    (** List of existentially quantified type variables *)

  cl_vars  : var list;
    (** List of variables bound by the constructor *)

  cl_body  : expr
    (** Body of the clause *)
}

(** Program *)
type program = expr

(* ========================================================================= *)

(** Produce S-expression that represents given program *)
val to_sexpr : program -> SExpr.t

(** Internal type-checker for Core programs.
  It is used as a sanity check, if implemented transformations preserve
  types. *)
val check_well_typed : program -> unit
