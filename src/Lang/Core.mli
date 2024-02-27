(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Core Language. *)

(* Author: Piotr Polesiuk, 2023,2024 *)

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

  (** Create exact copy (with different UID) of a type variable *)
  val clone : 'k tvar -> 'k tvar

  (** Get the kind of given type variable *)
  val kind : 'k tvar -> 'k kind

  (** Existential version of type variable, where its kind is packed *)
  type ex = Ex : 'k tvar -> ex

  (** Finite maps from type variables *)
  module Map : Map1.S with type 'k key = 'k t
end

(** Types, indexed by a type-represented kind *)
type _ typ =
  | TUnit : ktype typ
    (** Unit type *)

  | TEffPure : keffect typ
    (** Pure effect *)

  | TEffJoin : effect * effect -> keffect typ
    (** Join of two effects. Avoid using this constructor directly: Function
      [Effect.join] provides similar functionalily, but removes duplicates. *)

  | TVar  : 'k tvar -> 'k typ
    (** Type variable *)

  | TArrow  : ttype * ttype * effect -> ktype typ
    (** Arrow type *)

  | TForall : 'k tvar * ttype -> ktype typ
    (** Polymorphic type *)

  | TLabel   : effect * ttype * effect -> ktype typ
    (** Type of first class label. It stores effect of a label as well as
      type and effect of the context of the delimiter ([EReset]) *)

  | TData    : ttype * ctor_type list -> ktype typ
    (** Proof of the shape of ADT.
      
      Algebraic data type (ADTs) are just abstract types, but each operation
      on them like constructors or pattern-matching requires additional
      computationally irrelevant parameter of type that describes the shape
      of ADTs. This approach simplifies many things, e.g., mutually recursive
      types are not recursive at all! *)

  | TApp     : ('k1 -> 'k2) typ * 'k1 typ -> 'k2 typ
    (** Type application *)

(** Proper types *)
and ttype = ktype typ

(** Effects *)
and effect = keffect typ

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

(** ADT definition *)
type data_def = {
  dd_tvar  : TVar.ex;
    (** Type variable, that represents this ADT. *)

  dd_proof : var;
    (** An irrelevant variables that stores the proof that this ADT has
      the following constructors. *)

  dd_args  : TVar.ex list;
    (** List of type parameters of this ADT. *)

  dd_ctors : ctor_type list
    (** List of constructors. *)
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

  (** Existential version of type representation, where its kind is packed *)
  type ex = Ex : 'k typ -> ex
end

(* ========================================================================= *)
(** Operations on effects *)
module Effect : sig
  (** Join of two effects. Same as TEffJoin constructor, but removes
    duplicates. *)
  val join : effect -> effect -> effect

  (** Effect of possible non-termination *)
  val nterm : effect
end

(* ========================================================================= *)
(** Built-in types *)
module BuiltinType : sig
  (** List of all built-in types together with their names *)
  val all : (string * TVar.ex) list
end

(* ========================================================================= *)

(** Expressions *)
type expr =
  | EValue of value
    (** value *)

  | ELet of var * expr * expr
    (** Let expression *)

  | ELetPure of var * expr * expr
    (** Let expression, that binds pure expression *)

  | ELetIrr of var * expr * expr
    (** Let expression, that binds computationally irrelevant expression *)

  | EApp of value * value
    (** Function application *)

  | ETApp : value * 'k typ -> expr
    (** Type application *)

  | EData of data_def list * expr
    (** Mutually recursive datatype definitions *)

  | EMatch  of expr * value * match_clause list * ttype * effect
    (** Shallow pattern matching. The first parameter is the proof that the
      type of the matched value is an ADT *)

  | ELabel of keffect tvar * var * ttype * effect * expr
    (** Create a fresh runtime tag for control operators. *)

  | EShift of value * var * expr * ttype
    (** Shift-0 operator parametrized by runtime tag, binder for continuation
      variable, body, and the type of the whole expression. *)

  | EReset of value * expr * var * expr
    (** Reset-0 operator parametrized by runtime tag, body, and the return
      clause *)

  | ERepl of (unit -> expr) * ttype * effect
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      The second and third parameters are type and effect of an expression
      returned by the function. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Values *)
and value =
  | VUnit
    (** Unit value *)

  | VNum of int
    (** Integer literal *)

  | VVar of var
    (** Variable *)

  | VFn  of var * ttype * expr
    (** Lambda-abstraction *)

  | VTFun : 'k tvar * expr -> value
    (** Type function *)

  | VCtor of expr * int * Type.ex list * value list
    (** Fully-applied constructor of ADT. The meaning of the parameters
      is the following.
      - Computationally irrelevant proof that given that the type of the
        whole expression is an ADT.
      - An index of the constructor.
      - Existential type parameters of the constructor.
      - Regular parameters of the constructor. *)

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
