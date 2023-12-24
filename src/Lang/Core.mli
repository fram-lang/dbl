(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Core Language. *)

(* Author: Piotr Polesiuk, 2023 *)

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

(** Type variable, indexed by its kind *)
type 'k tvar

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

  | TData    : ttype * ctor_type list -> ktype typ
    (** Proof of the shape of ADT.
      
      Algebraic data type (ADTs) are just abstract types, but each operation
      on them like constructors or pattern-matching requires additional
      computationally irrelevant parameter of type that describes the shape
      of ADTs. This approach simplifies many things, e.g., mutually recursive
      types are not recursive at all! *)

(** Proper types *)
and ttype = ktype typ

(** Effects *)
and effect = keffect typ

(** ADT constructor type *)
and ctor_type = {
  ctor_name      : string;
    (** Name of the constructor *)

  ctor_arg_types : ttype list
    (** Types of constructor arguments *)
}

(* ========================================================================= *)

(** Variables *)
type var = Var.t

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

  | EData : 'a tvar * var * ctor_type list * expr -> expr
    (** Definition of non-recursive ADT. It binds a type variable, an
      irrelevant variable that stores the proof that this ADT has given
      constructors, the list of constructors and the rest of an expression. *)

  | EHandle of keffect tvar * var * expr * h_expr * ttype * effect
    (** Handler. It stores handled (abstract) effect, capability variable,
      handled expression, handler body, and type and effect of the whole
      handler expression *)

  | ERepl of (unit -> expr) * effect
    (** REPL. It is a function that prompts user for another input. It returns
      an expression to evaluate, usually containing another REPL expression.
      The second parameter is an effect of an expression returned by the
      function. *)

  | EReplExpr of expr * string * expr
    (** Print type (second parameter), evaluate and print the first expression,
      then continue to the second expression. *)

(** Values *)
and value =
  | VUnit
    (** Unit value *)

  | VVar of var
    (** Variable *)

  | VFn  of var * ttype * expr
    (** Lambda-abstraction *)

  | VTFun : 'k tvar * expr -> value
    (** Type function *)

  | VCtor of value * int * value list
    (** Fully-applied constructor of ADT. The first parameter is a
      computationally irrelevent proof that given that the type of the
      whole expression is an ADT. The second parameter is an index of the
      constructor. *)

(** Handler expressions *)
and h_expr =
  | HEffect of ttype * ttype * var * var * expr
    (** Handler of effectful functional operation. It stores input type,
      output type, formal parameter, resumption formal parameter, and the 
      body. *)

(** Program *)
type program = expr

(* ========================================================================= *)
(** Operations on kinds *)
module Kind : sig
  (** Existential version of kind, where its kind index is packed *)
  type ex = Ex : 'k kind -> ex
end

(* ========================================================================= *)
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

(* ========================================================================= *)
(** Operations on types *)
module Type : sig
  (** Get the kind of given type *)
  val kind : 'k typ -> 'k kind

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

(** Internal type-checker for Core programs.
  It is used as a sanity check, if implemented transformations preserve
  types. *)
val check_well_typed : program -> unit
