(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common functions of pretty printers *)

(** Context of pretty-printing *)
type ctx

(** Create an empty context. *)
val empty_context : unit -> ctx

(** Internal printable representation of atomic effect *)
type effect_tree =
  | PP_EffWildcard
  | PP_EffVar         of TVar.t
  | PP_EffUVar        of UID.t
  | PP_EffSimpleGuard of effect_tree

(** Internal printable representation of types *)
type type_tree =
  | PP_TVar       of TVar.t
  | PP_TUVar      of UID.t
  | PP_TPureArrow of scheme_tree * type_tree
  | PP_TArrow     of scheme_tree * type_tree * effect_tree list
  | PP_TLabel     of effect_tree list * type_tree * effect_tree list
  | PP_THandler   of
    { eff_var : TVar.t;
      cap_tp  : type_tree;
      in_tp   : type_tree;
      in_eff  : effect_tree list;
      out_tp  : type_tree;
      out_eff : effect_tree list
    }
  | PP_TEffect    of effect_tree list
  | PP_TApp       of type_tree * type_tree
  | PP_TAlias     of PPTree.uid * type_tree

(** Internal printable representation of type schemes *)
and scheme_tree =
  { ppsch_targs : (Names.tname * TVar.t) list;
    ppsch_named : (Names.name * scheme_tree) list;
    ppsch_body  : type_tree
  }

(** Pretty-print kind *)
val pp_kind : ctx -> Kind.t -> string

(** Pretty-print type variable *)
val pp_tvar : ctx -> PPTree.t -> TVar.t -> string

(** Pretty-print internal representation of effects *)
val pp_effect_trees : ctx -> PPTree.t -> effect_tree list -> string

(** Pretty-print internal representation of types *)
val pp_type_tree : ctx -> PPTree.t -> type_tree -> string

(** Pretty-print internal representation of schemes *)
val pp_scheme_tree : ctx -> PPTree.t -> scheme_tree -> string

(** Pretty-print additional information about printing context, e.g.,
  locations of binders of anonymous types. *)
val additional_info : ctx -> string
