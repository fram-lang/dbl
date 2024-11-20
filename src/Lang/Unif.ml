(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The Unif language: result of type-inference. *)

include SyntaxNode.Export

include UnifPriv.KindBase
include UnifPriv.TypeBase

type subst = UnifPriv.Subst.t

module Kind = struct
  include UnifPriv.KindBase
end

module TVar  = UnifPriv.TVar
module Scope = UnifPriv.Scope

module Name = UnifPriv.Name

module Type = struct
  include UnifPriv.TypeBase
  include UnifPriv.Type
  include UnifPriv.TypeWhnf

  let subst = UnifPriv.Subst.in_type
end

module Effect = UnifPriv.Effect

module Scheme = struct
  let of_type        = UnifPriv.Type.mono_scheme
  let is_monomorphic = UnifPriv.Type.scheme_is_monomorphic
  let uvars          = UnifPriv.Type.scheme_uvars
  let collect_uvars  = UnifPriv.Type.collect_scheme_uvars
  let refresh        = UnifPriv.Type.refresh_scheme
  let subst          = UnifPriv.Subst.in_scheme
end

module NamedScheme = struct
  let subst = UnifPriv.Subst.in_named_scheme
end

module CtorDecl = struct
  let collect_uvars = UnifPriv.Type.collect_ctor_uvars

  let subst = UnifPriv.Subst.in_ctor_decl

  let find_index cs name = List.find_index (fun c -> c.ctor_name = name) cs
end

module Subst = UnifPriv.Subst
module BuiltinType = UnifPriv.BuiltinType

type var = Var.t

type data_def =
  | DD_Data of
    { tvar  : tvar;
      proof : var;
      args  : named_tvar list;
      ctors : ctor_decl list
    }

  | DD_Label of
    { tvar      : tvar;
      var       : var;
      delim_tp  : typ;
      delim_eff : effrow
    }

type pattern = pattern_data node
and pattern_data =
  | PWildcard
  | PVar  of var * scheme
  | PCtor of string * int * expr * ctor_decl list * tvar list * pattern list

and expr = expr_data node
and expr_data =
  | EUnitPrf
  | ENum        of int
  | ENum64      of int64
  | EStr        of string
  | EChr        of char
  | EVar        of var
  | EPureFn     of var * scheme * expr
  | EFn         of var * scheme * expr
  | ETFun       of tvar * expr
  | EApp        of expr * expr
  | ETApp       of expr * typ
  | ELet        of var * scheme * expr * expr
  | ELetRec     of rec_def list * expr
  | ECtor       of expr * int * typ list * expr list
  | EData       of data_def list * expr
  | EMatchEmpty of expr * expr * typ * effrow
  | EMatch      of expr * match_clause list * typ * effrow
  | EHandle     of tvar * var * typ * expr * expr
  | EHandler    of
    { label     : var;
      effect    : tvar;
      delim_tp  : typ;
      delim_eff : effect;
      cap_type  : typ;
      cap_body  : expr;
      ret_var   : var;
      body_tp   : typ;
      ret_body  : expr;
      fin_var   : var;
      fin_body  : expr;
    }
  | EEffect     of expr * var * expr * typ
  | EExtern     of string * typ
  | ERepl       of (unit -> expr) * typ * effrow
  | EReplExpr   of expr * string * expr

and rec_def = var * scheme * expr

and match_clause = pattern * expr

type program = expr
