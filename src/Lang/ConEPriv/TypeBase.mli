(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of types in the ConE language. *)

type kind  = UnifCommon.Kind.t
type tname = UnifCommon.Names.tname
type name  = UnifCommon.Names.name

type tvar       = TVar.t
type named_tvar = tname * tvar

type gvar  = Effct.gvar
type effct = Effct.t

type ceffect = CEffect.t =
  | Pure
  | Impure of effct

type typ

type scheme =
  { sch_targs : named_tvar list;
    sch_named : named_scheme list;
    sch_body  : typ
  }

and named_scheme = name * scheme

type type_view =
  | TVar     of tvar
  | TArrow   of scheme * typ * ceffect
  | TLabel   of effct * typ * effct
  | THandler of
    { tvar    : tvar;
      cap_tp  : typ;
      in_tp   : typ;
      in_eff  : effct;
      out_tp  : typ;
      out_eff : effct
    }
  | TEffect  of effct
  | TApp     of typ * typ
  | TAlias   of PPTree.uid * typ

type ctor_decl =
  { ctor_name        : string;
    ctor_targs       : named_tvar list;
    ctor_named       : named_scheme list;
    ctor_arg_schemes : scheme list
  }

type constr = effct * effct

val t_var         : tvar -> typ
val t_arrow       : scheme -> typ -> ceffect -> typ
val t_pure_arrow  : scheme -> typ -> typ
val t_pure_arrows : scheme list -> typ -> typ
val t_label       : effct -> typ -> effct -> typ
val t_handler     : tvar -> typ -> typ -> effct -> typ -> effct -> typ
val t_effect      : effct -> typ
val t_app         : typ -> typ -> typ
val t_apps        : typ -> typ list -> typ
val t_alias       : PPTree.uid -> typ -> typ

val view : typ -> type_view

val to_effect : typ -> effct
