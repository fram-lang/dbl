(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of types in the ConE language. *)

type kind  = UnifCommon.Kind.t
type tname = UnifCommon.Names.tname
type name  = UnifCommon.Names.name

module TVar = UnifCommon.TVar
type tvar       = TVar.t
type named_tvar = tname * tvar

type gvar  = Effct.gvar
type effct = Effct.t

type ceffect = CEffect.t =
  | Pure
  | Impure of effct

type typ = type_view

and scheme =
  { sch_targs : named_tvar list;
    sch_named : named_scheme list;
    sch_body  : typ
  }

and named_scheme = name * scheme

and type_view =
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

type ctor_decl =
  { ctor_name        : string;
    ctor_targs       : named_tvar list;
    ctor_named       : named_scheme list;
    ctor_arg_schemes : scheme list
  }

type constr = effct * effct

let t_var x = TVar x

let t_arrow sch tp eff = TArrow(sch, tp, eff)

let t_pure_arrow sch tp = t_arrow sch tp Pure

let t_pure_arrows schs tp =
  List.fold_right t_pure_arrow schs tp

let t_label eff delim_tp delim_eff = TLabel(eff, delim_tp, delim_eff)

let t_handler tvar cap_tp in_tp in_eff out_tp out_eff =
  THandler { tvar; cap_tp; in_tp; in_eff; out_tp; out_eff }

let t_effect eff = TEffect eff

let t_app tp1 tp2 = TApp (tp1, tp2)

let t_apps tp tps = List.fold_left t_app tp tps

let view tp = tp

let to_effect tp =
  match view tp with
  | TVar    x   -> Effct.var x
  | TEffect eff -> eff
  | TArrow _ | TLabel _ | THandler _ ->
    failwith "Internal kind error"
  | TApp _ ->
    failwith "Internal error: type application in effect"
