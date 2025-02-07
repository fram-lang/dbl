(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on type expressions *)

open TypeBase
open Type
open Syntax

let rec to_type (tp : type_expr) =
  match tp.data with
  | TE_Type tp  -> tp
  | TE_Effect _ -> t_effect
  | TE_PureArrow(sch, tp) ->
    t_pure_arrow (to_scheme sch) (to_type tp)
  | TE_Arrow(sch, tp, _) ->
    t_arrow (to_scheme sch) (to_type tp) Impure
  | TE_Handler
    { eff_var; cap_type; in_type; in_eff = _; out_type; out_eff = _ } ->
    t_handler eff_var (to_type cap_type) (to_type in_type) (to_type out_type)
  | TE_Label lbl ->
    t_label (to_type lbl.delim_tp)
  | TE_App(tp1, tp2) ->
    t_app (to_type tp1) (to_type tp2)
  | TE_Option tp ->
    t_option (to_type tp)

and to_scheme (sch : scheme_expr) =
  { sch_targs = sch.se_targs;
    sch_named = List.map to_named_scheme sch.se_named;
    sch_body  = to_type sch.se_body
  }

and to_named_scheme (name, sch) =
  (name, to_scheme sch)

let to_ctor_decl (cde : ctor_decl_expr) =
  { ctor_name  = cde.cde_name;
    ctor_targs = cde.cde_targs;
    ctor_named = List.map to_named_scheme cde.cde_named;
    ctor_arg_schemes = List.map to_scheme cde.cde_arg_schemes
  }

let mono_scheme_expr (tp : type_expr) =
  { se_pos   = tp.pos;
    se_targs = [];
    se_named = [];
    se_body  = tp
  }

let of_scheme_expr (sch : scheme_expr) =
  match sch with
  | { se_pos = _; se_targs = []; se_named = []; se_body } -> Some se_body
  | _ -> None

let rec subst sub (tp : type_expr) =
  { tp with data =
    match tp.data with
    | TE_Type tp    -> TE_Type (Subst.in_type sub tp)
    | TE_Effect tps -> TE_Effect (List.map (subst sub) tps)
    | TE_PureArrow(sch, tp) ->
      TE_PureArrow(subst_in_scheme sub sch, subst sub tp)
    | TE_Arrow(sch, tp, eff) ->
      TE_Arrow(subst_in_scheme sub sch, subst sub tp, subst sub eff)
    | TE_Handler{ eff_var; cap_type; in_type; in_eff; out_type; out_eff } ->
      let out_type = subst sub out_type in
      let out_eff = subst sub out_eff in
      let (sub, eff_var) = Subst.add_tvar (Subst.enter_scope sub) eff_var in
      TE_Handler { eff_var; out_type; out_eff;
        cap_type = subst sub cap_type;
        in_type = subst sub in_type;
        in_eff = subst sub in_eff
      }
    | TE_Label { eff; delim_tp; delim_eff } ->
      TE_Label {
        eff       = subst sub eff;
        delim_tp  = subst sub delim_tp;
        delim_eff = subst sub delim_eff
      }
    | TE_App(tp1, tp2) ->
      TE_App(subst sub tp1, subst sub tp2)
    | TE_Option tp ->
      TE_Option (subst sub tp)
  }

and subst_in_scheme sub (sch : scheme_expr) =
  let (sub, targs) =
    Subst.add_named_tvars (Subst.enter_scope sub) sch.se_targs in
  { se_pos   = sch.se_pos;
    se_targs = targs;
    se_named = List.map (subst_in_named_scheme sub) sch.se_named;
    se_body  = subst sub sch.se_body
  }

and subst_in_named_scheme sub (name, sch) =
  (name, subst_in_scheme sub sch)
