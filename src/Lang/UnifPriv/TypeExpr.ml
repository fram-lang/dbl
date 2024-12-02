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
    { effect; cap_type; in_type; in_eff = _; out_type; out_eff = _ } ->
    t_handler effect (to_type cap_type) (to_type in_type) (to_type out_type)
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
  | { se_pos = _; se_targs = []; se_named = _; se_body } -> Some se_body
  | _ -> None
