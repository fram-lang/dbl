(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pretty-printing of types *)

open TypeBase

include UnifCommon.Pretty

let tr_guarded_var tr_var (x, p) =
  if IncrSAT.Formula.is_true p then tr_var x
  else PP_EffSimpleGuard (tr_var x)

let tr_tvar_effect x = PP_EffVar x

let tr_gvar_effect gv = PP_EffUVar (Effct.GVar.uid gv)

let tr_effect eff =
  let (tvars, gvars) = Effct.view eff in
  List.map (tr_guarded_var tr_tvar_effect) tvars @
  List.map (tr_guarded_var tr_gvar_effect) gvars

let rec tr_type tp =
  match view tp with
  | TVar x -> PP_TVar x
  | TArrow(sch, tp, Pure) ->
    PP_TPureArrow(tr_scheme sch, tr_type tp)
  | TArrow(sch, tp, Impure eff) ->
    PP_TArrow(tr_scheme sch, tr_type tp, tr_effect eff)
  | TLabel(eff, delim_tp, delim_eff) ->
    PP_TLabel(tr_effect eff, tr_type delim_tp, tr_effect delim_eff)
  | THandler h ->
    PP_THandler
      { eff_var = h.tvar;
        cap_tp  = tr_type   h.cap_tp;
        in_tp   = tr_type   h.in_tp;
        in_eff  = tr_effect h.in_eff;
        out_tp  = tr_type   h.out_tp;
        out_eff = tr_effect h.out_eff
      }
  | TEffect eff    -> PP_TEffect (tr_effect eff)
  | TApp(tp1, tp2) -> PP_TApp(tr_type tp1, tr_type tp2)

and tr_scheme sch =
  { ppsch_targs = sch.sch_targs;
    ppsch_named = List.map tr_named_scheme sch.sch_named;
    ppsch_body  = tr_type sch.sch_body
  }

and tr_named_scheme (name, sch) = (name, tr_scheme sch)

let pp_effect ctx pp_tree eff =
  pp_effect_trees ctx pp_tree (tr_effect eff)

let pp_type ctx pp_tree tp =
  pp_type_tree ctx pp_tree (tr_type tp)

let pp_scheme ctx pp_tree sch =
  pp_scheme_tree ctx pp_tree (tr_scheme sch)
