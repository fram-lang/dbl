(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pretty-printing of kinds and types *)

open TypeBase

include UnifCommon.Pretty

let any_effect = [ PP_EffWildcard ]

let rec tr_type tp =
  match view tp with
  | TEffect -> PP_TEffect any_effect
  | TUVar u -> PP_TUVar (UVar.uid u)
  | TVar  x -> PP_TVar x
  | TArrow(sch, tp, Pure) ->
    PP_TPureArrow(tr_scheme sch, tr_type tp)
  | TArrow(sch, tp, Impure) ->
    PP_TArrow(tr_scheme sch, tr_type tp, any_effect)
  | THandler(eff_var, cap_tp, in_tp, out_tp) ->
    PP_THandler
      { eff_var = eff_var;
        cap_tp  = tr_type cap_tp;
        in_tp   = tr_type in_tp;
        in_eff  = any_effect;
        out_tp  = tr_type out_tp;
        out_eff = any_effect
      }
  | TLabel delim_tp ->
    PP_TLabel(any_effect, tr_type delim_tp, any_effect)
  | TApp(tp1, tp2) -> PP_TApp(tr_type tp1, tr_type tp2)

and tr_scheme sch =
  { ppsch_targs = sch.sch_targs;
    ppsch_named = List.map tr_named_scheme sch.sch_named;
    ppsch_body  = tr_type sch.sch_body
  }

and tr_named_scheme (name, sch) = (name, tr_scheme sch)

let pp_type ctx pp_tree tp =
  pp_type_tree ctx pp_tree (tr_type tp)

let pp_scheme ctx pp_tree sch =
  pp_scheme_tree ctx pp_tree (tr_scheme sch)
