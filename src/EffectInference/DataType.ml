(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of datatype definitions. *)

open Common

let tr_ctor_decl_expr env (ctor : S.ctor_decl_expr) =
  let env0 = env in
  let env = Env.enter_scope env in
  let (env, targs) = Env.add_named_tvars env ctor.cde_targs in
  let named = List.map (Type.tr_named_scheme_expr env) ctor.cde_named in
  let arg_schemes = List.map (Type.tr_scheme_expr env) ctor.cde_arg_schemes in
  let ctor =
    { T.ctor_name        = ctor.cde_name;
      T.ctor_targs       = targs;
      T.ctor_named       = named;
      T.ctor_arg_schemes = arg_schemes
    } in
  ConstrSolver.leave_scope_with_ctors ~env0 ~tvars:(List.map snd targs)
    (Env.constraints env) [ctor];
  ctor

(* ========================================================================= *)

let prepare_data_def env (dd : S.data_def) =
  match dd with
  | DD_Data d ->
    let (env, x) = Env.add_tvar env d.tvar in
    (env, (x, dd))

  | DD_Label l ->
    let (env, x) = Env.add_tvar env l.tvar in
    (env, (x, dd))

let tr_data_def env (x, (dd : S.data_def)) =
  match dd with
  | DD_Data d ->
    let env0 = env in
    let env = Env.enter_scope env in
    let (env, args) = Env.add_named_tvars env d.args in
    let ctors = List.map (tr_ctor_decl_expr env) d.ctors in
    ConstrSolver.leave_scope_with_ctors ~env0 ~tvars:(List.map snd args)
      (Env.constraints env) ctors;
    let env = env0 in
    let tp =
      T.Type.t_apps (T.Type.t_var x)
        (List.map (fun (_, x) -> T.Type.t_var x) args) in
    let info =
      { Env.adt_proof    = d.proof;
        Env.adt_type     = tp;
        Env.adt_args     = args;
        Env.adt_ctors    = ctors;
        Env.adt_positive =
          match d.eff with
          | Pure   -> true
          | Impure -> false
      } in
    let env = Env.add_data env d.proof info in
    let dd = T.DD_Data
      { tvar     = x;
        proof    = d.proof;
        args     = args;
        ctors    = ctors;
        positive = info.adt_positive
      } in
    (env, dd)

  | DD_Label l ->
    let delim_tp  = Type.tr_type env l.delim_tp in
    let delim_eff = Env.fresh_gvar env in
    let lbl_tp  = T.Type.t_label (T.Effct.var x) delim_tp delim_eff in
    let lbl_tp' = Type.tr_type_expr env l.annot in
    let origin  = OLabelAnnot(l.annot.pos, l.annot.pp, lbl_tp, lbl_tp') in
    Subtyping.subtype ~origin env lbl_tp lbl_tp';
    let env = Env.add_mono_var env l.var lbl_tp' in
    let dd = T.DD_Label
      { tvar      = x;
        var       = l.var;
        delim_tp  = delim_tp;
        delim_eff = delim_eff
      } in
    (env, dd)

let tr_data_defs env dds =
  let (env, dds) = List.fold_left_map prepare_data_def env dds in
  let tvs = List.map fst dds in
  let (env, dds) = List.fold_left_map tr_data_def env dds in
  (env, dds, tvs)
