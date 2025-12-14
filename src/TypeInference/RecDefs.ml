(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Type-inference for recursive definitions *)

open Common
open TypeCheckFix

(* ========================================================================= *)
(** The first pass: extending the environment by recursive data and effects
  (attached to labels). Additionally, section type parameters are added to the
  environment. *)

(** Representation of recursive definition after the first pass *)
type def1 =
  | D1_Blank
    (** The definition was full processed, and nothing is left *)

  | D1_LetId of S.is_public * S.ident * S.poly_expr_def
    (** A recursive value definition *)

  | D1_ValParam of S.name * S.ident * S.scheme_expr option
    (** A value parameter declaration *)

  | D1_Label   of
    T.tvar * S.is_public * S.ident * (Position.t * S.scheme_expr) option

  | D1_Data    of
    { public_ctors : bool;
      tvar         : T.tvar;
      name         : S.tvar;
      args         : type_param list;
      args_env     : PartialEnv.t;
      ctors        : S.ctor_decl list
    }

  | D1_Section of def1 S.node list

(** The main function of the first pass. *)
let rec prepare_rec_data env (def : S.def) =
  let make data = { def with data = data } in
  let pos = def.pos in
  match def.data with
  | DLetId(public, id, body) ->
    (env, make (D1_LetId(public, id, body)))

  | DLetPat _ ->
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)

  | DLabel(eff, pat) ->
    let (env, _, x) = Type.check_type_arg env eff T.Kind.k_effect in
    begin match pat.data with
    | PId(public, id) -> (env, make (D1_Label(x, public, id, None)))

    | PAnnot({ data = PId(public, id); _ }, sch) ->
      (env, make (D1_Label(x, public, id, Some(pat.pos, sch))))

    | PAnnot({ data = PWildcard | PCtor _ | PAnnot _; _ }, _)
    | PWildcard | PCtor _ ->
      Error.fatal (Error.invalid_rec_def ~pos:def.pos)
    end

  | DHandlePat _ ->
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)

  | DTypeParam(name, x, kind) ->
    let kind = Type.tr_kind kind in
    let name = tr_tname name in
    let env = Env.declare_type ~pos env name x kind in
    (env, make D1_Blank)

  | DValParam(name, x, sch) ->
    (env, make (D1_ValParam(name, x, sch)))

  | DData { public_tp; public_ctors; tvar=name; args; ctors } ->
    let (penv, args) = Type.tr_named_type_args args in
    let kind = DataType.kind args in
    let (env, x) = Env.add_tvar ~pos env ~public:public_tp name kind in
    let def =
      make (D1_Data
        { public_ctors = public_ctors;
          tvar         = x;
          name         = name;
          args         = args;
          args_env     = penv;
          ctors        = ctors
        })
    in (env, def)

  | DType _ ->
    (* TODO: recursive type aliases are not supported, yet *)
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)

  | DSection defs | DRec defs ->
    let (env, defs) = List.fold_left_map prepare_rec_data env defs in
    (env, make (D1_Section defs))

  | DModule(public, name, defs) ->
    (* TODO: recursive modules are not supported, yet *)
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)

  | DOpen(public, path) ->
    let m = ModulePath.lookup_module env path in
    let env = Env.open_module ~public env m in
    (env, make (D1_Blank))

  | DReplExpr _ | DReplDir _ ->
    assert false

(* ========================================================================= *)
(** The second pass: finalization of checking labels and ADTs, i.e., extending
  the environment by term-level information (labels and proofs). Additionally,
  section value parameters are added to the environment *)

(** The second pass for single definition. See [finalize_rec_data_defs] for
  more details. *)
let rec finalize_rec_data ~nonrec_scope env (def : def1 S.node) =
  let pos = def.pos in
  let make data =
    { T.pos  = pos;
      T.pp   = Env.pp_tree env;
      T.data = data
    } in
  match def.data with
  | D1_Blank | D1_LetId _ -> (env, [], T.Pure)

  | D1_ValParam(name, x, sch) ->
    let (sch_env, params) = Env.begin_generalize env in
    let sch =
      match sch with
      | Some sch -> Type.tr_scheme sch_env sch
      | None     ->
        let tp = make (T.TE_Type (Env.fresh_uvar ~pos env T.Kind.k_type)) in
        T.SchemeExpr.of_type_expr tp
    in
    let env =
      ParamGen.end_generalize_declare ~pos:def.pos params env name x sch in
    (env, [], T.Pure)

  | D1_Label(tvar, public, id, sch_opt) ->
    let delim_tp = Env.fresh_uvar ~pos env T.Kind.k_type in
    let l_tp = T.Type.t_label delim_tp in
    let annot =
      match sch_opt with
      | None -> make (T.TE_Type l_tp)
      | Some(sch_pos, sch) ->
        let sch = Type.tr_scheme env sch in
        begin match T.SchemeExpr.to_type_expr sch with
        | Some tp -> tp
        | None    -> Error.fatal (Error.polymorphic_label ~pos:sch_pos)
        end
    in
    let sch = T.Scheme.of_type l_tp in
    let id =
      NameUtils.tr_ident ~pos:def.pos ~pp:(Env.pp_tree env) id sch in
    let (env, x) = Env.add_val ~public env id sch in
    let dd = T.DD_Label
      { tvar     = tvar;
        var      = x;
        delim_tp = delim_tp;
        annot    = annot
      } in
    (env, [dd], T.Impure)

  | D1_Data def ->
    let (data_env, _, args, _) =
      PartialEnv.extend env def.args def.args_env in
    let ctors =
      DataType.check_ctor_decls ~data_targs:args data_env def.ctors in
    let (env, dd) =
      DataType.finalize_check
        ~public:def.public_ctors
        ~nonrec_scope env def.tvar
        ~name:def.name
        args ctors
    in
    (env, [dd], T.Pure)

  | D1_Section defs ->
    let (env, dds, eff) =
      finalize_rec_data_defs ~nonrec_scope env defs in
    (env, dds, eff)

(** The main function of the second pass. It returns extended environment,
  list of data/label definitions and the effect of this block (impure, when
  new run-time labels are generated). *)
and finalize_rec_data_defs ~nonrec_scope env defs =
  match defs with
  | [] -> (env, [], T.Pure)
  | def :: defs ->
    let (env, dds1, eff1) = finalize_rec_data ~nonrec_scope env def in
    let (env, dds2, eff2) = finalize_rec_data_defs ~nonrec_scope env defs in
    (env, dds1 @ dds2, T.Effect.join eff1 eff2)

(* ========================================================================= *)
(** The third pass: Guessing the schemes of recursive definitions *)

(** Information gathered by guessing the scheme of a recursive value *)
type rec_fun3 =
  { rf3_scheme  : T.scheme_expr;
      (** Type scheme of a recursive value *)

    rf3_targs   : type_param list;
      (** Explicit formal type parameters *)

    rf3_named   : T.pattern list;
      (** Patterns for explicit named parameters *)

    rf3_args    :
      (Position.t * PPTree.t * T.pattern * T.scheme_expr * T.effct) list;
      (** Formal parameters of the function, together with position and effect
        of the lambda abstractions. *)

    rf3_penv    : PartialEnv.t;
      (** Partial environment with formal parameters *)

    rf3_body    : S.expr;
      (** Body of the recursive value *)

    rf3_body_tp : T.typ
      (** Guessed type of a body *)
  }

(** Representation of recursive definition after the third pass *)
type def3 =
  | D3_LetFun  of S.is_public * Name.t * T.var * rec_fun3
  | D3_Section of def3 T.node list

(** Information gathered by guessing the type of a recursive function *)
type rec_fun_body =
  { rfb_type    : T.type_expr;
      (** Type of the recursive function *)

    rfb_args    :
      (Position.t * PPTree.t * T.pattern * T.scheme_expr * T.effct) list;
      (** Formal parameters of the function, together with position and effect
        of the lambda abstractions. *)

    rfb_penv    : PartialEnv.t;
      (** Partial environment with formal parameter *)

    rfb_body    : S.expr;
      (** Body of the recursive function *)

    rfb_body_tp : T.typ;
      (** Guessed type of the body *)
  }

let mk_arrow_type_expr ~pos ~pp sch tp eff =
  { T.pos  = pos;
    T.pp   = pp;
    T.data =
      match eff with
      | T.Pure   -> T.TE_PureArrow(sch, tp)
      | T.Impure ->
        let eff =
          { T.pos  = pos;
            T.pp   = pp;
            T.data = T.TE_Type T.Type.t_effect
          } in
        T.TE_Arrow(sch, tp, eff)
  }

(** Check formal parameters and guess the type of a recursive value.
  This type is unified with the [tp] parameter, which should be an unification
  variable, but it tracks the scope of the guessed type. *)
let rec guess_rec_fun_type env (e : S.expr) tp =
  let pos = e.pos in
  match e.data with
  | EFn(pat, body) ->
    let fn_pp = Env.pp_tree env in
    let (penv, pat, sch, eff1) = Pattern.infer_scheme env pat in
    (* we ignore renaming, because guessed type cannot contain variables bound
      in the pattern. However, we need to extend the environment in order to
      handle shadowing correctly *)
    let (env, _, _, _) = PartialEnv.extend env [] penv in
    let body_tp = Env.fresh_uvar ~pos:body.pos env T.Kind.k_type in
    let (rfb, eff2) = guess_rec_fun_type env body body_tp in
    let eff = T.Effect.join eff1 eff2 in
    let pp = Env.pp_tree env in
    let fun_tp_expr = mk_arrow_type_expr ~pos ~pp sch rfb.rfb_type eff in
    let fun_tp = T.TypeExpr.to_type fun_tp_expr in
    Error.check_unify_result ~pos
      (Unification.subtype env fun_tp tp)
      ~on_error:(Error.expr_type_mismatch ~pp fun_tp tp);
    { rfb_type    = fun_tp_expr;
      rfb_args    = (pos, fn_pp, pat, sch, eff) :: rfb.rfb_args;
      rfb_penv    = PartialEnv.join ~pp penv rfb.rfb_penv;
      rfb_body    = rfb.rfb_body;
      rfb_body_tp = rfb.rfb_body_tp
    }, T.Pure

  | EAnnot(_, tp_expr) ->
    let tp_expr = Type.tr_ttype env tp_expr in
    let tp' = T.TypeExpr.to_type tp_expr in
    Error.check_unify_result ~pos
      (Unification.subtype env tp' tp)
      ~on_error:(Error.expr_type_mismatch ~pp:(Env.pp_tree env) tp' tp);
    { rfb_type    = tp_expr;
      rfb_args    = [];
      rfb_penv    = PartialEnv.empty;
      rfb_body    = e;
      rfb_body_tp = tp
    }, T.Impure

  | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | EPoly _ | EApp _ | EDefs _
  | EMatch _ | EHandler _ | EEffect _ | EExtern _ | ERepl _ ->
    let pp = Env.pp_tree env in
    { rfb_type    = { T.pos = pos; T.pp = pp; T.data = T.TE_Type tp };
      rfb_args    = [];
      rfb_penv    = PartialEnv.empty;
      rfb_body    = e;
      rfb_body_tp = tp
    }, T.Impure

(** Check formal parameters and guess the scheme of a recursive value *)
let rec_def_scheme ~pos env (body : S.poly_expr_def) =
  match body.data with
  | PE_Expr e ->
    let tp = Env.fresh_uvar ~pos env T.Kind.k_type in
    let (rfb, _) = guess_rec_fun_type env e tp in
    { rf3_scheme  = T.SchemeExpr.of_type_expr rfb.rfb_type;
      rf3_targs   = [];
      rf3_named   = [];
      rf3_args    = rfb.rfb_args;
      rf3_penv    = rfb.rfb_penv;
      rf3_body    = rfb.rfb_body;
      rf3_body_tp = rfb.rfb_body_tp
    }

  | PE_Poly e ->
    Error.fatal (Error.invalid_rec_def ~pos)

  | PE_Fn(nps, body) ->
    let se_pp = Env.pp_tree env in
    let (env, penv, tvars, named, eff1) =
      Pattern.infer_named_patterns env nps in
    begin match eff1 with
    | Pure -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos)
    end;
    let body_tp = Env.fresh_uvar ~pos:body.pos env T.Kind.k_type in
    (* we ignore renaming, because guessed scheme cannot contain variables
      bound in patterns, except for type parameters. Moreover, we provide
      empty type parameter list, in order to avoid renaming of type parameters
      in the type of the body. Renaming will be handled in the next pass. *)
    let (env, _, _, _) = PartialEnv.extend env [] penv in
    let (rfb, _) = guess_rec_fun_type env body body_tp in
    let sch =
      { T.se_pos   = pos;
        T.se_pp    = se_pp;
        T.se_targs = List.map (fun (_, name, x) -> (name, x)) tvars;
        T.se_named =
          List.map (fun (name, _, sch) -> (Name.to_unif name, sch)) named;
        T.se_body  = rfb.rfb_type
      } in
    let pp = Env.pp_tree env in
    { rf3_scheme  = sch;
      rf3_targs   = tvars;
      rf3_named   = List.map (fun (_, pat, _) -> pat) named;
      rf3_args    = rfb.rfb_args;
      rf3_penv    = PartialEnv.join ~pp penv rfb.rfb_penv;
      rf3_body    = rfb.rfb_body;
      rf3_body_tp = rfb.rfb_body_tp
    }

(** The third pass for a single definition. It returns an option type: value
  [None] means that the definition does not produce any recursive value. *)
let rec prepare_rec_fun env (def : def1 S.node) =
  let pos = def.pos in
  let make data =
    { T.pos  = pos;
      T.pp   = Env.pp_tree env;
      T.data = data
    } in
  match def.data with
  | D1_Blank | D1_Label _ | D1_Data _ | D1_ValParam _ -> None

  | D1_LetId(public, name, body) ->
    let rf3 = rec_def_scheme ~pos env body in
    let sch = T.SchemeExpr.to_scheme rf3.rf3_scheme in
    let name = NameUtils.tr_ident ~pos ~pp:(Env.pp_tree env) name sch in
    let (env, x) = Env.add_val env name sch in
    let def = make (D3_LetFun(public, name, x, rf3)) in
    Some (env, def)

  | D1_Section defs ->
    let (env, defs) = prepare_rec_funs env defs in
    Some (env, make (D3_Section defs))

and prepare_rec_funs env defs =
  match defs with
  | [] -> (env, [])
  | def :: defs ->
    begin match prepare_rec_fun env def with
    | None -> prepare_rec_funs env defs
    | Some(env, def) ->
      let (env, defs) = prepare_rec_funs env defs in
      (env, def :: defs)
    end

(* ========================================================================= *)
(** The fourth pass: actual type-checking *)

(** Representation of recursive definition after the fourth pass *)
type def4 =
  | D4_LetFun  of
    { public  : S.is_public;
      name    : Name.t;
      var     : T.var;
      scheme  : T.scheme_expr;
      targs   : T.tvar list;
      pats    : T.pattern list;
      body    : T.expr;
      body_tp : T.typ
    }

  | D4_Section of def4 T.node list

(** Build a function from a list of arguments and a body. *)
let mk_function args body body_tp =
  let mk_fn (pos, pp, pat, sch_expr, eff) (body, body_tp) =
    let sch = T.SchemeExpr.to_scheme sch_expr in
    let (x, body) = ExprUtils.match_var pat body body_tp eff in
    let body_tp = T.Type.t_arrow sch body_tp eff in
    { T.pos  = pos;
      T.pp   = pp;
      T.data = T.EFn(x, sch_expr, body, eff)
    }, body_tp
  in
  List.fold_right mk_fn args (body, body_tp)

(** The main function of the fourth pass. *)
let rec check_rec_fun ~tcfix env (def : def3 T.node) =
  let open (val tcfix : TCFix) in
  match def.data with
  | D3_LetFun(public, name, x, rf3) ->
    let (body_env, _, targs, ren) =
      PartialEnv.extend env rf3.rf3_targs rf3.rf3_penv in
    let rename_arg (pos, pp, pat, sch, eff) =
      (pos, pp, T.Ren.rename_pattern ren pat,
        T.Ren.rename_scheme_expr ren sch, eff) in
    let pats = List.map (T.Ren.rename_pattern ren) rf3.rf3_named in
    let args = List.map rename_arg rf3.rf3_args in
    let body_tp = T.Ren.rename_type ren rf3.rf3_body_tp in
    let er = check_expr_type body_env rf3.rf3_body body_tp in
    begin match args, er.er_effect with
    | _ :: _, _ | _, Pure -> ()
    | [], Impure -> Error.report (Error.func_not_pure ~pos:def.pos)
    end;
    let (body, body_tp) = mk_function args er.er_expr body_tp in
    let def =
      { def with
        data = D4_LetFun
          { public  = public;
            name    = name;
            var     = x;
            scheme  = rf3.rf3_scheme;
            targs   = List.map snd targs;
            pats    = pats;
            body    = body;
            body_tp = body_tp
          }
      } in
    (er.er_constr, def)

  | D3_Section defs ->
    let (env, defs) = check_rec_funs ~tcfix env defs in
    (env, { def with data = D4_Section defs })

and check_rec_funs ~tcfix env fds =
  List.fold_left_map
    (fun cs1 fd ->
      let (cs2, fd) = check_rec_fun ~tcfix env fd in
      (cs1 @ cs2, fd))
    [] fds

(* ========================================================================= *)
(** The fifth pass: collecting unification variables *)

(** The main function of the fifth pass *)
let rec collect_rec_fun_uvars uvs (def : def4 T.node) =
  match def.data with
  | D4_LetFun { scheme; _ } ->
    T.Scheme.collect_uvars (T.SchemeExpr.to_scheme scheme) uvs

  | D4_Section defs ->
    List.fold_left collect_rec_fun_uvars uvs defs

(* ========================================================================= *)
(** The sixth pass: building final environment *)

(** The sixth pass for a single definition *)
let rec add_rec_fun env targs1 named1 (def : def4 T.node) =
  match def.data with
  | D4_LetFun d ->
    let y_sch = T.SchemeExpr.to_scheme d.scheme in
    let y_sch =
      { T.sch_targs = targs1 @ y_sch.sch_targs;
        T.sch_named = named1 @ y_sch.sch_named;
        T.sch_body  = y_sch.sch_body
      } in
    let (env, y) = Env.add_val ~public:d.public env d.name y_sch in
    let pats = List.map (fun pat -> (Var.fresh (), pat)) d.pats in
    let vars = List.map fst pats in
    let body = ExprUtils.match_args pats d.body d.body_tp Pure in
    let body =
      { T.pos  = def.pos;
        T.pp   = Env.pp_tree env;
        T.data = T.PF_Fun(d.targs, vars, body)
      } in
    let def =
      { T.rd_pos      = def.pos;
        T.rd_pp       = Env.pp_tree env;
        T.rd_poly_var = y;
        T.rd_var      = d.var;
        T.rd_scheme   = d.scheme;
        T.rd_body     = body
      }
    in
    (env, [def])

  | D4_Section defs ->
    add_rec_funs env targs1 named1 defs

(** The main function of the sixth pass *)
and add_rec_funs env targs named defs =
  match defs with
  | [] -> (env, [])
  | def :: defs ->
    let (env, defs1) = add_rec_fun  env targs named def  in
    let (env, defs2) = add_rec_funs env targs named defs in
    (env, defs1 @ defs2)

(* ========================================================================= *)
(** The seventh pass: checking productivity of recursive definitions *)

(** Update the body of a recursive definition, adding local definitions of
  less-generalized functions after the first impure function. This function
  also checks if the recursive definition is productive. *)
let update_rec_body ~pos fds (body : T.poly_fun) =
  let rec update_expr (e : T.expr) =
    let make data = { body with data = data } in
    match e.data with

    | ENum _ | ENum64 _ | EStr _ | EChr _ | EExtern _ -> e

    | EFn(x, sch, body, Impure) ->
      make (T.EFn(x, sch, make (T.ERecCtx body), Impure))

    | EFn(x, sch, body, Pure) ->
      make (T.EFn(x, sch, update_expr body, Pure))

    | EInst(e, tps, es) ->
      make (T.EInst(update_poly_expr e, tps, List.map update_poly_fun es))

    | EAppPoly(e1, e2) ->
      make (T.EAppPoly(update_expr e1, update_poly_fun e2))

    | EAppMono(e1, e2) ->
      make (T.EAppMono(update_expr e1, update_expr e2))

    | ELetPoly(x, e1, e2) ->
      make (T.ELetPoly(x, update_poly_expr e1, update_expr e2))

    | ELetMono(x, e1, e2) ->
      make (T.ELetMono(x, update_expr e1, update_expr e2))

    | EData(dds, e) ->
      make (T.EData(dds, update_expr e))

    | ETypeAlias(a, tp, e) ->
      make (T.ETypeAlias(a, tp, update_expr e))

    | EMatchEmpty(prf, e, tp, Pure) ->
      make (T.EMatchEmpty(prf, update_expr e, tp, Pure))

    | EMatch(e, cls, tp, Pure) ->
      let cls = List.map (fun (pat, e) -> (pat, update_expr e)) cls in
      make (T.EMatch(update_expr e, cls, tp, Pure))

    | EMatchPoly(e1, pat, e2, tp, Pure) ->
      make (T.EMatchPoly(update_poly_expr e1, pat, update_expr e2, tp, Pure))

    | EHandler h ->
      make (T.EHandler {
        label    = h.label;
        eff_var  = h.eff_var;
        delim_tp = h.delim_tp;
        cap_type = h.cap_type;
        cap_body = update_expr h.cap_body;
        ret_var  = h.ret_var;
        body_tp  = h.body_tp;
        ret_body = update_expr h.ret_body;
        fin_var  = h.fin_var;
        fin_body = update_expr h.fin_body
      })

    | EAnnot(e, tp) ->
      make (T.EAnnot(update_expr e, tp))

    | ELetRec _ | ERecCtx _ | EMatchEmpty(_, _, _, Impure)
    | EMatch(_, _, _, Impure) | EMatchPoly(_, _, _, _, Impure) | EHandle _
    | EEffect _ ->
      Error.report (Error.non_productive_rec_def ~pos);
      e

    | ERepl _ | EReplExpr _ | EReplDir -> assert false

  and update_poly_expr (e : T.poly_expr) =
    let make data = { body with data = data } in
    match e.data with
    | EVar x ->
      if List.exists (fun fd -> Var.equal x fd.T.rd_var) fds then
        Error.report (Error.non_productive_rec_def ~pos);
      e

    | ECtor _ -> e

    | EPolyFun(targs, named, body) ->
      make (T.EPolyFun(targs, named, update_expr body))

    | EGen(targs, named, body) ->
      make (T.EGen(targs, named, update_poly_expr body))

  and update_poly_fun (e : T.poly_fun) =
    let make data = { body with data = data } in
    match e.data with
    | PF_Fun(targs, vars, body) ->
      make (T.PF_Fun(targs, vars, update_expr body))

    | PF_Hole _ ->
      (* we have no means to check if the hole is productive *)
      Error.report (Error.non_productive_rec_def ~pos);
      e
  in
  update_poly_fun body

(** The main function of the seventh pass *)
let finalize_rec_fun (fds : T.rec_def list) (fd : T.rec_def) =
  { fd with rd_body = update_rec_body ~pos:fd.rd_pos fds fd.rd_body }

(* ========================================================================= *)

type 'st rec_result =
  { rec_env    : ('st, sec) opn Env.t;
    rec_dds    : T.data_def list;
    rec_targs  : T.named_tvar list;
    rec_named  : (T.name * T.var * T.scheme_expr) list;
    rec_fds    : T.rec_def list;
    rec_eff    : T.effct;
    rec_constr : Constr.t list
  }

let check_rec_defs ~tcfix ~pos env defs =
  let nonrec_scope = Env.scope env in
  let (env, _) = Env.enter_scope env in
  let (env, defs) = List.fold_left_map prepare_rec_data env defs in
  let (env, dds, eff) = finalize_rec_data_defs ~nonrec_scope env defs in
  let (rec_env, params) = Env.begin_generalize env in
  let (rec_env, fds) = prepare_rec_funs rec_env defs in
  let (cs, fds) = check_rec_funs ~tcfix rec_env fds in
  let cs = ConstrSolve.solve_partial cs in
  let uvars = List.fold_left collect_rec_fun_uvars T.UVar.Set.empty fds in
  let (targs, named, cs) =
    ParamGen.end_generalize_pure ~pos params uvars cs in
  let named' =
    List.map
      (fun (name, x, sch) -> (Name.to_unif name, T.SchemeExpr.to_scheme sch))
      named in
  let (env, fds) = add_rec_funs env targs named' fds in
  let fds = List.map (finalize_rec_fun fds) fds in
  let named =
    List.map (fun (name, x, sch) -> (Name.to_unif name, x, sch)) named in
  { rec_env    = env;
    rec_dds    = dds;
    rec_targs  = targs;
    rec_named  = named;
    rec_fds    = fds;
    rec_eff    = eff;
    rec_constr = cs
  }
