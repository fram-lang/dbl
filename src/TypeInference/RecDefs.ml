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
      args         : T.named_tvar list;
      args_env     : PartialEnv.t;
      ctors        : S.ctor_decl list
    }

  | D1_Section of def1 T.node list

(** The main function of the first pass. *)
let rec prepare_rec_data env (def : S.def) =
  let make data = { def with data = data } in
  let pos = def.pos in
  match def.data with
  | DLetId(public, id, body) ->
    (env, make (D1_LetId(public, id, body)))

  | DLetPat _ ->
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)

  | DMethodFn _ ->
    failwith "We are going to remove method functions"

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

  | DBlock defs | DRec defs ->
    let (env, defs) = List.fold_left_map prepare_rec_data env defs in
    (env, make (D1_Section defs))

  | DModule(public, name, defs) ->
    (* TODO: recursive modules are not supported, yet *)
    Error.fatal (Error.invalid_rec_def ~pos:def.pos)

  | DOpen(public, path) ->
    let m = ModulePath.lookup_module env path in
    let env = Env.open_module ~public env m in
    (env, make (D1_Blank))

  | DReplExpr _ ->
    assert false

(* ========================================================================= *)
(** The second pass: finalization of checking labels and ADTs, i.e., extending
  the environment by term-level information (labels and proofs). Additionally,
  section value parameters are added to the environment *)

(** The second pass for single definition. See [finalize_rec_data_defs] for
  more details. *)
let rec finalize_rec_data ~nonrec_scope env (def : def1 T.node) =
  match def.data with
  | D1_Blank | D1_LetId _ -> (env, [], T.Pure)

  | D1_ValParam(name, x, sch) ->
    let (sch_env, params) = Env.begin_generalize env in
    let sch =
      match sch with
      | Some sch -> Type.tr_scheme sch_env sch
      | None     ->
        let tp =
          { T.pos  = def.pos;
            T.data = T.TE_Type (Env.fresh_uvar env T.Kind.k_type) }
        in
        T.SchemeExpr.of_type_expr tp
    in
    let env =
      ParamGen.end_generalize_declare ~pos:def.pos params env name x sch in
    (env, [], T.Pure)

  | D1_Label(tvar, public, id, sch_opt) ->
    let delim_tp = Env.fresh_uvar env T.Kind.k_type in
    let l_tp = T.Type.t_label delim_tp in
    let annot =
      match sch_opt with
      | None -> { def with data = T.TE_Type l_tp }
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
    let data_env = PartialEnv.extend env def.args_env in
    let ctors =
      DataType.check_ctor_decls ~data_targs:def.args data_env def.ctors in
    let (env, dd) =
      DataType.finalize_check
        ~public:def.public_ctors
        ~nonrec_scope env def.tvar
        ~name:def.name
        def.args ctors
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
  { rf3_scheme   : T.scheme;
      (** Type scheme of a recursive value *)

    rf3_targs    : T.named_tvar list;
      (** Explicit formal type parameters *)

    rf3_named    : (T.pattern * T.scheme) list;
      (** Explicit named formal parameters *)

    rf3_args     : (Position.t * T.pattern * T.scheme * T.effect) list;
      (** Formal parameters of the function, together with position and effect
        of the lambda abstractions. *)

    rf3_penv     : PartialEnv.t;
      (** Partial environment with formal parameters *)

    rf3_body     : S.expr;
      (** Body of the recursive value *)

    rf3_body_tp  : T.typ
      (** Guessed type of a body *)
  }

(** Representation of recursive definition after the third pass *)
type def3 =
  | D3_LetFun  of S.is_public * Name.t * T.var * rec_fun3
  | D3_Section of def3 T.node list

(** Information gathered by guessing the type of a recursive function *)
type rec_fun_body =
  { rfb_args     : (Position.t * T.pattern * T.scheme * T.effect) list;
      (** Formal parameters of the function, together with position and effect
        of the lambda abstractions. *)

    rfb_penv : PartialEnv.t;
      (** Partial environment with formal parameter *)

    rfb_body : S.expr;
      (** Body of the recursive function *)

    rfb_body_tp : T.typ;
      (** Guessed type of the body *)
  }

(** Check formal parameters and guess the type of a recursive value.
  This type is unified with the [tp] parameter, which should be an unification
  variable, but it tracks the scope of the guessed type. *)
let rec guess_rec_fun_type env (e : S.expr) tp =
  let pos = e.pos in
  match e.data with
  | EFn(pat, body) ->
    let (penv, pat, sch, eff1) = Pattern.infer_scheme env pat in
    let env = PartialEnv.extend env penv in
    let body_tp = Env.fresh_uvar env T.Kind.k_type in
    let (rfb, eff2) = guess_rec_fun_type env body body_tp in
    let eff = T.Effect.join eff1 eff2 in
    let fun_tp = T.Type.t_arrow sch body_tp eff in
    let pp = Env.pp_tree env in
    Error.check_unify_result ~pos
      (Unification.subtype env fun_tp tp)
      ~on_error:(Error.expr_type_mismatch ~pp fun_tp tp);
    { rfb_args    = (pos, pat, sch, eff) :: rfb.rfb_args;
      rfb_penv    = PartialEnv.join ~pp penv rfb.rfb_penv;
      rfb_body    = rfb.rfb_body;
      rfb_body_tp = rfb.rfb_body_tp
    }, T.Pure

  | EAnnot(_, tp_expr) ->
    let tp' = Type.tr_ttype env tp_expr |> T.TypeExpr.to_type in
    Error.check_unify_result ~pos
      (Unification.subtype env tp' tp)
      ~on_error:(Error.expr_type_mismatch ~pp:(Env.pp_tree env) tp' tp);
    { rfb_args    = [];
      rfb_penv    = PartialEnv.empty;
      rfb_body    = e;
      rfb_body_tp = tp
    }, T.Impure

  | EUnit | ENum _ | ENum64 _ | EStr _ | EChr _ | EPoly _ | EApp _ | EDefs _
  | EMatch _ | EHandler _ | EEffect _ | EExtern _ | ERepl _ ->
    { rfb_args    = [];
      rfb_penv    = PartialEnv.empty;
      rfb_body    = e;
      rfb_body_tp = tp
    }, T.Impure

(** Check formal parameters and guess the scheme of a recursive value *)
let rec_def_scheme ~pos env (body : S.poly_expr_def) =
  match body.data with
  | PE_Expr e ->
    let tp = Env.fresh_uvar env T.Kind.k_type in
    let (rfb, _) = guess_rec_fun_type env e tp in
    { rf3_scheme  = T.Scheme.of_type tp;
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
    let (env2, penv, tvars, named, eff1) =
      Pattern.infer_named_patterns env nps in
    begin match eff1 with
    | Pure -> ()
    | Impure -> Error.report (Error.func_not_pure ~pos)
    end;
    let body_tp = Env.fresh_uvar env2 T.Kind.k_type in
    let env = PartialEnv.extend env penv in
    let (rfb, _) = guess_rec_fun_type env body body_tp in
    let sch =
      { T.sch_targs = tvars;
        T.sch_named =
          List.map (fun (name, _, sch) -> (Name.to_unif name, sch)) named;
        T.sch_body  = body_tp
      } in
    let pp = Env.pp_tree env in
    { rf3_scheme  = sch;
      rf3_targs   = tvars;
      rf3_named   = List.map (fun (_, pat, sch) -> (pat, sch)) named;
      rf3_args    = rfb.rfb_args;
      rf3_penv    = PartialEnv.join ~pp penv rfb.rfb_penv;
      rf3_body    = rfb.rfb_body;
      rf3_body_tp = rfb.rfb_body_tp
    }

(** The third pass for a single definition. It returns an option type: value
  [None] means that the definition does not produce any recursive value. *)
let rec prepare_rec_fun env (def : def1 T.node) =
  let pos = def.pos in
  match def.data with
  | D1_Blank | D1_Label _ | D1_Data _ | D1_ValParam _ -> None

  | D1_LetId(public, name, body) ->
    let rf3 = rec_def_scheme ~pos env body in
    let sch = rf3.rf3_scheme in
    let name = NameUtils.tr_ident ~pos ~pp:(Env.pp_tree env) name sch in
    let (env, x) = Env.add_val env name sch in
    let def = { def with data = D3_LetFun(public, name, x, rf3) } in
    Some (env, def)

  | D1_Section defs ->
    let (env, defs) = prepare_rec_funs env defs in
    Some (env, { def with data = D3_Section defs })

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
      scheme  : T.scheme;
      targs   : T.named_tvar list;
      named   : (T.pattern * T.scheme) list;
      args    : (Position.t * T.pattern * T.scheme * T.effect) list;
      body    : T.expr;
      body_tp : T.typ
    }

  | D4_Section of def4 T.node list

(** The main function of the fourth pass. *)
let rec check_rec_fun ~tcfix env (def : def3 T.node) =
  let open (val tcfix : TCFix) in
  match def.data with
  | D3_LetFun(public, name, x, rf3) ->
    let body_env = PartialEnv.extend env rf3.rf3_penv in
    let er = check_expr_type body_env rf3.rf3_body rf3.rf3_body_tp in
    begin match rf3.rf3_args, er.er_effect with
    | _ :: _, _ | _, Pure -> ()
    | [], Impure -> Error.report (Error.func_not_pure ~pos:def.pos)
    end;
    let def =
      { def with
        data = D4_LetFun
          { public  = public;
            name    = name;
            var     = x;
            scheme  = rf3.rf3_scheme;
            targs   = rf3.rf3_targs;
            named   = rf3.rf3_named;
            args    = rf3.rf3_args;
            body    = er.er_expr;
            body_tp = rf3.rf3_body_tp
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
    T.Scheme.collect_uvars scheme uvs

  | D4_Section defs ->
    List.fold_left collect_rec_fun_uvars uvs defs

(* ========================================================================= *)
(** The sixth pass: building final environment *)

(** Representation of recursive definition after the sixth pass *)
type def6 = {
  d6_pos         : Position.t;
    (** Position of the definition *)

  d6_poly_var    : T.var;
    (** Variable that represents more general form of the recursive value *)

  d6_poly_scheme : T.scheme;
    (** More general scheme of the recursive value *)

  d6_mono_var    : T.var;
    (** Variable that represents less general form of the recursive value *)

  d6_mono_body   : T.poly_expr;
    (** Less general form of the recursive value *)

  d6_targs       : T.tvar list;
    (** Type parameters of the recursive value, including section-declared
      parameters. *)

  d6_named       : (T.var * T.scheme) list;
    (** Named arguments of the recursive value, including section-declared
      parameters. *)

  d6_named_pats  : (T.var * T.pattern) list;
    (** Patterns of named arguments. *)

  d6_args        : (Position.t * T.pattern * T.scheme * T.effect) list;
    (** Formal parameters of the function, together with position and effect
      of the lambda abstractions. *)

  d6_body        : T.expr;
    (** Body of the recursive value *)

  d6_body_tp     : T.typ;
    (** Type of the body *)
}

(** The sixth pass for a single definition *)
let rec add_rec_fun env targs1 named1 (def : def4 T.node) =
  let make data = { def with data = data } in
  match def.data with
  | D4_LetFun d ->
    let y_sch =
      { T.sch_targs = targs1 @ d.scheme.sch_targs;
        T.sch_named =
          List.map
            (fun (name, _, sch) ->
              (Name.to_unif name, T.SchemeExpr.to_scheme sch))
            named1 @
          d.scheme.sch_named;
        T.sch_body  = d.scheme.sch_body
      } in
    let (env, y) = Env.add_val ~public:d.public env d.name y_sch in
    let named2 =
      List.map (fun (pat, sch) -> (Var.fresh (), pat, sch)) d.named in
    let mono_body =
      make (T.EPolyFun(
        List.map snd d.targs,
        List.map (fun (x, _, sch) -> (x, sch)) named2,
        make (T.EInst(
          make (T.EVar y),
          List.map
            (fun (_, x) -> make (T.TE_Type (T.Type.t_var x)))
            (targs1 @ d.targs),
          List.map (fun (_, x, _) -> make (T.EVar x)) named1 @
          List.map (fun (x, _, _) -> make (T.EVar x)) named2))))
    in
    let named_pats =
      List.map
        (fun (_, x, sch_expr) ->
          let sch = T.SchemeExpr.to_scheme sch_expr in
          let pat =
            make (T.PAnnot(
              make (T.PAs(make(T.PWildcard sch), x, sch)),
              sch_expr)) in
          (x, pat))
        named1 @
      List.map (fun (x, pat, _) -> (x, pat)) named2
    in
    let def =
      { d6_pos         = def.pos;
        d6_poly_var    = y;
        d6_poly_scheme = y_sch;
        d6_mono_var    = d.var;
        d6_mono_body   = mono_body;
        d6_targs       = List.map snd (targs1 @ d.targs);
        d6_named       =
          List.map (fun (_, x, sch) -> (x, T.SchemeExpr.to_scheme sch)) named1
          @ List.map (fun (x, _, sch) -> (x, sch)) named2;
        d6_named_pats  = named_pats;
        d6_args        = d.args;
        d6_body        = d.body;
        d6_body_tp     = d.body_tp
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
(** The seventh pass: update bodies of recursive definitions with local
  definitions of less-generalized functions *)

(** Update the body of a recursive definition, adding local definitions of
  less-generalized functions after the first impure function. This function
  also checks if the recursive definition is productive. *)
let update_rec_body ~pos ~ctx fds (body : T.expr) =
  let rec update_expr (e : T.expr) =
    let make data = { body with data = data } in
    match e.data with
    | EUnitPrf | ENum _ | ENum64 _ | EStr _ | EChr _ | EExtern _ -> e
    
    | EFn(x, sch, body, Impure) ->
      ctx e

    | EFn(x, sch, body, Pure) ->
      make (T.EFn(x, sch, update_expr body, Pure))

    | EInst(e, tps, es) ->
      make (T.EInst(update_poly_expr e, tps, List.map update_poly_expr es))

    | EAppPoly(e1, e2) ->
      make (T.EAppPoly(update_expr e1, update_poly_expr e2))

    | EAppMono(e1, e2) ->
      make (T.EAppMono(update_expr e1, update_expr e2))

    | ELetPoly(x, e1, e2) ->
      make (T.ELetPoly(x, update_poly_expr e1, update_expr e2))

    | ELetMono(x, e1, e2) ->
      make (T.ELetMono(x, update_expr e1, update_expr e2))

    | ELetRec(fds, e) ->
      let fds =
        List.map (fun (x, sch, e) -> (x, sch, update_poly_expr e)) fds in
      make (T.ELetRec(fds, update_expr e))

    | ECtor(prf, n, tps, nargs, args) ->
      make (T.ECtor(update_expr prf, n, tps,
        List.map update_poly_expr nargs, List.map update_poly_expr args))

    | EData(dds, e) ->
      make (T.EData(dds, update_expr e))

    | EMatchEmpty(prf, e, tp, Pure) ->
      make (T.EMatchEmpty(update_expr prf, update_expr e, tp, Pure))

    | EMatch(e, cls, tp, Pure) ->
      let cls = List.map (fun (pat, e) -> (pat, update_expr e)) cls in
      make (T.EMatch(update_expr e, cls, tp, Pure))

    | EMatchPoly(e1, pat, e2, tp, Pure) ->
      make (T.EMatchPoly(update_poly_expr e1, pat, update_expr e2, tp, Pure))

    | EHandler h ->
      make (T.EHandler {
        label    = h.label;
        effect   = h.effect;
        delim_tp = h.delim_tp;
        cap_type = h.cap_type;
        cap_body = update_expr h.cap_body;
        ret_var  = h.ret_var;
        body_tp  = h.body_tp;
        ret_body = update_expr h.ret_body;
        fin_var  = h.fin_var;
        fin_body = update_expr h.fin_body
      })

    | EAnnot(e, tp, eff) ->
      make (T.EAnnot(update_expr e, tp, eff))

    | EMatchEmpty(_, _, _, Impure) | EMatch(_, _, _, Impure)
    | EMatchPoly(_, _, _, _, Impure) | EHandle _ | EEffect _ ->
      Error.report (Error.non_productive_rec_def ~pos);
      e

    | ERepl _ | EReplExpr _ -> assert false

  and update_poly_expr (e : T.poly_expr) =
    let make data = { body with data = data } in
    match e.T.data with
    | T.EOptionPrf -> e
    | T.EVar x ->
      if List.exists (fun fd -> Var.equal x fd.d6_mono_var) fds then
        Error.report (Error.non_productive_rec_def ~pos);
      e
    | T.EPolyFun(targs, named, body) ->
      make (T.EPolyFun(targs, named, update_expr body))
    | T.EHole _ ->
      (* we have no means to check if the hole is productive *)
      Error.report (Error.non_productive_rec_def ~pos);
      e
  in
  update_expr body

(** Build a function from a list of arguments and a body. *)
let mk_function args body body_tp =
  let mk_fn (pos, pat, sch, eff) (body, body_tp) =
    let (x, body) = ExprUtils.match_var pat body body_tp eff in
    let body_tp = T.Type.t_arrow sch body_tp eff in
    { T.pos  = pos;
      T.data = T.EFn(x, sch, body, eff)
    }, body_tp
  in
  List.fold_right mk_fn args (body, body_tp)

(** The main function of the seventh pass *)
let finalize_rec_fun (fds : def6 list) (fd : def6) =
  (* Build a local context that provides less-generalized functions *)
  let build_local_inst fd' body =
    { T.pos  = fd.d6_pos;
      T.data = T.ELetPoly(fd'.d6_mono_var, fd'.d6_mono_body, body)
    } in
  let build_local_ctx body =
    List.fold_right build_local_inst fds body in
  (* Build a body of the definition, by moving matching of arguments to the
    first impure function, or to the other place, where it is possible. *)
  let rec abstract_args pats args =
    match args with
    | [] ->
      let ctx = build_local_ctx in
      let body = update_rec_body ~pos:fd.d6_pos ~ctx fds fd.d6_body in
      ExprUtils.match_args pats body fd.d6_body_tp Pure
    | (pos, pat, sch, T.Pure) :: args ->
      let x = Var.fresh () in
      let body = abstract_args ((x, pat) :: pats) args in
      { T.pos  = pos;
        T.data = T.EFn(x, sch, body, T.Pure)
      }
    | (pos, pat, sch, T.Impure) :: args ->
      let x = Var.fresh () in
      let pats = List.rev ((x, pat) :: pats) in
      let (body, body_tp) = mk_function args fd.d6_body fd.d6_body_tp in
      let body = ExprUtils.match_args pats body body_tp Impure in
      let body = build_local_ctx body in
      { T.pos  = pos;
        T.data = T.EFn(x, sch, body, T.Impure)
      }
  in
  let body = abstract_args fd.d6_named_pats fd.d6_args in
  let poly_expr =
    { T.pos  = fd.d6_pos;
      T.data = T.EPolyFun(fd.d6_targs, fd.d6_named, body)
    } in
  (fd.d6_poly_var, fd.d6_poly_scheme, poly_expr)

(* ========================================================================= *)

type 'st rec_result =
  { rec_env    : ('st, sec) opn Env.t;
    rec_dds    : T.data_def list;
    rec_fds    : T.rec_def list;
    rec_effect : T.effect;
    rec_constr : Constr.t list
  }

let check_rec_defs ~tcfix ~pos env defs =
  let nonrec_scope = Env.scope env in
  let (env, defs) = List.fold_left_map prepare_rec_data env defs in
  let (env, dds, eff) = finalize_rec_data_defs ~nonrec_scope env defs in
  let (rec_env, params) = Env.begin_generalize env in
  let (rec_env, fds) = prepare_rec_funs rec_env defs in
  let (cs, fds) = check_rec_funs ~tcfix rec_env fds in
  let cs = ConstrSolve.solve_partial cs in
  let uvars = List.fold_left collect_rec_fun_uvars T.UVar.Set.empty fds in
  let (targs, named, cs) =
    ParamGen.end_generalize_pure ~pos params uvars cs in
  let (env, fds) = add_rec_funs env targs named fds in
  let fds = List.map (finalize_rec_fun fds) fds in
  { rec_env    = env;
    rec_dds    = dds;
    rec_fds    = fds;
    rec_effect = eff;
    rec_constr = cs
  }
