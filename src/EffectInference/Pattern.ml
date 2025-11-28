(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of patterns to the internal representation. *)

open Common

module PEnv = struct
  type t =
    { tvars : (S.tvar * T.tvar) list;
      vars  : (T.var * T.scheme) list;
      eff   : T.ceffect
    }

  let empty = { tvars = []; vars = []; eff = T.Pure }

  let add_tvar env x y =
    { env with tvars = (x, y) :: env.tvars }

  let add_var env x sch =
    { env with vars = (x, sch) :: env.vars }

  let join penv1 penv2 =
    { tvars = penv1.tvars @ penv2.tvars;
      vars  = penv1.vars @ penv2.vars;
      eff   = T.CEffect.join penv1.eff penv2.eff
    }

  let open_penv env penv =
    let open_tvar (env, sub) (x, y1) =
      let (env, y2) = Env.add_tvar env x in
      ((env, T.Subst.rename sub y1 y2), y2)
    in
    let ((env, sub), tvs) =
      List.fold_left_map open_tvar (env, T.Subst.empty) penv.tvars in
    let open_var env (x, sch) =
      let sch = T.Scheme.subst sub sch in
      let env = Env.add_poly_var env x sch in
      (env, (x, sch))
    in
    let (env, xs) = List.fold_left_map open_var env penv.vars in
    (env, tvs, xs)

  let tvars env = List.map snd env.tvars

  let vars env = List.map fst env.vars

  let has_existential penv =
    match penv.tvars with
    | []     -> false
    | _ :: _ -> true

  let collect_gvars ~outer_scope penv gvs =
    List.fold_left
      (fun gvs (_, sch) -> T.Scheme.collect_gvars ~outer_scope sch gvs)
      gvs
      penv.vars
end

(* ========================================================================= *)

type t =
  | PWildcard
  | PAs       of t * T.var
  | PCtor     of
      { name  : string;
        idx   : int;
        proof : T.expr;
        ctors : T.ctor_decl list;
        tvars : T.tvar list;
        named : (T.name * t) list;
        args  : t list
      }

(* ========================================================================= *)

let open_tvars env targs tvars =
  assert (List.length targs = List.length tvars);
  let ((env, penv, sub), tvars) =
    List.fold_left_map
      (fun (env, penv, sub) (x, (_, y)) ->
        let (env, z) = Env.add_tvar env x in
        let penv = PEnv.add_tvar penv x z in
        let sub  = T.Subst.rename sub y z in
        ((env, penv, sub), z))
      (env, PEnv.empty, T.Subst.empty)
      (List.combine tvars targs)
  in
  (env, penv, sub, tvars)

let rec check_type env (pat : S.pattern) tp =
  match pat.data with
  | PWildcard | PAnnot _ ->
    check_scheme env pat (T.Scheme.of_type tp)

  | PAs(pat, x) ->
    let (pat, penv) = check_type env pat tp in
    (PAs(pat, x), PEnv.add_var penv x (T.Scheme.of_type tp))

  | PCtor(name, idx, prf, tvars, pats1, pats2) ->
    (* Infer the type of the pattern using the proof. *)
    let (prf, tp', ctors, eff) = ProofExpr.tr_proof_expr env prf in
    (* Check if the obtained type is a supertype of the expected type. *)
    let origin = OPatternType(pat.pos, pat.pp, tp, tp') in
    Subtyping.subtype ~origin env tp tp';
    let ctor = List.nth ctors idx in
    (* Check parameteres *)
    begin match tvars with
    | [] ->
      assert (List.is_empty ctor.ctor_targs);
      let penv = PEnv.empty in
      let sub  = T.Subst.empty in
      let (penv, pats1) =
        check_named_patterns env penv sub pats1 ctor.ctor_named in
      let (penv, pats2) =
        check_patterns env penv sub pats2 ctor.ctor_arg_schemes in
      (PCtor
        { name  = name;
          idx   = idx;
          proof = prf;
          ctors = ctors;
          tvars = [];
          named = pats1;
          args  = pats2
        }, penv)

    | _ ->
      let outer_env = env in
      let env = Env.enter_scope env in
      let (env, penv, sub, tvars) =
        open_tvars env ctor.ctor_targs tvars in
      let (penv, pats1) =
        check_named_patterns env penv sub pats1 ctor.ctor_named in
      let (penv, pats2) =
        check_patterns env penv sub pats2 ctor.ctor_arg_schemes in
      let gvs =
        PEnv.collect_gvars ~outer_scope:(Env.scope outer_env)
          penv T.GVar.Set.empty in
      ConstrSolver.leave_scope_with_gvars ~outer_env ~tvars
        (Env.constraints env) gvs;
      (PCtor
        { name  = name;
          idx   = idx;
          proof = prf;
          ctors = ctors;
          tvars = tvars;
          named = pats1;
          args  = pats2
        }, penv)
    end

and check_named_patterns env penv sub pats schs =
  assert (List.length pats = List.length schs);
  List.fold_left_map
    (fun penv (pat, (name, sch)) ->
      let (pat, penv2) = check_scheme env pat (T.Scheme.subst sub sch) in
      (PEnv.join penv penv2, (name, pat)))
    penv
    (List.combine pats schs)

and check_patterns env penv sub pats schs =
  assert (List.length pats = List.length schs);
  List.fold_left_map
    (fun penv (pat, sch) ->
      let (pat, penv2) = check_scheme env pat (T.Scheme.subst sub sch) in
      (PEnv.join penv penv2, pat))
    penv
    (List.combine pats schs)

and check_scheme env (pat : S.pattern) sch =
  match pat.data with
  | PWildcard -> (PWildcard, PEnv.empty)

  | PAs(pat, x) ->
    let (pat, penv) = check_scheme env pat sch in
    (PAs(pat, x), PEnv.add_var penv x sch)

  | PCtor _ ->
    begin match T.Scheme.to_type sch with
    | Some tp -> check_type env pat tp
    | None    -> assert false
    end

  | PAnnot(pat', sch') ->
    let sch' = Type.tr_scheme_expr env sch' in
    let origin = OPatternScheme(pat.pos, pat.pp, sch, sch') in
    Subtyping.subscheme ~origin env sch sch';
    check_scheme env pat' sch'
