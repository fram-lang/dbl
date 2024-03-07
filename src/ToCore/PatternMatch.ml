(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of pattern-matching *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common
open PatternContext

(** Take a variable that stores given value, and pass to given
  meta-continuation in order to produce an expression *)
let as_variable (v : T.value) cont =
  match v with
  | VVar x -> cont x
  | _ ->
    let x = Var.fresh () in
    T.ELetPure(x, T.EValue v, cont x)

(** Collect all type variables and regular variables bound by given pattern.
  Returns extended environment, list of type variables (ins Source and
  Target), and list of variables together with their types. *)
let rec var_of_pattern env (p : S.pattern) =
  match p.data with
  | PWildcard -> (env, [], [])
  | PVar(x, sch) -> (env, [], [(x, Type.tr_scheme env sch)])
  | PCtor(_, _, _, _, tvs, ps) ->
    let (env, tvs1) = Env.add_tvars env tvs in
    let (env, tvs2, xs2) = var_of_patterns env ps in
    (env, List.combine tvs tvs1 @ tvs2, xs2)

(** Collect all variables bound by given list of patterns. Returns extended
  environment, list of type variables, and list of variables together with
  their types. *)
and var_of_patterns env ps =
  match ps with
  | [] -> (env, [], [])
  | p :: ps ->
    let (env, tvs1, xs1) = var_of_pattern  env p  in
    let (env, tvs2, xs2) = var_of_patterns env ps in
    (env, tvs1 @ tvs2, xs1 @ xs2)

(** Create a function that represents a clause. *)
let rec mk_clause_fun tvs xs body =
  match tvs, xs with
  | [], [] -> T.EValue (T.VFn(Var.fresh (), T.Type.t_unit, body))
  | [], (x, tp) :: xs ->
    T.EValue (T.VFn(x, tp, mk_clause_fun [] xs body))
  | T.TVar.Ex tv :: tvs, xs ->
    T.EValue (T.VTFun(tv, mk_clause_fun tvs xs body))

(** Build an application of a clause-representing function *)
let rec mk_clause_app f tvs xs env =
  match tvs, xs with
  | [], [] -> T.EApp(T.VVar f, v_unit)
  | [], (x, _) :: xs ->
    let fv = Var.fresh () in
    T.ELetPure(fv, T.EApp(T.VVar f, T.VVar x),
      mk_clause_app fv [] xs env)
  | tv :: tvs, xs ->
    let (T.TVar.Ex a) = Env.lookup_tvar env tv in
    let fv = Var.fresh () in
    T.ELetPure(fv, T.ETApp(T.VVar f, T.TVar a),
      mk_clause_app fv tvs xs env)

(** Context of pattern-matching *)
module type MatchContext = sig
  (** Translation from Unif to Core *)
  val tr_expr : Env.t -> S.expr -> T.expr

  (** Location of the matching expression *)
  val pos     : Position.t

  (** Result type of the whole matching expression *)
  val res_tp  : T.ttype

  (** Result effect of the whole matching expression *)
  val res_eff : T.effect
end

module Make(Ctx : MatchContext) = struct
  (** Generalized clause of the pattern matching *)
  type clause = {
    cl_env      : Env.t;
      (** Environment of the clause *)

    cl_patterns : S.pattern list;
      (** List of patterns matched against the list of values *)

    cl_body     : Env.t -> T.expr;
      (** Body-generating function *)

    cl_small    : bool;
      (** Set if the clause is small, i.e., when it is safe to duplicate
        its body *)

    cl_used     : bool ref
      (* A flag that indicates if the pattern was used *)
  }

  (** Class of column (first patterns of generalized clauses) *)
  type column_class =
    | CC_Var
      (** All patterns are variables or wild-cards *)

    | CC_ADT of T.expr * S.ctor_decl list
      (** Algebraic data type. It store computationally irrelevant proof of
        the shape of the constructor and full list of constructors *)

  (** Return the class of the first column *)
  let rec column_class cls =
    match cls with
    | [] -> CC_Var
    | { cl_patterns = []; _ } :: _ -> assert false
    | { cl_patterns = p :: _; cl_env; _ } :: cls ->
      begin match p.data with
      | PWildcard | PVar _ -> column_class cls
      | PCtor(_, _, proof, ctors, _, _) ->
        CC_ADT(Ctx.tr_expr cl_env proof, ctors)
      end

  (** Make sure that all non-head-constructor clauses are small. If not,
    generate a function that represents a clause body. This function pass
    modified clauses to the meta-continuation *)
  let save_non_ctor_clause cl cont =
    match cl.cl_patterns with
    | _ when cl.cl_small -> cont cl
    | [] -> assert false
    | { data = PCtor _; _ } :: _ -> cont cl
    | { data = (PWildcard | PVar _ ); _ } :: _ ->
      let (env, tvs, xs) = var_of_patterns cl.cl_env cl.cl_patterns in
      let cl_f = Var.fresh () in
      T.ELetPure(cl_f, mk_clause_fun (List.map snd tvs) xs (cl.cl_body env),
        cont {
          cl_env      = cl.cl_env;
          cl_patterns = cl.cl_patterns;
          cl_body     = mk_clause_app cl_f (List.map fst tvs) xs;
          cl_small    = true;
          cl_used     = cl.cl_used
        })

  let rec save_non_ctor_clauses cls cont =
    match cls with
    | [] -> cont []
    | cl :: cls ->
      save_non_ctor_clause  cl  (fun cl ->
      save_non_ctor_clauses cls (fun cls ->
      cont (cl :: cls)))

  (* ======================================================================= *)

  (** Simplify a single clause of [CCVar] class, assuming that matched value
    is stored in variable [x]. *)
  let simplify_var_clause x cl =
    match cl.cl_patterns with
    | [] -> assert false
    | p :: ps ->
      begin match p.data with
      | PWildcard ->
        { cl_env      = cl.cl_env;
          cl_patterns = ps;
          cl_body     = cl.cl_body;
          cl_small    = cl.cl_small;
          cl_used     = cl.cl_used
        }
      | PVar(y, _) ->
        { cl_env      = cl.cl_env;
          cl_patterns = ps;
          cl_body     =
            (fun env -> T.ELetPure(y, T.EValue (T.VVar x), cl.cl_body env));
          cl_small    = cl.cl_small;
          cl_used     = cl.cl_used
        }
      | PCtor _ -> assert false
      end

  (** Simplify clauses of [CCVar] class, assuming that matched value is stored
    in variable [x]. *)
  let simplify_var x cls =
    List.map (simplify_var_clause x) cls

  (* ======================================================================= *)

  let simplify_ctor_clause v vs1 idx tvs cl =
    match cl.cl_patterns with
    | [] -> assert false
    | { data = PWildcard; pos } :: ps ->
      assert cl.cl_small;
      let dummy_pat = { S.data = S.PWildcard; S.pos = pos } in
      Some {
        cl_env      = cl.cl_env;
        cl_patterns = List.map (fun _ -> dummy_pat) vs1 @ ps;
        cl_body     = cl.cl_body;
        cl_small    = cl.cl_small;
        cl_used     = cl.cl_used
      }
    | { data = PVar(x, _); pos } :: ps ->
      assert cl.cl_small;
      let dummy_pat = { S.data = S.PWildcard; S.pos = pos } in
      let body env = T.ELetPure(x, T.EValue v, cl.cl_body env) in
      Some {
        cl_env      = cl.cl_env;
        cl_patterns = List.map (fun _ -> dummy_pat) vs1 @ ps;
        cl_body     = body;
        cl_small    = cl.cl_small;
        cl_used     = cl.cl_used
      }
    | { data = PCtor(_, n, _, _, ptvs, ps1); _ } :: ps when n = idx ->
      assert (List.length ps1 = List.length vs1);
      Some {
        cl_env      = Env.add_tvars' cl.cl_env ptvs tvs;
        cl_patterns = ps1 @ ps;
        cl_body     = cl.cl_body;
        cl_small    = cl.cl_small;
        cl_used     = cl.cl_used
      }
    | { data = PCtor _; _ } :: _ -> None

  (** Simplify and translate a constructor clause set. *)
  let rec simplify_ctor ctx v vs cls idx (ctor : S.ctor_decl) =
    let tvs =
      List.map
        (fun (_, a) ->
          let (T.Kind.Ex k) = tr_kind (S.TVar.kind a) in
          T.TVar.Ex (T.TVar.fresh k))
        ctor.ctor_targs in
    let xs1 =
      List.map (fun (name, _) -> (name, Var.fresh ())) ctor.ctor_named in
    let xs2 = List.map (fun _ -> Var.fresh ()) ctor.ctor_arg_schemes in
    let ctx =
      focus_with ctx
        (ExCtor(ctor.ctor_name,
          List.map (fun (name, _) -> (name, ExHole)) xs1,
          List.map (fun _ -> ExHole) xs2)) in
    let xs = List.map snd xs1 @ xs2 in
    let vs1 = List.map (fun x -> T.VVar x) xs in
    let cls = List.filter_map (simplify_ctor_clause v vs1 idx tvs) cls in
    { T.cl_tvars = tvs;
      T.cl_vars  = xs;
      T.cl_body  = tr_match ctx (vs1 @ vs) cls
    }

  (* ======================================================================= *)

  (** Main function of the translation. It solves a bit more general problem:
    it takes list of values [vs] and list of clauses [cls], where each of
    clauses have list of patterns (the same length as [vs]). *)
  and tr_match ctx vs cls =
    match vs, cls with
    | [], cl :: _ ->
      assert (List.is_empty cl.cl_patterns);
      cl.cl_used := true;
      cl.cl_body cl.cl_env

    | [], [] ->
      (* TODO: non-exhaustive pattern-match doesn't have to be fatal *)
      Error.fatal (Error.non_exhaustive_match ~pos:Ctx.pos ctx)

    | v :: vs, _ ->
      begin match column_class cls with
      | CC_Var ->
        as_variable v (fun x ->
        tr_match (refocus ctx) vs (simplify_var x cls))

      | CC_ADT(proof, ctors) ->
        as_variable v (fun x ->
        save_non_ctor_clauses cls (fun cls ->
        let cls = List.mapi (simplify_ctor ctx (T.VVar x) vs cls) ctors in
        T.EMatch(proof, v, cls, Ctx.res_tp, Ctx.res_eff)))
      end

  (** Create an internal representation of a clause that matches single value
    *)
  let mk_clause env (pat, body) =
    { cl_env      = env;
      cl_patterns = [ pat ];
      cl_body     = (fun env -> Ctx.tr_expr env body);
      cl_small    = false;
      cl_used     = ref false
    }

  let tr_single_match env v cls =
    let cls = List.map (mk_clause env) cls in
    tr_match CtxRoot [v] cls
    (* TODO: warn redundant patterns/clauses *)
end

let tr_single_match ~pos ~env ~tr_expr v cls tp eff =
  let module M = Make(struct
    let tr_expr = tr_expr
    let pos     = pos
    let res_tp  = tp
    let res_eff = eff
  end)
  in
  M.tr_single_match env v cls
