(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of deep pattern-matching *)

open Common
open PatternContext

type clause = {
  cl_pos   : Position.t;
  cl_pat   : Pattern.t;
  cl_body  : T.expr;
  cl_tvars : T.tvar list;
  cl_vars  : T.var list
}

(* ========================================================================= *)

(** Expression-building do-notation *)
let (let^ ) m cont = m cont
let (let* ) m f cont = m (Fun.flip f cont)
let return x cont = cont x

let m_let_pure x body cont =
  T.ELetPure(x, body, cont ())

let as_variable (e : T.expr) cont =
  match e with
  | EVar x -> cont x
  | _ ->
    let x = Var.fresh () in
    T.ELet(x, e, cont x)

(* ========================================================================= *)

(** Internal representation of a clause. *)
type iclause = {
  c_patterns : Pattern.t list;
    (** List of patterns matched against the list of values *)

  c_clause   : clause;
    (** Original clause *)

  c_tvar_map : T.typ T.TVar.Map.t;
    (** Map from type variables in the original clause to the types already
      introduced in the translation *)

  c_var_map  : T.expr Var.Map.t;
    (** Map from variables in the original clause to the values already
      introduced in the translation *)

  c_used     : bool ref
    (** A flag that indicates if the pattern was used *)
}

let warn_if_unused cl =
  if not !(cl.c_used) then
    Error.warn (Error.unused_pattern ~pos:cl.c_clause.cl_pos)

(** Apply the body function to the parameters. *)
let make_body c =
  let mk_type_arg x =
    match T.TVar.Map.find_opt x c.c_tvar_map with
    | Some tp -> tp
    | None    -> assert false
  in
  let mk_arg x =
    match Var.Map.find_opt x c.c_var_map with
    | Some v -> v
    | None   -> assert false
  in
  let cl = c.c_clause in
  let body = cl.cl_body in
  let body = ExprUtils.mk_tapps body (List.map mk_type_arg cl.cl_tvars) in
  let body = ExprUtils.mk_apps body (List.map mk_arg cl.cl_vars) in
  T.EApp(body, ExprUtils.mk_unit)

(* ========================================================================= *)

(** Drop wild-card patterns from the head position in given clause *)
let drop_wildcard (cl : iclause) =
  match cl.c_patterns with
  | PWildcard :: pats -> { cl with c_patterns = pats }
  | _ -> assert false

(** Simplify as-patterns on the head position in given clause list *)
let rec simplify_as_pattern x (cl : iclause) =
  match cl.c_patterns with
  | [] -> assert false
  | (PWildcard | PCtor _ | POr _) :: pats -> cl
  | PAs(pat, y) :: pats ->
    let cl =
      { cl with
        c_patterns = pat :: pats;
        c_var_map  = Var.Map.add y (T.EVar x) cl.c_var_map
      } in
    simplify_as_pattern x cl

(** Expand or-patterns at head position (recursively for nested or-patterns) *)
let rec expand_or_at_head (cl : iclause) =
  match cl.c_patterns with
  | POr(pat1, pat2) :: pats ->
    expand_or_at_head { cl with c_patterns = pat1 :: pats } @
    expand_or_at_head { cl with c_patterns = pat2 :: pats }
  | _ -> [cl]

(** Simplify as-patterns on the head position in given clause list *)
let simplify_as_patterns x cls =
  (* First expand or-patterns, then simplify as-patterns *)
  let cls = List.concat_map expand_or_at_head cls in
  List.map (simplify_as_pattern x) cls

(* ========================================================================= *)

(** Simplify the constructor clause, assuming that it matches the given
  constructor of the given index. If it matches other constructor, return
  [None]. The [tvs] is the list of type variables introduced by the generated
  match clause. *)
let simplify_ctor idx (ctor : T.ctor_decl) tvs cl =
  match cl.c_patterns with
  | [] -> assert false
  | PWildcard :: pats ->
    let pats1 = List.map (fun _ -> Pattern.PWildcard) ctor.ctor_named in
    let pats2 = List.map (fun _ -> Pattern.PWildcard) ctor.ctor_arg_schemes in
    Some { cl with c_patterns = pats1 @ pats2 @ pats }

  | PCtor pc :: pats when pc.idx = idx ->
    assert (List.length pc.tvars = List.length tvs);
    assert (List.length pc.named = List.length ctor.ctor_named);
    assert (List.length pc.args  = List.length ctor.ctor_arg_schemes);
    let tvar_map =
      List.fold_left2
        (fun tvar_map x y -> T.TVar.Map.add x (T.Type.t_var y) tvar_map)
        cl.c_tvar_map pc.tvars tvs in
    Some
      { cl with
        c_patterns = List.map snd pc.named @ pc.args @ pats;
        c_tvar_map = tvar_map
      }

  | PCtor _ :: _ -> None

  | PAs _ :: _ | POr _ :: _ ->
    (* As-patterns and or-patterns should be already simplified *)
    assert false

(* ========================================================================= *)

(** Class of the column of patterns *)
type column_class =
  | CC_Wildcard
    (** All patterns wild-cards *)

  | CC_ADT of T.expr * T.ctor_decl list
    (** There is a constructor pattern in the column. It stores computationally
      irrelevant proof of the shape of the constructor and the list of all
      constructors. *)

(** Classify the first column of patterns in the clause list *)
let rec column_class (cls : iclause list) =
  match cls with
  | [] -> CC_Wildcard

  | cl :: cls ->
    begin match cl.c_patterns with
    | [] -> assert false
    | PWildcard :: _ -> column_class cls
    | PCtor cp :: _ -> CC_ADT(cp.proof, cp.ctors)
    | PAs _ :: _ | POr _ :: _ ->
      (* As-patterns and or-patterns should be already simplified *)
      assert false
    end

(* ========================================================================= *)
(** Context of pattern-matching *)
module type MatchContext = sig
  (** Location of the matching expression *)
  val pos     : Position.t

  (** Result type of the whole matching expression *)
  val res_tp  : T.typ

  (** Result effect of the whole matching expression *)
  val res_eff : T.ceffect
end

module Make(Ctx : MatchContext) = struct
  (** Main function of the translation. It solves a bit more general problem:
    it takes list of values [vs] and list of clauses [cls], where each of
    clauses have list of patterns. *)
  let rec tr_match ctx xs cls =
    match xs, cls with
    | [], [] -> 
      (* TODO: non-exhaustive pattern-match doesn't have to be fatal *)
      Error.fatal (Error.non_exhaustive_match ~pos:Ctx.pos ctx)

    | [], cl :: _ ->
      cl.c_used := true;
      make_body cl

    | x :: xs, cls ->
      let cls = simplify_as_patterns x cls in
      begin match column_class cls with
      | CC_Wildcard ->
        tr_match (refocus ctx) xs (List.map drop_wildcard cls)

      | CC_ADT(proof, ctors) ->
        let match_cls = List.mapi (tr_match_clause ctx xs cls) ctors in
        T.EMatch(proof, T.EVar x, match_cls, Ctx.res_tp, Ctx.res_eff)
      end

  (** Build a match clause for a single constructor. *)
  and tr_match_clause ctx xs cls idx (ctor : T.ctor_decl) =
    let tvs =
      List.map
        (fun (_, x) -> T.TVar.clone ~scope:Scope.any x)
        ctor.ctor_targs
    in
    let cls = List.filter_map (simplify_ctor idx ctor tvs) cls in
    let xs1 = List.map (fun _ -> Var.fresh ()) ctor.ctor_named in
    let xs2 = List.map (fun _ -> Var.fresh ()) ctor.ctor_arg_schemes in
    let ctxs1 = List.map (fun (name, _) -> (name, ExHole)) ctor.ctor_named in
    let ctxs2 = List.map (fun _ -> ExHole) ctor.ctor_arg_schemes in
    let ctx = focus_with ctx (ExCtor(ctor.ctor_name, ctxs1, ctxs2)) in
    { T.cl_tvars = tvs;
      T.cl_vars  = xs1 @ xs2;
      T.cl_body  = tr_match ctx (xs1 @ xs2 @ xs) cls
    }

  let tr_single_match x cls =
    let e = tr_match CtxRoot [x] cls in
    List.iter warn_if_unused cls;
    e
end

(* ========================================================================= *)

(** Ensure that all clauses are small *)
let rec make_clauses_small cls =
  match cls with
  | [] -> return []
  | cl :: cls ->
    let x = Var.fresh () in
    let* ()  = m_let_pure x cl.cl_body in
    let  cl  = { cl with cl_body = T.EVar x } in
    let* cls = make_clauses_small cls in
    return (cl :: cls)

let tr_match ~pos e ~tp ~eff cls =
  let^ cls = make_clauses_small cls in
  let mk_iclause cl =
    { c_patterns = [cl.cl_pat];
      c_clause   = cl;
      c_tvar_map = T.TVar.Map.empty;
      c_var_map  = Var.Map.empty;
      c_used     = ref false
    } in
  let cls = List.map mk_iclause cls in
  let module M = Make(struct
    let pos    = pos
    let res_tp = tp
    let res_eff = eff
  end) in
  let^ x = as_variable e in
  M.tr_single_match x cls
