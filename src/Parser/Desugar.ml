(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** The first phase of desugaring and post-parsing *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Lang.Surface

type ty_def =
  | TD_Id of tvar * Raw.type_expr list
    (** Name with parameters *)

type let_pattern =
  | LP_Id of ident * Raw.expr list
    (** identifier definition with a list of formal parameters. *)

  | LP_Fun of ident * type_arg list * inst_arg list * Raw.expr list
    (** Function definition with list of formal type, implicit, and explicit
      parameters *)

  | LP_Pat of pattern
    (** Let definition with pattern-matching *)

(** Apply function [f] to each element of [xs]. Function [f] returns elements
  of [Either.t] type, that describes on which list the result should be put.
  It warns, when elements of the right list appear before somee element of the
  left list. *)
let rec map_inst_like f xs =
  match xs with
  | [] -> ([], [])
  | x :: xs ->
    begin match f x with
    | Either.Left y ->
      let (ys, zs) = map_inst_like f xs in
      (y :: ys, zs)
    | Either.Right z ->
      let (ys, zs) = map_inst_like f xs in
      if not (List.is_empty ys) then
        Error.warn (Error.value_before_type_param x.pos);
      (ys, z :: zs)
    end

let rec tr_type_expr (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TWildcard -> make TWildcard
  | TParen tp -> make (tr_type_expr tp).data
  | TVar x    -> make (TVar x)
  | TPureArrow(tp1, tp2) ->
    make (TPureArrow(tr_scheme_expr tp1, tr_type_expr tp2))
  | TArrow(tp1, tp2, eff) ->
    make (TArrow(tr_scheme_expr tp1, tr_type_expr tp2, tr_type_expr eff))
  | TEffect(tps, ee) ->
    make (TEffect(List.map tr_type_expr tps, Option.map tr_type_expr ee))
  | TApp(tp1, tp2) ->
    make (TApp(tr_type_expr tp1, tr_type_expr tp2))
  | TRecord _ ->
    Error.fatal (Error.desugar_error tp.pos)

and tr_scheme_expr (tp : Raw.type_expr) =
  let pos = tp.pos in
  match tp.data with
  | TParen tp ->
    { (tr_scheme_expr tp) with sch_pos = pos }
  | TPureArrow({ data = TRecord flds; _}, tp) ->
    let (tvs, implicit) = map_inst_like tr_scheme_field flds in
    { sch_pos      = pos;
      sch_tvars    = tvs;
      sch_implicit = implicit;
      sch_body     = tr_type_expr tp
    }
  | TArrow({data = TRecord _; _}, _, _) ->
    Error.fatal (Error.impure_scheme pos)

  | TWildcard | TVar _ | TPureArrow _ | TArrow _ | TEffect _ | TApp _ ->
    { sch_pos      = pos;
      sch_tvars    = [];
      sch_implicit = [];
      sch_body     = tr_type_expr tp
    }
  | TRecord _ ->
    Error.fatal (Error.desugar_error tp.pos)

and tr_scheme_field (fld : Raw.ty_field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType tp ->
    Either.Left (tr_type_arg tp)
  | FldName n ->
    Either.Right (make (IName(n, make TWildcard)))
  | FldNameVal(n, tp) ->
    Either.Right (make (IName(n, tr_type_expr tp)))

(** Translate a type expression as a type parameter *)
and tr_type_arg (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TParen tp -> make (tr_type_arg tp).data
  | TVar x    -> make (TA_Var x)
  | TWildcard | TPureArrow _ | TArrow _ | TEffect _ | TApp _ | TRecord _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a left-hand-side of the type definition. The additional
  parameter is an accumulated list of formal parameters *)
let rec tr_type_def (tp : Raw.type_expr) args =
  match tp.data with
  | TVar x -> TD_Id(x, args)
  | TApp(tp1, tp2) -> tr_type_def tp1 (tp2 :: args)
  | TWildcard | TParen _ | TPureArrow _ | TArrow _ | TEffect _ | TRecord _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a simple pattern, i.e., pattern that cannot be applied to
  the list of parameters. This function makes sure, that the provided list
  of parameters is empty *)
let rec tr_simple_pattern (p : Raw.expr) ps =
  let make data = { p with data = data } in
  match ps, p.data with
  | [], EWildcard -> make PWildcard
  | [], EUnit     -> Error.fatal (Error.desugar_error p.pos)
  | [], EParen p  -> make (tr_pattern p []).data
  | [], EVar  x   -> make (PVar x)
  | [], EName n   -> make (PName n)
  | [], EAnnot(p, sch) -> make (PAnnot(tr_pattern p [], tr_scheme_expr sch))
  | [], (ECtor _ | EFn _ | EApp _ | EDefs _ | EMatch _ | EHandle _
    | ERecord _) ->
    assert false
  | p1 :: _, _ ->
    Error.fatal (Error.invalid_pattern_arg p1.pos)

(** Translate a pattern. Argument [ps] is an accumulated list of subpatterns *)
and tr_pattern (p : Raw.expr) ps =
  let make data = { p with data = data } in
  match p.data with
  | EWildcard | EUnit | EParen _ | EVar _ | EName _ | EAnnot _ ->
    tr_simple_pattern p ps
  | ECtor c -> make (PCtor(make c, List.map (fun p -> tr_pattern p []) ps))
  | EApp(p, p1) -> tr_pattern p (p1 :: ps)

  | EFn _ | EDefs _ | EMatch _ | EHandle _ | ERecord _ ->
    Error.fatal (Error.desugar_error p.pos)

(** Translate a formal parameter of a function *)
let rec tr_function_arg (arg : Raw.expr) =
  match arg.data with
  | EParen arg -> tr_function_arg arg
  | EAnnot(p, sch) ->
    ArgAnnot(tr_pattern p [], tr_scheme_expr sch)
  | EWildcard | EUnit | EVar _ | EName _ | ECtor _ | EApp _ ->
    ArgPattern (tr_pattern arg [])

  | EFn _ | EDefs _ | EMatch _ | EHandle _ | ERecord _ ->
    Error.fatal (Error.desugar_error arg.pos)

let tr_inst_arg (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType arg ->
    Either.Left (tr_type_arg arg)
  | FldName n ->
    Either.Right (make (IName(n, ArgPattern(make (PName n)))))
  | FldNameVal(n, e) ->
    Either.Right (make (IName(n, tr_function_arg e)))

(** Translate an expression as a let-pattern. Argument [ps] is an accumulated
  list of formal parameters/subpatterns *)
let rec tr_let_pattern (p : Raw.expr) ps =
  match p.data with
  | EWildcard | EUnit | EParen _ | ECtor _ | EAnnot _ ->
    LP_Pat (tr_pattern p ps)
  | EVar _ | EName _ ->
    let id =
      match p.data with
      | EVar  x -> IdVar  x
      | EName n -> IdName n
      | _ -> assert false
    in
    begin match ps with
    | { data = ERecord iargs; _ } :: ps ->
      let (targs, iargs) = map_inst_like tr_inst_arg iargs in
      LP_Fun(id, targs, iargs, ps)
    | _ ->
      LP_Id(id, ps)
    end
  | EApp(p, p1) -> tr_let_pattern p (p1 :: ps)

  | EFn _ | EDefs _ | EMatch _ | EHandle _ | ERecord _ ->
    Error.fatal (Error.desugar_error p.pos)

(** Translate a function, given a list of formal parameters *)
let rec tr_function args body =
  match args with
  | [] -> body
  | arg :: args ->
    { pos  = Position.join arg.pos body.pos;
      data = EFn(tr_function_arg arg, tr_function args body)
    }

let tr_poly_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EVar  x -> make (EVar  x)
  | EName n -> make (EName n)
  | ECtor c -> make (ECtor c)

  | EWildcard | EUnit | EParen _ | EFn _ | EApp _ | EDefs _ | EMatch _
  | EHandle _ | ERecord _ | EAnnot _ ->
    Error.fatal (Error.desugar_error e.pos)

let rec tr_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EUnit          -> make EUnit
  | EParen e       -> make (tr_expr e).data
  | EVar _ | EName _ | ECtor _ -> make (EPoly(tr_poly_expr e, []))
  | EFn(es, e)     -> make (tr_function es (tr_expr e)).data
  | EApp(e, { data = ERecord flds; _ }) ->
    make (EPoly(tr_poly_expr e, List.map tr_explicit_inst flds))
  | EApp(e1, e2)   -> make (EApp(tr_expr e1, tr_expr e2))
  | EDefs(defs, e) -> make (EDefs(tr_defs defs, tr_expr e))
  | EMatch(e, cls) -> make (EMatch(tr_expr e, List.map tr_match_clause cls))
  | EHandle(pat, e, h) ->
    make (EHandle(tr_pattern pat [], tr_expr e, tr_h_expr h))
  | EWildcard | ERecord _ | EAnnot _ ->
    Error.fatal (Error.desugar_error e.pos)

and tr_match_clause (cl : Raw.match_clause) =
  let make data = { cl with data = data } in
  match cl.data with
  | Clause(pat, body) ->
    make (Clause(tr_pattern pat [], tr_expr body))

and tr_h_expr (h : Raw.h_expr) =
  let make data = { h with data = data } in
  match h.data with
  | HEffect(x, r, e) ->
    make (HEffect(x, r, tr_expr e))

and tr_explicit_inst (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType _ ->
    Error.fatal (Error.desugar_error fld.pos)
  | FldName n ->
    make (IName(n, make (EPoly(make (EName n), []))))
  | FldNameVal(n, e) ->
    make (IName(n, tr_expr e))

and tr_def (def : Raw.def) =
  let make data = { def with data = data } in
  match def.data with
  | DLet(p, e) ->
    begin match tr_let_pattern p [] with
    | LP_Id(id, args) ->
      make (DLetId(id, tr_function args (tr_expr e)))
    | LP_Fun(id, targs, iargs, args) ->
      make (DLetFun(id, targs, iargs, tr_function args (tr_expr e)))
    | LP_Pat p -> make (DLetPat(p, tr_expr e))
    end
  | DImplicit n  -> make (DImplicit n)
  | DData(tp, cs) ->
    begin match tr_type_def tp [] with
    | TD_Id(x, args) ->
      make (DData(x, List.map tr_type_arg args, List.map tr_ctor_decl cs))
    end

and tr_defs defs = List.map tr_def defs

and tr_ctor_decl (d : Raw.ctor_decl) =
  let make data = { d with data = data } in
  match d.data with
  | CtorDecl(name, tps) -> make (CtorDecl(name, List.map tr_type_expr tps))

let tr_program (p : Raw.program) =
  let make data = { p with data = data } in
  make (EDefs(tr_defs p.data, make EUnit))
