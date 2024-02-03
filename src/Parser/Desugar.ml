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

  | LP_Fun of ident * named_type_arg list * named_arg list * Raw.expr list
    (** Function definition with list of formal type, named, and explicit
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

let ident_of_name (name : Raw.name) =
  match name with
  | NVar x      -> IdVar x
  | NImplicit n -> IdImplicit n

let rec tr_type_expr (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TWildcard -> make TWildcard
  | TParen tp -> make (tr_type_expr tp).data
  | TVar x    -> make (TVar x)
  | TArrow(tp1, tp2) ->
    let sch = tr_scheme_expr tp1 in
    begin match tr_eff_type tp2 with
    | (None, tp2) -> make (TPureArrow(sch, tp2))
    | (Some eff, tp2) -> make (TArrow(sch, tp2, eff))
    end
  | TEffect(tps, ee) ->
    make (TEffect(List.map tr_type_expr tps, Option.map tr_type_expr ee))
  | TApp(tp1, tp2) ->
    make (TApp(tr_type_expr tp1, tr_type_expr tp2))
  | TRecord _ | TTypeLbl _ | TEffectLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

and tr_eff_type (tp : Raw.type_expr) =
  let make l_pos data = { pos = Position.join l_pos tp.pos; data } in
  match tp.data with
  | TWildcard | TParen _ | TVar _ | TArrow _ | TEffect _ | TRecord _
  | TTypeLbl _ | TEffectLbl _ ->
    (None, tr_type_expr tp)

  | TApp({ data = TEffect _; _ } as eff, tp) ->
    (Some (tr_type_expr eff), tr_type_expr tp)

  | TApp(tp1, tp2) ->
    let (eff, tp1) = tr_eff_type tp1 in
    (eff, make tp1.pos (TApp(tp1, tr_type_expr tp2)))

and tr_scheme_expr (tp : Raw.type_expr) =
  let pos = tp.pos in
  match tp.data with
  | TParen tp ->
    { (tr_scheme_expr tp) with sch_pos = pos }
  | TArrow({ data = TRecord flds; _}, tp) ->
    let (tvs, named) = map_inst_like tr_scheme_field flds in
    begin match tr_eff_type tp with
    | (None, tp) ->
      { sch_pos   = pos;
        sch_targs = tvs;
        sch_named = named;
        sch_body  = tp
      }
    | (Some _, _) ->
      Error.fatal (Error.impure_scheme pos)
    end

  | TWildcard | TVar _ | TArrow _ | TEffect _ | TApp _ ->
    { sch_pos   = pos;
      sch_targs = [];
      sch_named = [];
      sch_body  = tr_type_expr tp
    }
  | TRecord _ | TTypeLbl _ | TEffectLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

and tr_scheme_field (fld : Raw.ty_field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType tp ->
    Either.Left (make (TNAnon, tr_type_arg tp))
  | FldEffect ->
    Either.Left (make (TNEffect, make TA_Effect))
  | FldEffectVal arg ->
    Either.Left (make (TNEffect, tr_type_arg arg))
  | FldType x ->
    Either.Left (make (TNVar x, make (TA_Var x)))
  | FldTypeVal(x, arg) ->
    Either.Left (make (TNVar x, tr_type_arg arg))
  | FldName n ->
    let sch =
      { sch_pos   = fld.pos;
        sch_targs = [];
        sch_named = [];
        sch_body  = make TWildcard
      }
    in
    Either.Right (make (n, sch))
  | FldNameVal(n, tp) ->
    Either.Right (make (n, tr_scheme_expr tp))
  | FldNameAnnot _ ->
    assert false

(** Translate a type expression as a type parameter *)
and tr_type_arg (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TParen tp -> make (tr_type_arg tp).data
  | TVar x    -> make (TA_Var x)
  | TWildcard | TArrow _ | TEffect _ | TApp _ | TRecord _ | TTypeLbl _
  | TEffectLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a type expression as a named type parameter *)
let rec tr_named_type_arg (tp : Raw.type_expr) =
  let make data = { tp with data = data } in
  match tp.data with
  | TParen tp   -> make (tr_named_type_arg tp).data
  | TVar   x    -> make (TNVar x, make (TA_Var x))
  | TTypeLbl tp -> make (TNAnon, tr_type_arg tp)
  | TEffectLbl tp -> make (TNEffect, tr_type_arg tp)
  | TWildcard | TArrow _ | TEffect _ | TApp _ | TRecord _ ->
    Error.fatal (Error.desugar_error tp.pos)

(** Translate a left-hand-side of the type definition. The additional
  parameter is an accumulated list of formal parameters *)
let rec tr_type_def (tp : Raw.type_expr) args =
  match tp.data with
  | TVar x -> TD_Id(x, args)
  | TApp(tp1, tp2) -> tr_type_def tp1 (tp2 :: args)
  | TWildcard | TParen _ | TArrow _ | TEffect _ | TRecord _ | TTypeLbl _
  | TEffectLbl _ ->
    Error.fatal (Error.desugar_error tp.pos)

let tr_ctor_decl (d : Raw.ctor_decl) =
  let make data = { d with data = data } in
  match d.data with
  | CtorDecl(name, { data = TRecord flds; _ } :: schs ) ->
    let (tvs, implicit) = map_inst_like tr_scheme_field flds in
    let schs = List.map tr_scheme_expr schs in
    make (CtorDecl(name, tvs, implicit, schs))
  | CtorDecl(name, schs) ->
    make (CtorDecl(name, [], [], List.map tr_scheme_expr schs))

let tr_data_def (dd : Raw.data_def) =
  let make data = { dd with data = data } in
  match dd.data with
  | DD_Data(tp, cs) ->
    begin match tr_type_def tp [] with
    | TD_Id(x, args) ->
      make (DD_Data(x, List.map tr_named_type_arg args,
                       List.map tr_ctor_decl cs))
    end

(** Translate a simple pattern, i.e., pattern that cannot be applied to
  the list of parameters. This function makes sure, that the provided list
  of parameters is empty *)
let rec tr_simple_pattern (p : Raw.expr) ps =
  let make data = { p with data = data } in
  match ps, p.data with
  | [], EWildcard -> make PWildcard
  | [], EUnit     -> Error.fatal (Error.desugar_error p.pos)
  | [], EParen p  -> make (tr_pattern p []).data
  | [], EVar  x   -> make (PId (IdVar x))
  | [], EImplicit n -> make (PId (IdImplicit n))
  | [], EAnnot(p, sch) -> make (PAnnot(tr_pattern p [], tr_scheme_expr sch))
  | [], (ECtor _ | EFn _ | EApp _ | EEffect _ | EDefs _ | EMatch _
    | EHandler _ | ERecord _) ->
    assert false
  | p1 :: _, _ ->
    Error.fatal (Error.invalid_pattern_arg p1.pos)

(** Translate a pattern. Argument [ps] is an accumulated list of subpatterns *)
and tr_pattern (p : Raw.expr) ps =
  let make data = { p with data = data } in
  match p.data with
  | EWildcard | EUnit | EParen _ | EVar _ | EImplicit _ | EAnnot _ ->
    tr_simple_pattern p ps
  | ECtor c ->
    begin match ps with
    | { data = Raw.ERecord ips; _ } :: ps ->
      let (targs, ips) = map_inst_like tr_named_pattern ips in
      make (PCtor(make c, targs, ips, List.map (fun p -> tr_pattern p []) ps))
    | _ ->
      make (PCtor(make c, [], [], List.map (fun p -> tr_pattern p []) ps))
    end
  | EApp(p, p1) -> tr_pattern p (p1 :: ps)

  | EFn _ | EEffect _ | EDefs _ | EMatch _ | EHandler _ | ERecord _ ->
    Error.fatal (Error.desugar_error p.pos)

and tr_named_pattern (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType _ ->
    Error.fatal (Error.anon_type_pattern fld.pos)
  | FldEffect ->
    Either.Left (make (TNEffect, make TA_Effect))
  | FldEffectVal arg ->
    Either.Left (make (TNEffect, tr_type_arg arg))
  | FldType x ->
    Either.Left (make (TNVar x, make (TA_Var x)))
  | FldTypeVal(x, arg) ->
    Either.Left (make (TNVar x, tr_type_arg arg))
  | FldName n ->
    Either.Right (make (n, make (PId (ident_of_name n))))
  | FldNameVal(n, p) ->
    Either.Right (make (n, tr_pattern p []))
  | FldNameAnnot(n, sch) ->
    Either.Right
      (make (n, make (PAnnot(make (PId (ident_of_name n)),
                             tr_scheme_expr sch))))

(** Translate a formal parameter of a function *)
let rec tr_function_arg (arg : Raw.expr) =
  match arg.data with
  | EParen arg -> tr_function_arg arg
  | EAnnot(p, sch) ->
    ArgAnnot(tr_pattern p [], tr_scheme_expr sch)
  | EWildcard | EUnit | EVar _ | EImplicit _ | ECtor _ | EApp _ ->
    ArgPattern (tr_pattern arg [])

  | EFn _ | EEffect _ | EDefs _ | EMatch _ | EHandler _ | ERecord _ ->
    Error.fatal (Error.desugar_error arg.pos)

let tr_named_arg (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType arg ->
    Either.Left (make (TNAnon, tr_type_arg arg))
  | FldEffect ->
    Either.Left (make (TNEffect, make TA_Effect))
  | FldEffectVal arg ->
    Either.Left (make (TNEffect, tr_type_arg arg))
  | FldType x ->
    Either.Left (make (TNVar x, make (TA_Var x)))
  | FldTypeVal(x, arg) ->
    Either.Left (make (TNVar x, tr_type_arg arg))
  | FldName n ->
    Either.Right (make (n, ArgPattern(make (PId (ident_of_name n)))))
  | FldNameVal(n, e) ->
    Either.Right (make (n, tr_function_arg e))
  | FldNameAnnot(n, sch) ->
    Either.Right
      (make (n, ArgAnnot(make (PId (ident_of_name n)), tr_scheme_expr sch)))

(** Translate an expression as a let-pattern. Argument [ps] is an accumulated
  list of formal parameters/subpatterns *)
let rec tr_let_pattern (p : Raw.expr) ps =
  match p.data with
  | EWildcard | EUnit | EParen _ | ECtor _ | EAnnot _ ->
    LP_Pat (tr_pattern p ps)
  | EVar _ | EImplicit _ ->
    let id =
      match p.data with
      | EVar      x -> IdVar      x
      | EImplicit n -> IdImplicit n
      | _ -> assert false
    in
    begin match ps with
    | { data = ERecord iargs; _ } :: ps ->
      let (targs, iargs) = map_inst_like tr_named_arg iargs in
      LP_Fun(id, targs, iargs, ps)
    | _ ->
      LP_Id(id, ps)
    end
  | EApp(p, p1) -> tr_let_pattern p (p1 :: ps)

  | EFn _ | EEffect _ | EDefs _ | EMatch _ | EHandler _ | ERecord _ ->
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
  | EVar      x -> make (EVar      x)
  | EImplicit n -> make (EImplicit n)
  | ECtor     c -> make (ECtor     c)

  | EWildcard | EUnit | EParen _ | EFn _ | EApp _ | EEffect _ | EDefs _
  | EMatch _ | ERecord _ | EHandler _ | EAnnot _ ->
    Error.fatal (Error.desugar_error e.pos)

let rec tr_expr (e : Raw.expr) =
  let make data = { e with data = data } in
  match e.data with
  | EUnit          -> make EUnit
  | EParen e       -> make (tr_expr e).data
  | EVar _ | EImplicit _ | ECtor _ -> make (EPoly(tr_poly_expr e, [], []))
  | EFn(es, e)     -> make (tr_function es (tr_expr e)).data
  | EApp(e, { data = ERecord flds; _ }) ->
    let (tinst, inst) = map_inst_like tr_explicit_inst flds in
    make (EPoly(tr_poly_expr e, tinst, inst))
  | EApp(e1, e2)   -> make (EApp(tr_expr e1, tr_expr e2))
  | EDefs(defs, e) -> make (EDefs(tr_defs defs, tr_expr e))
  | EMatch(e, cls) -> make (EMatch(tr_expr e, List.map tr_match_clause cls))
  | EHandler h     -> make (EHandler (tr_expr h))
  | EEffect(es, rp, e) ->
    make (tr_function es
      { pos  = Position.join rp.pos e.pos;
        data = EEffect(tr_function_arg rp, tr_expr e)}).data
  | EWildcard | ERecord _ | EAnnot _ ->
    Error.fatal (Error.desugar_error e.pos)

and tr_match_clause (cl : Raw.match_clause) =
  let make data = { cl with data = data } in
  match cl.data with
  | Clause(pat, body) ->
    make (Clause(tr_pattern pat [], tr_expr body))

and tr_explicit_inst (fld : Raw.field) =
  let make data = { fld with data = data } in
  match fld.data with
  | FldAnonType _ ->
    Error.fatal (Error.desugar_error fld.pos)
  | FldEffectVal eff ->
    Either.Left (make (TNEffect, tr_type_expr eff))
  | FldType x ->
    Either.Left (make (TNVar x, make (TVar x)))
  | FldTypeVal(x, tp) ->
    Either.Left (make (TNVar x, tr_type_expr tp))
  | FldName n ->
    let pe =
      match n with
      | NVar      x -> make (EVar x)
      | NImplicit n -> make (EImplicit n)
    in
    Either.Right (make (n, make (EPoly(pe, [], []))))
  | FldNameVal(n, e) ->
    Either.Right (make (n, tr_expr e))
  | FldEffect | FldNameAnnot _ ->
    Error.fatal (Error.desugar_error fld.pos)

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
  | DData    dd  -> make (DData (tr_data_def dd))
  | DDataRec dds -> make (DDataRec (List.map tr_data_def dds))
  | DHandle(pat, h) ->
    make (DHandlePat(tr_pattern pat [], { h with data = EHandler(tr_expr h) }))
  | DHandleWith(pat, e) ->
    make (DHandlePat(tr_pattern pat [], tr_expr e))

and tr_defs defs = List.map tr_def defs

let tr_program (p : Raw.program) =
  let make data = { p with data = data } in
  make (EDefs(tr_defs p.data, make EUnit))
