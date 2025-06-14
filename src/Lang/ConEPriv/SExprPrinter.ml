(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translating ConE to S-expressions *)

open SExpr
open TypeBase
open Syntax

let tr_kind = UnifCommon.Kind.to_sexpr

let tr_tvar = TVar.to_sexpr

let tr_tvar_binder x =
  List [ tr_tvar x; tr_kind (UnifCommon.TVar.kind x) ]

let tr_named_tvar_binder (_, x) =
  tr_tvar_binder x

let rec tr_type tp =
  match view tp with
  | TVar x -> tr_tvar x
  | TArrow _ -> List (tr_arrow tp)
  | TLabel(eff, delim_tp, delim_eff) ->
    List [Sym "label";
      Effct.to_sexpr eff;
      tr_type delim_tp;
      Effct.to_sexpr delim_eff]
  | THandler h ->
    List [Sym "handler";
      tr_tvar h.tvar;
      tr_type h.cap_tp;
      tr_type h.in_tp;
      Effct.to_sexpr h.in_eff;
      tr_type h.out_tp;
      Effct.to_sexpr h.out_eff]
  | TEffect eff ->
    List [Sym "effect"; Effct.to_sexpr eff]
  | TApp _ -> tr_type_app tp []
  | TAlias (PP_UID uid, tp) ->
    List [Sym "alias"; Sym (UID.to_string uid); tr_type tp]

and tr_arrow tp =
  match view tp with
  | TArrow(sch, tp2, Pure) -> tr_scheme sch :: tr_arrow tp2
  | TArrow(sch, tp2, eff) ->
    [ tr_scheme sch; Sym "->"; tr_type tp2; CEffect.to_sexpr eff]
  | _ -> [ Sym "->"; tr_type tp ]

and tr_type_app tp args =
  match view tp with
  | TApp(tp1, tp2) -> tr_type_app tp1 (tr_type tp2 :: args)

  | TVar _ | TArrow _ | TLabel _ | THandler _ | TEffect _ | TAlias _ ->
    List (Sym "app" :: tr_type tp :: args)

and tr_scheme sch =
  List [
    Sym "forall";
    List (List.map tr_named_tvar_binder sch.sch_targs);
    List (List.map tr_named_scheme sch.sch_named);
    tr_type sch.sch_body ]

and tr_named_scheme (_, sch) =
  tr_scheme sch

let tr_constr (eff1, eff2) =
  List [ Effct.to_sexpr eff1; Sym "<:"; Effct.to_sexpr eff2 ]

(* ========================================================================= *)

let tr_var x = Sym (Var.unique_name x)

let tr_ctor_decl (ctor : ctor_decl) =
  List (
    Sym ctor.ctor_name ::
    List (List.map tr_named_tvar_binder ctor.ctor_targs) ::
    List (List.map tr_named_scheme ctor.ctor_named) ::
    List.map tr_scheme ctor.ctor_arg_schemes)

let tr_data_def (dd : data_def) =
  match dd with
  | DD_Data adt ->
    List (
      (if adt.positive then Sym "positive-data" else Sym "data") ::
      tr_tvar adt.tvar ::
      tr_var  adt.proof ::
      List (List.map tr_named_tvar_binder adt.args) ::
      List.map tr_ctor_decl adt.ctors)

  | DD_Label lbl ->
    List [
      Sym "label";
      tr_tvar lbl.tvar;
      tr_var lbl.var;
      tr_type lbl.delim_tp;
      Effct.to_sexpr lbl.delim_eff
    ]

(* ========================================================================= *)

let rec tr_expr (e : expr) =
  match e with
  | EUnitPrf   -> Sym "unit-prf"
  | EBoolPrf   -> Sym "bool-prf"
  | EOptionPrf -> Sym "option-prf"
  | ENum n     -> Sym (string_of_int n)
  | ENum64 n   -> Sym (Int64.to_string n ^ "L")
  | EStr s     -> Sym (Printf.sprintf "\"%s\"" (String.escaped s))
  | EChr c     -> Sym (Printf.sprintf "\'%s\'" (Char.escaped c))
  | EVar x     -> tr_var x
  | EFn _      -> List (Sym "fn" :: tr_fn e)
  | ETFun _    -> List (Sym "tfun" :: tr_tfun e)
  | ECAbs(cs, e) ->
    List [ Sym "cabs"; List (List.map tr_constr cs); tr_expr e ]
  | EApp _ | ETApp _ | ECApp _ -> tr_app e []
  | ELet _ | ELetPure _ | ELetRec _ | ERecCtx _ | EData _ | EReset _ ->
    List (Sym "defs" :: tr_defs e)
  | ECtor(prf, idx, tps, es) ->
    List (
      Sym "ctor" ::
      Sym (string_of_int idx) ::
      List (List.map tr_type tps) ::
      List.map tr_expr es)
  | EMatch(proof, e, cls, tp, eff) ->
    List [ Sym "match"; tr_expr proof; tr_expr e;
      List (Sym "clauses" :: List.map tr_clause cls);
      tr_type tp; CEffect.to_sexpr eff ]
  | EShift(lbl, k, body, tp) ->
    List
      [ Sym "shift";
        tr_expr lbl;
        tr_var k;
        tr_type tp;
        tr_expr body ]
  | EExtern(name, tp) ->
    List [ Sym "extern"; Sym name; tr_type tp ]
  | ERepl(_, tp, eff) ->
    List [ Sym "repl"; tr_type tp; CEffect.to_sexpr eff ]
  | EReplExpr(e1, tp, e2, _) ->
    List [ Sym "repl-expr"; tr_expr e1; Sym ("{" ^ tp ^ "}") ; tr_expr e2 ]

and tr_fn e =
  match e with
  | EFn(x, sch, e) ->
    List [ tr_var x; tr_scheme sch ] :: tr_fn e
  | _ -> [ tr_expr e ]

and tr_tfun e =
  match e with
  | ETFun(x, e) -> tr_tvar_binder x :: tr_tfun e
  | _ -> [ tr_expr e ]

and tr_app e args =
  match e with
  | EApp(e1, e2) -> tr_app e1 (tr_expr e2 :: args)
  | ETApp(e1, tp) -> tr_app e1 (List [ Sym "type"; tr_type tp ] :: args)
  | ECApp e1 -> tr_app e1 (Sym "constr" :: args)

  | EUnitPrf | EBoolPrf | EOptionPrf | ENum _ | ENum64 _ | EStr _ | EChr _ 
  | EVar _ | EFn _ | ETFun _ | ECAbs _ | ELet _ | ELetPure _ | ELetRec _ 
  | ERecCtx _ | EData _ | ECtor _ | EMatch _ | EShift _ | EReset _ | EExtern _
  | ERepl _ | EReplExpr _ ->
    List (tr_expr e :: args)

and tr_defs e =
  match e with
  | ELet(x, e1, e2) ->
    List [ Sym "let"; tr_var x; tr_expr e1 ] :: tr_defs e2
  | ELetPure(x, e1, e2) ->
    List [ Sym "let-pure"; tr_var x; tr_expr e1 ] :: tr_defs e2
  | ELetRec(rds, e2) ->
    List (Sym "let-rec" :: List.map tr_rec_def rds) :: tr_defs e2
  | ERecCtx e2 -> List [ Sym "rec-ctx" ] :: tr_defs e2
  | EData(dds, e2) ->
    List (Sym "data" :: List.map tr_data_def dds) :: tr_defs e2
  | EReset(lbl, body, x, ret) ->
    List
      [ Sym "reset";
        tr_expr lbl;
        tr_var x; tr_expr ret
      ] :: tr_defs body

  | EUnitPrf | EBoolPrf | EOptionPrf | ENum _ | ENum64 _ | EStr _ | EChr _ 
  | EVar _ | EFn _ | ETFun _ | ECAbs _ | EApp _ | ETApp _ | ECApp _ | ECtor _
  | EMatch _ | EShift _ | EExtern _ | ERepl _ | EReplExpr _ ->
    [ tr_expr e ]

and tr_rec_def rd =
  List [
    tr_var rd.rd_var;
    List (Sym "evars"   :: List.map tr_tvar rd.rd_evars);
    List (Sym "constrs" :: List.map tr_constr rd.rd_constr);
    tr_scheme rd.rd_scheme;
    tr_expr   rd.rd_body
  ]

and tr_clause (c : Syntax.match_clause) =
  List (
    List (Sym "tvars" :: List.map tr_tvar_binder c.cl_tvars) ::
    List (Sym "vars" :: List.map tr_var c.cl_vars) ::
    tr_defs c.cl_body)

(* ========================================================================= *)

let tr_program = tr_expr
