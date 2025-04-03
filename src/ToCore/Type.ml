(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types *)

open Common

(** Translate effect *)
let tr_effect env eff =
  let (tvars, gvars) = S.Effct.view eff in
  List.iter (fun (gv, _) -> S.GVar.set gv S.Effct.pure) gvars;
  List.fold_left
    (fun eff (x, p) ->
      if IncrSAT.Formula.fix p then 
        let (Ex x) = Env.lookup_tvar env x in
        match T.TVar.kind x with
        | KEffect -> T.Effect.join (TVar x) eff
        | KType | KArrow _ ->
          failwith "Internal kind error"
      else eff)
    T.TEffPure
    tvars

let tr_ceffect env (eff : S.ceffect) =
  match eff with
  | Pure       -> T.TEffPure
  | Impure eff -> T.Effect.join (tr_effect env eff) T.Effect.nterm

let rec tr_type env tp =
  match S.Type.view tp with
  | TVar x ->
    let (Ex x) = Env.lookup_tvar env x in
    T.Type.Ex (T.TVar x)

  | TArrow(sch, tp, eff) ->
    T.Type.Ex (
      T.TArrow(tr_scheme env sch, tr_ttype env tp, tr_ceffect env eff))

  | TLabel(eff, delim_tp, delim_eff) ->
    T.Type.Ex
      (T.TLabel
        { effct     = tr_effect env eff;
          tvars     = [];
          val_types = [];
          delim_tp  = tr_ttype env delim_tp;
          delim_eff = tr_ceffect env (Impure delim_eff)
        })

  | THandler h ->
    let out_tp  = tr_ttype env h.out_tp in
    let out_eff = tr_ceffect env (Impure h.out_eff) in
    let (env, Ex x) = Env.add_tvar env h.tvar in
    begin match T.TVar.kind x with
    | KEffect ->
      let cap_tp = tr_ttype env h.cap_tp in
      let in_tp  = tr_ttype env h.in_tp in
      let in_eff =
        T.Effect.join (TVar x) (tr_ceffect env (Impure h.in_eff)) in
      T.Type.Ex (T.TArrow(
        TForall(x, TArrow(cap_tp, in_tp, in_eff)),
        out_tp, out_eff))
    | KType | KArrow _ ->
      failwith "Internal kind error"
    end

  | TApp(tp1, tp2) ->
    let (Ex tp1) = tr_type env tp1 in
    let (Ex tp2) = tr_type env tp2 in
    begin match T.Type.kind tp1 with
    | KArrow(k1, _) ->
      begin match T.Kind.equal k1 (T.Type.kind tp2) with
      | Equal -> T.Type.Ex (T.TApp(tp1, tp2))
      | NotEqual ->
        failwith "Internal kind error"
      end
    | KType | KEffect ->
      failwith "Internal kind error"
    end

  | TEffect eff ->
    T.Type.Ex (tr_effect env eff)

and tr_ttype env tp : T.ttype =
  let (Ex tp) = tr_type env tp in
  match T.Type.kind tp with
  | KType -> tp
  | KEffect | KArrow _ ->
    failwith "Internal kind error"

(** Translate a type scheme *)
and tr_scheme env (sch : S.scheme) =
  let S.{ sch_targs; sch_named; sch_body } = sch in
  let (env, tvars) = Env.add_named_tvars env sch_targs in
  let tps = List.map (fun (_, sch) -> tr_scheme env sch) sch_named in
  T.Type.t_foralls tvars
    (T.Type.t_pure_arrows tps
      (tr_ttype env sch_body))

let tr_constr env (eff1, eff2) =
  (tr_effect env eff1, tr_effect env eff2)
