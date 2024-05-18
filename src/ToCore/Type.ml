(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types *)

open Common

let eff_cons env x eff =
  let (Ex x) = Env.lookup_tvar env x in
  match T.TVar.kind x with
  | KEffect -> T.Effect.join (TVar x) eff
  | KType | KArrow _ ->
    failwith "Internal kind error"

let pack_type (type k) (k : S.kind) (tp : k T.typ) : T.Type.ex =
  match S.Kind.view k with
  | KEffrow ->
    (* We add nterm effect to all effects rows in the program *)
    begin match T.Type.kind tp with
    | KEffect ->
      T.Type.Ex (T.Effect.join tp T.Effect.nterm)
    | _ -> assert false
    end
  | KUVar u ->
    let b = S.Kind.set_non_effect k in
    assert b;
    T.Type.Ex tp
  | _ ->
    T.Type.Ex tp

let rec tr_effrow_end env (eff : S.Type.effrow_end) =
  match eff with
  | EEClosed -> T.TEffPure
  | EEUVar(_,uvar) when Env.in_repl_mode env ->
    let uid = Lang.Unif.UVar.uid uvar in
    TUVar(uid, KEffect)
  | EEUVar _  ->
    InterpLib.Error.report ~cls:FatalError
      "Unsolved unification variables left.";
    raise InterpLib.Error.Fatal_error
  | EEVar x ->
    let (Ex x) = Env.lookup_tvar env x in
    begin match T.TVar.kind x with
    | KEffect -> TVar x
    | KType | KArrow _ -> failwith "Internal kind error"
    end
  | EEApp(tp1, tp2) -> tr_effect env (S.Type.t_app tp1 tp2)

(** Translate type *)
and tr_type env tp =
  match S.Type.view tp with
  | TUVar(_,uvar) when Env.in_repl_mode env ->
    let uid = Lang.Unif.UVar.uid uvar in
    let (T.Kind.Ex k) = S.Type.kind tp |> tr_kind in
    pack_type (S.Type.kind tp) (TUVar (uid, k))
  | TUVar _  ->
    InterpLib.Error.report ~cls:FatalError
      "Unsolved unification variables left.";
    raise InterpLib.Error.Fatal_error
  | TVar x  ->
    let (Ex x) = Env.lookup_tvar env x in
    pack_type (S.Type.kind tp) (TVar x)
  | TEffect xs ->
    T.Type.Ex (S.TVar.Set.fold (eff_cons env) xs T.TEffPure)
  | TEffrow(xs, ee) ->
    (* We add nterm effect to all effects in the program *)
    let eff = S.TVar.Set.fold (eff_cons env) xs (tr_effrow_end env ee) in
    T.Type.Ex (T.Effect.join eff T.Effect.nterm)
  | TPureArrow(sch, tp2) ->
    T.Type.Ex (TArrow(tr_scheme env sch, tr_ttype env tp2, TEffPure))
  | TArrow(sch, tp2, eff) ->
    T.Type.Ex (TArrow(tr_scheme env sch, tr_ttype env tp2, tr_effect env eff))
  | THandler(x, tp, tp0, eff0) ->
    let (env, Ex x) = Env.add_tvar env x in
    begin match T.TVar.kind x with
    | KEffect ->
      let tp   = tr_ttype env tp in
      let tp0  = tr_ttype env tp0 in
      let eff0 = tr_effect env eff0 in
      T.Type.Ex (TForall(x,
        TArrow(TLabel
          { effect    = TVar x;
            tvars     = [];
            val_types = [];
            delim_tp  = tp0;
            delim_eff = eff0
          }, tp, TEffPure)))
    | _ ->
      failwith "Internal kind error"
    end
  | TLabel(eff, tp0, eff0) ->
    let eff  = tr_effect env eff in
    let tp0  = tr_ttype  env tp0 in
    let eff0 = tr_effect env eff0 in
    T.Type.Ex (TLabel
      { effect    = eff;
        tvars     = [];
        val_types = [];
        delim_tp  = tp0;
        delim_eff = eff0
      })
  | TApp(tp1, tp2) ->
    let (Ex tp1) = tr_type env tp1 in
    let (Ex tp2) = tr_type env tp2 in
    begin match T.Type.kind tp1 with
    | KArrow(k1, _) ->
      begin match T.Kind.equal k1 (T.Type.kind tp2) with
      | Equal -> pack_type (S.Type.kind tp) (TApp(tp1, tp2))
      | NotEqual ->
        failwith "Internal kind error"
      end
    | KType | KEffect ->
      failwith "Internal kind error"
    end

(** Translate type of kind type *)
and tr_ttype env tp : T.ttype =
  let (Ex tp) = tr_type env tp in
  match T.Type.kind tp with
  | KType -> tp
  | KEffect | KArrow _ ->
    failwith "Internal kind error"

(** Translate an effect (a type of effect kind) *)
and tr_effect env eff : T.effect =
  let (Ex eff) = tr_type env eff in
  match T.Type.kind eff with
  | KEffect -> eff
  | KType | KArrow _ ->
    failwith "Internal kind error"

(** Translate a type scheme *)
and tr_scheme env (sch : S.scheme) =
  let { S.sch_targs; sch_named; sch_body } = sch in
  let (env, tvars) = List.fold_left_map Env.add_named_tvar env sch_targs in
  let itps = List.map (fun (_, tp) -> tr_scheme env tp) sch_named in
  T.Type.t_foralls tvars
    (T.Type.t_pure_arrows itps
      (tr_ttype env sch_body))
