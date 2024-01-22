(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types *)

(* Author: Piotr Polesiuk, 2023,2024 *)

open Common

let eff_cons env x eff =
  let (Ex x) = Env.lookup_tvar env x in
  match T.TVar.kind x with
  | KEffect -> T.Effect.join (TVar x) eff
  | KType | KArrow _ ->
    failwith "Internal kind error"

let pack_type (type k) (tp : k T.typ) : T.Type.ex =
  match T.Type.kind tp with
  | KEffect ->
    (* We add nterm effect to all effects in the program *)
    T.Type.Ex (T.Effect.join tp T.Effect.nterm)
  | _ ->
    T.Type.Ex tp

let rec tr_effect_end env (eff : S.Type.effect_end) =
  match eff with
  | EEClosed -> T.TEffPure
  | EEUVar _  ->
    (* TODO: they can be supported, and its especially useful in REPL *)
    InterpLib.Error.incr_error_counter ();
    Printf.eprintf "error: Unsolved unification variables left\n";
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
  | TUnit    -> T.Type.Ex TUnit
  | TUVar _  ->
    (* TODO: they can be supported, and its especially useful in REPL *)
    InterpLib.Error.incr_error_counter ();
    Printf.eprintf "error: Unsolved unification variables left\n";
    raise InterpLib.Error.Fatal_error
  | TVar x  ->
    let (Ex x) = Env.lookup_tvar env x in
    pack_type (TVar x)
  | TEffect(xs, ee) ->
    (* We add nterm effect to all effects in the program *)
    let eff = S.TVar.Set.fold (eff_cons env) xs (tr_effect_end env ee) in
    T.Type.Ex (T.Effect.join eff T.Effect.nterm)
  | TPureArrow(sch, tp2) ->
    T.Type.Ex (TArrow(tr_scheme env sch, tr_ttype env tp2, TEffPure))
  | TArrow(sch, tp2, eff) ->
    T.Type.Ex (TArrow(tr_scheme env sch, tr_ttype env tp2, tr_effect env eff))
  | TApp(tp1, tp2) ->
    let (Ex tp1) = tr_type env tp1 in
    let (Ex tp2) = tr_type env tp2 in
    begin match T.Type.kind tp1 with
    | KArrow(k1, _) ->
      begin match T.Kind.equal k1 (T.Type.kind tp2) with
      | Equal -> pack_type (TApp(tp1, tp2))
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
