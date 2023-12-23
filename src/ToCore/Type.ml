(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

let eff_cons env x eff =
  let (Ex x) = Env.lookup_tvar env x in
  match T.TVar.kind x with
  | KEffect -> T.Effect.join (TVar x) eff
  | KType ->
    failwith "Internal kind error"

let tr_effect_end env (eff : S.Type.effect_end) =
  match eff with
  | EEClosed -> T.TEffPure
  | EEVar x ->
    let (Ex x) = Env.lookup_tvar env x in
    begin match T.TVar.kind x with
    | KEffect -> TVar x
    | KType   -> failwith "Internal kind error"
    end
  | EEUVar _  ->
    (* TODO: they can be supported, and its especially useful in REPL *)
    InterpLib.Error.incr_error_counter ();
    Printf.eprintf "error: Unsolved unification variables left\n";
    raise InterpLib.Error.Fatal_error

(** Translate type *)
let rec tr_type env tp =
  match S.Type.view tp with
  | TUnit    -> T.Type.Ex TUnit
  | TUVar _  ->
    (* TODO: they can be supported, and its especially useful in REPL *)
    InterpLib.Error.incr_error_counter ();
    Printf.eprintf "error: Unsolved unification variables left\n";
    raise InterpLib.Error.Fatal_error
  | TVar x  ->
    let (Ex x) = Env.lookup_tvar env x in
    (* We add nterm effect to all effects in the program *)
    begin match T.TVar.kind x with
    | KEffect -> T.Type.Ex (T.Effect.join (TVar x) T.Effect.nterm)
    | KType   -> T.Type.Ex (TVar x)
    end
  | TEffect(xs, ee) ->
    (* We add nterm effect to all effects in the program *)
    let eff = S.TVar.Set.fold (eff_cons env) xs (tr_effect_end env ee) in
    T.Type.Ex (T.Effect.join eff T.Effect.nterm)
  | TPureArrow(tp1, tp2) ->
    T.Type.Ex (TArrow(tr_ttype env tp1, tr_ttype env tp2, TEffPure))
  | TArrow(tp1, tp2, eff) ->
    T.Type.Ex (TArrow(tr_ttype env tp1, tr_ttype env tp2, tr_effect env eff))

(** Translate type of kind type *)
and tr_ttype env tp : T.ttype =
  let (Ex tp) = tr_type env tp in
  match T.Type.kind tp with
  | KType -> tp
  | KEffect ->
    failwith "Internal kind error"

(** Translate an effect (a type of effect kind) *)
and tr_effect env eff : T.effect =
  let (Ex eff) = tr_type env eff in
  match T.Type.kind eff with
  | KEffect -> eff
  | KType ->
    failwith "Internal kind error"

(** Translate a constructor declaration *)
let tr_ctor_decl env (ctor : S.ctor_decl) =
  { T.ctor_name      = ctor.ctor_name;
    T.ctor_arg_types = List.map (tr_ttype env) ctor.ctor_arg_types
  }

(** Translate a list of constructor declarations *)
let tr_ctor_decls env decls =
  List.map (tr_ctor_decl env) decls
