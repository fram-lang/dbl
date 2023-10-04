(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of types *)

(* Author: Piotr Polesiuk, 2023 *)

open Common

(** Translate type *)
let rec tr_type env tp =
  match S.Type.view tp with
  | TUnit    -> T.Type.Ex TUnit
  | TRowPure -> T.Type.Ex TEffPure
  | TUVar _  ->
    (* TODO: they can be supported, and its especially useful in REPL *)
    InterpLib.Error.incr_error_counter ();
    Printf.eprintf "error: Unsolved unification variables left\n";
    raise InterpLib.Error.Fatal_error
  | TVar x  ->
    let (Ex x) = Env.lookup_tvar env x in
    T.Type.Ex (TVar x)
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

(** Translate effect *)
and tr_effect env eff : T.effect =
  let (Ex eff) = tr_type env eff in
  match T.Type.kind eff with
  | KEffect -> eff
  | KType ->
    failwith "Internal kind error"
