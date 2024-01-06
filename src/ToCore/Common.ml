(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common definitions of the translation *)

(* Author: Piotr Polesiuk, 2023,2024 *)

module S = Lang.Unif
module T = Lang.Core

(** Translate kind *)
let rec tr_kind k =
  match S.Kind.view k with
  | KType     -> T.Kind.Ex KType
  | KEffect   -> T.Kind.Ex KEffect
  | KClEffect -> T.Kind.Ex KEffect
  | KUVar u   ->
    S.KUVar.set u S.Kind.k_type;
    T.Kind.Ex KType
  | KArrow(k1, k2) ->
    let (Ex k1) = tr_kind k1 in
    let (Ex k2) = tr_kind k2 in
    T.Kind.Ex (KArrow(k1, k2))
