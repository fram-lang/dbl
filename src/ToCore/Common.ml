(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common definitions of the translation *)

(* Author: Piotr Polesiuk, 2023 *)

module S = Lang.Unif
module T = Lang.Core

(** Translate kind *)
let rec tr_kind k =
  match S.Kind.view k with
  | KType   -> T.Kind.Ex KType
  | KEffrow -> T.Kind.Ex KEffect
