(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Syntax of the Core Language. *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

type var = Var.t

type expr =
  | EValue   of value
  | ELet     of var * expr * expr
  | ELetPure of var * expr * expr
  | EApp     of value * value
  | ETApp     : value * 'k typ -> expr

and value =
  | VUnit
  | VVar of var
  | VFn  of var * ttype * expr
  | VTFun : 'k tvar * expr -> value

type program = expr
