(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Building counterexample of non-matched value in pattern-matching *)

(* Author: Piotr Polesiuk, 2023 *)

(** Example of not-matched pattern *)
type ex_pattern =
  | ExHole
    (** Hole for the matched value *)

  | ExWildcard
    (** Wild-card: any value *)

  | ExCtor of string * ex_pattern list
    (** Constructor pattern *)

(** Zipper context of a not-matched counterexample *)
type ctx =
  | CtxRoot
    (** Root of the tree *)

  | CtxDone of ex_pattern
    (** Pattern without holes *)

  | CtxCtor of string * ex_pattern list * ctx * ex_pattern list
    (** ADT constructor contexts. The left list of example patterns is
      in reversed order *)

(** Plug given example pattern into the context, and focus on the next hole *)
let rec refocus_with ctx ex =
  match ctx with
  | CtxRoot   -> CtxDone ex
  | CtxDone _ -> assert false
  | CtxCtor(name, left, ctx, right) ->
    begin match try_focus_ctor ctx name (ex :: left) right with
    | None ->
      refocus_with ctx (ExCtor(name, List.rev_append left (ex :: right)))
    | Some ctx -> ctx
    end

(** Try to focus on a hole within given example pattern *)
and try_focus ctx ex =
  match ex with
  | ExHole     -> Some ctx
  | ExWildcard -> None
  | ExCtor(name, exs) ->
    try_focus_ctor ctx name [] exs

and try_focus_ctor ctx name left right =
  match right with
  | [] -> None
  | ex :: right ->
    begin match try_focus (CtxCtor(name, left, ctx, right)) ex with
    | Some ctx -> Some ctx
    | None     -> try_focus_ctor ctx name (ex :: left) right
    end

(** Refocus given context on the next hole *)
let refocus ctx =
  refocus_with ctx ExWildcard

(** Plug given example pattern into the context, and focus on the next hole,
  possibly in the plugged pattern *)
let focus_with ctx ex =
  match try_focus ctx ex with
  | Some ctx -> ctx
  | None     -> refocus_with ctx ex
