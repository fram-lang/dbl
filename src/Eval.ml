(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Evaluator *)

(* Author: Piotr Polesiuk, 2023 *)

(** Value of the evaluator *)
type value =
  | VUnit
    (** Unit value *)

  | VFn of (value -> value comp)
    (** Function *)

(** CPS Computations *)
and 'v comp = ('v -> unit) -> unit

module Env : sig
  type t

  val empty : t

  val extend : t -> Var.t -> value -> t

  val lookup : t -> Var.t -> value
end = struct
  type t = value Var.Map.t

  let empty = Var.Map.empty

  let extend env x v =
    Var.Map.add x v env

  let lookup env x =
    Var.Map.find x env
end

let rec eval_expr env (e : Lang.Untyped.expr) cont =
  match e with
  | EValue v -> cont (eval_value env v)
  | ELet(x, e1, e2) ->
    eval_expr env e1 (fun v ->
    eval_expr (Env.extend env x v) e2 cont)
  | EApp(v1, v2) ->
    begin match eval_value env v1 with
    | VFn f -> f (eval_value env v2) cont
    | _ -> failwith "Runtime error!"
    end

and eval_value env (v : Lang.Untyped.value) =
  match v with
  | VUnit  -> VUnit
  | VVar x -> Env.lookup env x
  | VFn(x, body) ->
    VFn(fun v -> eval_expr (Env.extend env x v) body)

let eval_program p =
  eval_expr Env.empty p ignore
