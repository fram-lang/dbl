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

let to_string (v : value) =
  match v with
  | VUnit -> "()"
  | VFn _ -> "<fun>"

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
  | ERepl func -> eval_repl env func cont
  | EReplExpr(e1, tp, e2) ->
    Printf.printf ": %s\n" tp;
    eval_expr env e1 (fun v1 ->
      Printf.printf "= %s\n" (to_string v1);
      eval_expr env e2 cont)

and eval_value env (v : Lang.Untyped.value) =
  match v with
  | VUnit  -> VUnit
  | VVar x -> Env.lookup env x
  | VFn(x, body) ->
    VFn(fun v -> eval_expr (Env.extend env x v) body)

and eval_repl env func cont =
  match func () with
  | e -> eval_expr env e cont
  | exception InterpLib.Error.Fatal_error ->
    InterpLib.Error.reset ();
    (* TODO: backtrack references *)
    eval_repl env func cont

let eval_program p =
  eval_expr Env.empty p ignore
