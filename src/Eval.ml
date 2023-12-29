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

  | VCtor of int * value list
    (** Constructor of ADT *)

(** CPS Computations *)
and 'v comp = ('v -> ans) -> ans

(** Stack frame (related to control operators) *)
and frame =
  { f_label : UID.t
  ; f_cont  : (value -> ans)
  }

(** Answer type: depends on stack *)
and ans = frame list -> unit

let to_string (v : value) =
  match v with
  | VUnit   -> "()"
  | VFn   _ -> "<fun>"
  | VCtor _ -> "<ctor>"

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

let run_stack v stack =
  match stack with
  | [] -> failwith "Runtime error"
  | f :: stack -> f.f_cont v stack

(** reset0 operator *)
let reset0 l comp cont stack =
  comp run_stack ({ f_label = l; f_cont = cont } :: stack)

let rec reify_cont gstack v cont stack =
  match gstack with
  | [] -> cont v stack
  | f :: gstack ->
    reify_cont gstack v f.f_cont
      ({ f_label = f.f_label; f_cont = cont } :: stack)

let rec grab l comp cont gstack stack =
  match stack with
  | [] -> failwith "Unhandled effect"
  | f :: stack ->
    let gstack = { f_label = f.f_label; f_cont = cont } :: gstack in
    let cont = f.f_cont in
    if f.f_label = l then
      comp (VFn(reify_cont gstack)) cont stack
    else
      grab l comp cont gstack stack

(** Shift0 operator *)
let shift0 l comp cont stack =
  grab l comp cont [] stack

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
  | EMatch(v, cls) ->
    begin match eval_value env v with
    | VCtor(n, vs) ->
      let (xs, body) = List.nth cls n in
      let env = List.fold_left2 Env.extend env xs vs in
      eval_expr env body cont
    | _ -> failwith "Runtyime error!"
    end
  | EHandle(x, e, h) ->
    let l = UID.fresh () in
    let h = eval_h_expr env l h in
    reset0 l (eval_expr (Env.extend env x h) e) cont
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
  | VCtor(n, vs) ->
    VCtor(n, List.map (eval_value env) vs)

and eval_h_expr env l (h : Lang.Untyped.h_expr) =
  match h with
  | HEffect(x, r, body) ->
    VFn(fun v -> shift0 l (fun k ->
      eval_expr (Env.extend (Env.extend env x v) r k) body))

and eval_repl env func cont =
  match func () with
  | e -> eval_expr env e cont
  | exception InterpLib.Error.Fatal_error ->
    InterpLib.Error.reset ();
    (* TODO: backtrack references *)
    eval_repl env func cont

let eval_program p =
  eval_expr Env.empty p (fun _ _ -> ()) []
