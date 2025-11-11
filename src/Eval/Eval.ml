(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value

exception Runtime_error = ExternalUtils.Runtime_error

(** Evaluator *)
(* ========================================================================= *)

let run_stack v stack =
  match stack with
  | [] -> failwith "Runtime error"
  | f :: stack -> f.f_ret v f.f_cont stack

(** reset0 operator *)
let reset0 l vs comp ret cont stack =
  comp run_stack
    ({ f_label = l; f_vals = vs; f_ret = ret; f_cont = cont } :: stack)

let rec reify_cont gstack v cont stack =
  match gstack with
  | [] -> cont v stack
  | f :: gstack ->
    reify_cont gstack v f.f_cont ({ f with f_cont = cont } :: stack)

let rec grab l comp cont gstack stack =
  match stack with
  | [] -> failwith "Unhandled effect"
  | f :: stack ->
    let gstack = { f with f_cont = cont } :: gstack in
    let cont = f.f_cont in
    if f.f_label = l then
      comp f.f_vals (VFn(reify_cont gstack)) cont stack
    else
      grab l comp cont gstack stack

(** Shift0 operator *)
let shift0 l comp cont stack =
  grab l comp cont [] stack

let eval_lit (l : Lang.Untyped.lit) =
  match l with
  | LNum   n -> VNum n
  | LNum64 n -> VNum64 n
  | LStr   s -> VStr s

let rec eval_expr env (e : Lang.Untyped.expr) cont =
  match e with
  | EValue v -> cont (eval_value env v)
  | ELet(x, e1, e2) ->
    eval_expr env e1 (fun v ->
    eval_expr (Env.extend env x v) e2 cont)
  | ELetRec(rds, e2) ->
    let (env, rds) =
      List.fold_left_map
        (fun env (x, e) ->
          let (env, box) = Env.extend_box env x in
          (env, (box, e)))
        env rds in
    eval_rec_defs env rds (fun env -> eval_expr env e2 cont)
  | EFn(x, body) ->
    cont (VFn(fun v -> eval_expr (Env.extend env x v) body))
  | EApp(e1, v2) ->
    eval_expr env e1 (function
    | VFn f -> f (eval_value env v2) cont
    | v ->
      failwith ("Runtime error: expected <fun>, actual: "^to_string v^"!"))
  | ECtor(n, vs) ->
    cont (VCtor(n, List.map (eval_value env) vs))
  | EMatch(v, cls) ->
    begin match eval_value env v with
    | VCtor(n, vs) ->
      let (xs, body) = List.nth cls n in
      let env = List.fold_left2 Env.extend env xs vs in
      eval_expr env body cont
    | v -> failwith ("Runtime error: expected <ctor>, actual: "^to_string v^"!")
    end
  | ELabel(x, e) ->
    let l = UID.fresh () in
    eval_expr (Env.extend env x (VLabel l)) e cont
  | EShift(v, xs, x, e) ->
    begin match eval_value env v with
    | VLabel l ->
      shift0 l
        (fun vs k ->
          let env = List.fold_left2 Env.extend env xs vs in
          let env = Env.extend env x k in
          eval_expr env e)
        cont
    | v -> failwith ("Runtime error: expected <label>, actual: "^to_string v^"!")
    end
  | EReset(v, vs, e1, x, e2) ->
    begin match eval_value env v with
    | VLabel l ->
      let vs = List.map (eval_value env) vs in
      reset0 l vs (eval_expr env e1)
        (fun v -> eval_expr (Env.extend env x v) e2)
        cont
    | v -> failwith ("Runtime error: expected <label>, actual: "^to_string v^"!")
    end
  | ERepl func -> eval_repl env func cont
  | EReplExpr(e1, tp, e2) ->
    Printf.printf ": %s\n%!" tp;
    eval_expr env e1 (fun v1 ->
      Printf.printf "= %s\n" (as_string v1);
      eval_expr env e2 cont)

and eval_rec_defs env rds cont =
  match rds with
  | [] -> cont env
  | (box, e) :: rds ->
    eval_expr env e (fun v ->
    Env.update_box box v;
    eval_rec_defs env rds cont)

and eval_value env (v : Lang.Untyped.value) =
  match v with
  | VLit l -> eval_lit l
  | VVar x -> Env.lookup env x
  | VExtern name ->
    begin match Hashtbl.find_opt External.extern_map name with
    | Some v -> v
    | None   ->
      failwith ("Runtime error: undefined external function: " ^ name)
    end

and eval_repl env func cont =
  match func () with
  | e -> eval_expr env e cont
  | exception InterpLib.Error.Fatal_error ->
    InterpLib.Error.reset ();
    (* TODO: backtrack references *)
    eval_repl env func cont

let eval_program p =
  eval_expr Env.empty p (fun _ _ -> ()) []
