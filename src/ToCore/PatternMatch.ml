(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Translation of pattern-matching *)

(* Author: Piotr Polesiuk, 2023 *)

open Common
open PatternContext

(** Take a variable that stores given value, and pass to given
  meta-continuation in order to produce an expression *)
let as_variable (v : T.value) cont =
  match v with
  | VVar x -> cont x
  | _ ->
    let x = Var.fresh () in
    T.ELetPure(x, T.EValue v, cont x)

(** Context of pattern-matching *)
module type MatchContext = sig
  (** Location of the matching expression *)
  val pos     : Position.t

  (** Result type of the whole matching expression *)
  val res_tp  : T.ttype

  (** Result effect of the whole matching expression *)
  val res_eff : T.effect
end

module Make(Ctx : MatchContext) = struct
  (** Generalized clause of the pattern matching *)
  type clause = {
    cl_env      : Env.t;
      (** Environment of the clause *)

    cl_patterns : S.pattern list;
      (** List of patterns matched against the list of values *)

    cl_body     : Env.t -> T.expr;
      (** Body-generating function *)

    cl_used     : bool ref
      (* A flag that indicates if the pattern was used *)
  }

  (** Class of column (first patterns of generalized clauses) *)
  type column_class =
    | CCVar
      (** All patterns are variables or wild-cards *)

  (** Return the class of the first column *)
  let rec column_class cls =
    match cls with
    | [] -> CCVar
    | { cl_patterns = []; _ } :: _ -> assert false
    | { cl_patterns = p :: _; _ } :: cls ->
      begin match p.data with
      | PVar _ -> column_class cls
      end
  
  (* ======================================================================= *)

  (** Simplify a single clause of [CCVar] class, assuming that matched value
    is stored in variable [x]. *)
  let simplify_var_clause x cl =
    match cl.cl_patterns with
    | [] -> assert false
    | p :: ps ->
      begin match p.data with
      | PVar y ->
        { cl_env      = cl.cl_env;
          cl_patterns = ps;
          cl_body     =
            (fun env -> T.ELetPure(y, T.EValue (T.VVar x), cl.cl_body env));
          cl_used     = cl.cl_used
        }
      end

  (** Simplify clauses of [CCVar] class, assuming that matched value is stored
    in variable [x]. *)
  let simplify_var x cls =
    List.map (simplify_var_clause x) cls

  (* ======================================================================= *)

  (** Main function of the translation. It solves a bit more general problem:
    it takes list of values [vs] and list of clauses [cls], where each of
    clauses have list of patterns (the same length as [vs]). *)
  let rec tr_match ctx vs cls =
    match vs, cls with
    | [], cl :: _ ->
      assert (List.is_empty cl.cl_patterns);
      cl.cl_used := true;
      cl.cl_body cl.cl_env

    | [], [] ->
      (* TODO: non-exhaustive pattern-match doesn't have to be fatal *)
      Error.fatal (Error.non_exhaustive_match ~pos:Ctx.pos ctx)

    | v :: vs, _ ->
      begin match column_class cls with
      | CCVar ->
        as_variable v (fun x ->
        tr_match (refocus ctx) vs (simplify_var x cls))
      end

  (** Create an internal representation of a clause that matches single value
    *)
  let mk_clause env (pat, body) =
    { cl_env      = env;
      cl_patterns = [ pat ];
      cl_body     = body;
      cl_used     = ref false
    }

  let tr_single_match env v cls =
    let cls = List.map (mk_clause env) cls in
    tr_match CtxRoot [v] cls
    (* TODO: warn redundant patterns/clauses *)
end

let tr_single_match ~pos ~env v cls tp eff =
  let module M = Make(struct
    let pos     = pos
    let res_tp  = tp
    let res_eff = eff
  end)
  in
  M.tr_single_match env v cls
