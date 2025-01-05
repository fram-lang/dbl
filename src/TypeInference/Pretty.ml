(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Pretty printing of types and kinds *)

open Common

type ctx = {
  mutable kuvar_list : (T.kuvar * string) list;
    (** Association list with the names of kind unification variables. *)

  mutable uvar_map : string T.UVar.Map.t;
    (** Names associated to unification type variables *)

          used_uvars : (string, unit) Hashtbl.t;
    (** Names already used to pretty-print unification variables *)

  mutable tvar_map : string T.TVar.Map.t;
    (** Names associated to anonymous or non-printable type variables *)

          used_tvars : (string, unit) Hashtbl.t;
    (** Names already used to pretty-print type variables *)

  mutable anon_tvar_pos : (Position.t * string) list
    (** Position of definitions of anonymous or non-printable type variables *)
}

let empty_context () = {
  kuvar_list    = [];
  uvar_map      = T.UVar.Map.empty;
  used_uvars    = Hashtbl.create 42;
  tvar_map      = T.TVar.Map.empty;
  used_tvars    = Hashtbl.create 42;
  anon_tvar_pos = []
}
(*
(** Environment of pretty-printing *)
module PPEnv : sig
  type t

  val create : ctx -> Env.t -> t

  val add_tvar : t -> string -> T.tvar -> t

  val add_anon_tvar : t -> Position.t option -> string -> T.tvar -> unit

  val get_context : t -> ctx

  val lookup_tvar : t -> T.tvar -> string option

  val lookup_tvar_pp_info : t -> T.tvar -> Env.pp_info option

  val is_tvar_name_fresh : t -> string -> bool
end = struct
  type t = {
    local_names : string T.TVar.Map.t;
    env         : Env.t;
    context     : ctx
  }

  let create ctx env = {
    local_names = T.TVar.Map.empty;
    env         = env;
    context     = ctx
  }

  let add_tvar env name x =
    { env with local_names = T.TVar.Map.add x name env.local_names }

  let add_anon_tvar env pos name x =
    env.context.tvar_map <- T.TVar.Map.add x name env.context.tvar_map;
    Hashtbl.add env.context.used_tvars name ();
    match pos with
    | None -> ()
    | Some pos ->
      env.context.anon_tvar_pos <- (pos, name) :: env.context.anon_tvar_pos

  let get_context env = env.context

  let tvar_valid_name env x name =
    match Env.lookup_tvar env.env name with
    | Some tp ->
      begin match T.Type.view tp with
      | TVar y -> T.TVar.equal x y
      | _      -> false
      end
    | None    -> false

  let rec pp_path (p : string S.path) =
    match p with
    | NPName x    -> x
    | NPSel(n, p) -> Printf.sprintf "%s.%s" n (pp_path p)

  let lookup_tvar env x =
    match T.TVar.Map.find_opt x env.local_names with
    | Some name -> Some name
    | None ->
      begin match Env.lookup_tvar_pp_info env.env x with
      | Some info ->
        begin match List.find_opt (tvar_valid_name env x) info.pp_names with
        | Some path -> Some (pp_path path)
        | None -> T.TVar.Map.find_opt x env.context.tvar_map
        end
      | None -> T.TVar.Map.find_opt x env.context.tvar_map
      end

  let lookup_tvar_pp_info env x =
    Env.lookup_tvar_pp_info env.env x

  let is_tvar_name_fresh env name =
    not (Hashtbl.mem env.context.used_tvars name)
end

(* ========================================================================= *)

let paren buf prec eprec pp =
  if prec > eprec then Buffer.add_char buf '(';
  pp ();
  if prec > eprec then Buffer.add_char buf ')'

(* ========================================================================= *)

let kuvar_to_string ctx u non_eff =
  let kuvar_name (u', name) =
    if T.KUVar.equal u u' then Some name
    else None
  in
  match List.find_map kuvar_name ctx.kuvar_list with
  | Some name -> name
  | None ->
    let n = List.length ctx.kuvar_list in
    let name = (if non_eff then "?_k" else "?k") ^ string_of_int n in
    ctx.kuvar_list <- (u, name) :: ctx.kuvar_list;
    name

let rec pp_kind ctx buf prec k =
  match T.Kind.view k with
  | KType   -> Buffer.add_string buf "type"
  | KEffect -> Buffer.add_string buf "effect"
  | KEffrow -> Buffer.add_string buf "effrow"
  | KUVar u ->
    Buffer.add_string buf (kuvar_to_string ctx u (T.Kind.non_effect k))
  | KArrow(k1, k2) ->
    paren buf prec 0 (fun () ->
      pp_kind ctx buf 1 k1;
      Buffer.add_string buf " -> ";
      pp_kind ctx buf 0 k2)

(* ========================================================================= *)

let gen_uvar_name is_fresh =
  let rec gen2 n =
    let name = Printf.sprintf "a%d" n in
    if is_fresh name then name
    else gen2 (n+1)
  in
  let rec gen1 c =
    let name = Printf.sprintf "%c" c in
    if is_fresh name then name
    else if c >= 'z' then gen2 0
    else gen1 (Char.chr (Char.code c + 1))
  in
  gen1 'a'

let is_uvar_name_fresh ctx env name =
  (* TODO: check environment *)
  not (Hashtbl.mem ctx.used_uvars name)

let pp_uvar env u =
  let ctx = PPEnv.get_context env in
  match T.UVar.Map.find_opt u ctx.uvar_map with
  | Some name -> name
  | None ->
    let name = gen_uvar_name (is_uvar_name_fresh ctx env) in
    ctx.uvar_map <- T.UVar.Map.add u name ctx.uvar_map;
    Hashtbl.add ctx.used_uvars name ();
    name
    
(* ========================================================================= *)

let gen_tvar_name is_fresh base =
  let rec loop n =
    let name = Printf.sprintf "%s%d" base n in
    if is_fresh name then name
    else loop (n+1)
  in
  if is_fresh base then base
  else loop 0

let pp_tvar env x =
  match PPEnv.lookup_tvar env x with
  | Some name -> name
  | None ->
    let (pos, base) =
      match PPEnv.lookup_tvar_pp_info env x with
      | None -> (None, "T")
      | Some info -> (info.pp_pos, info.pp_base_name)
    in
    let name = gen_tvar_name (PPEnv.is_tvar_name_fresh env) ("#" ^ base) in
    PPEnv.add_anon_tvar env pos name x;
    name

(* ========================================================================= *)

let fresh_for_tvar env name x =
  begin match PPEnv.lookup_tvar env x with
  | Some name' -> name <> name'
  | None -> true
  end

let rec fresh_for_type env name tp =
  match T.Type.view tp with
  | TUVar _ -> true
  | TVar x -> fresh_for_tvar env name x
  | TEffect xs -> T.TVar.Set.for_all (fresh_for_tvar env name) xs
  | TEffrow(xs, ee) ->
    T.TVar.Set.for_all (fresh_for_tvar env name) xs &&
    fresh_for_effrow_end env name ee
  | TPureArrow(sch, tp) ->
    fresh_for_scheme env name sch && fresh_for_type env name tp
  | TArrow(sch, tp, eff) ->
    fresh_for_scheme env name sch &&
    fresh_for_type env name tp &&
    fresh_for_type env name eff
  | THandler(_, tp, itp, ieff, otp, oeff) ->
    fresh_for_type env name tp &&
    fresh_for_type env name itp &&
    fresh_for_type env name ieff &&
    fresh_for_type env name otp &&
    fresh_for_type env name oeff
  | TLabel(eff, tp0, eff0) ->
    fresh_for_type env name eff &&
    fresh_for_type env name tp0 &&
    fresh_for_type env name eff0
  | TApp(tp1, tp2) ->
    fresh_for_type env name tp1 && fresh_for_type env name tp2

and fresh_for_effrow_end env name (ee : T.Type.effrow_end) =
  match ee with
  | EEClosed | EEUVar _ -> true
  | EEVar x -> fresh_for_tvar env name x
  | EEApp(tp1, tp2) ->
    fresh_for_type env name tp1 && fresh_for_type env name tp2

and fresh_for_scheme env name { T.sch_targs = _; sch_named; sch_body } =
  List.for_all (fun (_, sch) -> fresh_for_scheme env name sch) sch_named &&
  fresh_for_type env name sch_body

(* ========================================================================= *)

let rec pp_type buf env prec tp =
  match T.Type.view tp with
  | TUVar(_, u) -> Buffer.add_string buf (pp_uvar env u)
  | TVar x -> Buffer.add_string buf (pp_tvar env x)
  | TEffect xs ->
    Buffer.add_string buf "[";
    pp_effect_prefix buf env "" (T.TVar.Set.to_list xs);
    Buffer.add_string buf "]"
  | TEffrow(xs, ee) ->
    Buffer.add_string buf "[";
    pp_effect_prefix buf env "" (T.TVar.Set.to_list xs);
    pp_effrow_end buf env ee;
    Buffer.add_string buf "]"
  | TPureArrow(sch, tp) ->
    paren buf prec 0 (fun () ->
      pp_scheme buf env 1 sch;
      Buffer.add_string buf " -> ";
      pp_type buf env 0 tp)
  | TArrow(sch, tp, eff) ->
    let (xs, ee) = T.Type.effrow_view eff in
    paren buf prec 0 (fun () ->
      pp_scheme buf env 1 sch;
      Buffer.add_string buf " ->[";
      pp_effect_prefix buf env "" (T.TVar.Set.to_list xs);
      pp_effrow_end buf env ee;
      Buffer.add_string buf "] ";
      pp_type buf env 0 tp)
  | THandler(x, tp, itp, ieff, otp, oeff) ->
    paren buf prec 0 (fun () ->
      let name = gen_tvar_name
        (fun n -> 
          fresh_for_type env n tp &&
          fresh_for_type env n itp && fresh_for_type env n ieff &&
          fresh_for_type env n otp && fresh_for_type env n oeff)
        "E" in
      let env1 = PPEnv.add_tvar env name x in
      Buffer.add_string buf "handler ";
      Buffer.add_string buf name;
      Buffer.add_string buf ", ";
      pp_type buf env1 1 tp;
      Buffer.add_string buf " @ ";
      pp_type buf env1 1 itp;
      Buffer.add_string buf " / ";
      pp_type buf env1 1 ieff;
      Buffer.add_string buf " => ";
      pp_type buf env 1 otp;
      Buffer.add_string buf " / ";
      pp_type buf env 1 oeff)
  | TLabel(eff, tp0, eff0) ->
    paren buf prec 0 (fun () ->
      Buffer.add_string buf "label ";
      pp_type buf env 1 eff;
      Buffer.add_string buf " @ ";
      pp_type buf env 1 tp0;
      Buffer.add_string buf " / ";
      pp_type buf env 1 eff0)
  | TApp(tp1, tp2) ->
    paren buf prec 10 (fun () ->
      pp_type buf env 10 tp1;
      Buffer.add_string buf " ";
      pp_type buf env 11 tp2)

and pp_effect_prefix buf env sep xs =
  match xs with
  | [] -> ()
  | x :: xs ->
    Buffer.add_string buf sep;
    Buffer.add_string buf (pp_tvar env x);
    pp_effect_prefix buf env "," xs

and pp_effrow_end buf env (ee : T.Type.effrow_end) =
  match ee with
  | EEClosed -> ()
  | EEUVar(_, u) ->
    Buffer.add_string buf "|";
    Buffer.add_string buf (pp_uvar env u)
  | EEVar x ->
    Buffer.add_string buf "|";
    Buffer.add_string buf (pp_tvar env x)
  | EEApp(tp1, tp2) ->
    pp_type buf env 10 tp1;
    Buffer.add_string buf " ";
    pp_type buf env 11 tp2

and pp_scheme buf env prec (sch : T.scheme) =
  match sch.sch_targs, sch.sch_named with
  | [], [] -> pp_type buf env prec sch.sch_body
  | _ ->
    paren buf prec 0 (fun () ->
      let env = pp_scheme_args sch buf env "{" sch.sch_targs sch.sch_named in
      Buffer.add_string buf "} -> ";
      pp_type buf env 0 sch.sch_body)

and pp_scheme_args sch buf env sep targs nargs =
  match targs with
  | [] -> pp_scheme_named_args buf env sep nargs
  | (tname, x) :: targs ->
    Buffer.add_string buf sep;
    let xname =
      match tname with
      | TNAnon ->
        let xname = gen_tvar_name (fun n -> fresh_for_scheme env n sch) "T" in
        Buffer.add_string buf "type ";
        Buffer.add_string buf xname;
        xname
      | TNEffect ->
        let xname = gen_tvar_name (fun n -> fresh_for_scheme env n sch) "E" in
        Buffer.add_string buf "effect=";
        Buffer.add_string buf xname;
        xname
      | TNVar xn ->
        let xname = gen_tvar_name (fun n -> fresh_for_scheme env n sch) xn in
        Buffer.add_string buf xn;
        if xname <> xn then begin
          Buffer.add_string buf "=";
          Buffer.add_string buf xname
        end;
        xname
    in
    let env = PPEnv.add_tvar env xname x in
    pp_scheme_args sch buf env ", " targs nargs

and pp_scheme_named_args buf env sep nargs =
  match nargs with
  | [] -> env
  | (name, sch) :: nargs ->
    Buffer.add_string buf sep;
    let sch = 
      begin match name with
      | NLabel         -> Buffer.add_string buf "label"; sch
      | NVar x         -> Buffer.add_string buf x; sch
      | NOptionalVar x -> 
        assert (T.Scheme.is_monomorphic sch);
        Buffer.add_string buf x;
        begin match T.Type.view sch.sch_body with
        | TApp(_, tp) -> T.Scheme.of_type tp;
        (* Error here? *)
        | _           -> sch 
        end
      | NImplicit n    -> Buffer.add_string buf n; sch
      | NMethod n      -> Buffer.add_string buf n; sch
      end
    in
    Buffer.add_string buf ":";
    pp_scheme buf env 0 sch;
    pp_scheme_named_args buf env ", " nargs

(* ========================================================================= *)
*)
let additional_info ctx =
  let buf = Buffer.create 80 in
  List.rev ctx.anon_tvar_pos |> List.iter (fun (pos, name) ->
    Printf.bprintf buf "\n  Type %s is defined at %s"
      name
      (Position.to_string pos));
  Buffer.contents buf
(*
(* ========================================================================= *)
*)
let kind_to_string ctx k =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let buf = Buffer.create 80 in
  pp_kind ctx buf 0 k;
  Buffer.contents buf

let tname_to_string (name : T.tname) =
  match name with
  | TNAnon   -> "<anon>"
  | TNEffect -> "effect"
  | TNVar x  -> x
*)
let tvar_to_string ctx env x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  pp_tvar (PPEnv.create ctx env) x
*)
let type_to_string ctx env tp =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let buf = Buffer.create 80 in
  pp_type buf (PPEnv.create ctx env) 0 tp;
  Buffer.contents buf
*)

let scheme_to_string ctx env sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
(*
  let buf = Buffer.create 80 in
  pp_scheme buf (PPEnv.create ctx env) 0 sch;
  Buffer.contents buf
*)
