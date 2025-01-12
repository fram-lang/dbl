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

(** Environment of pretty-printing *)
module PPEnv : sig
  type t

  val create : ctx -> Env.t -> t

  val add_tvar : t -> string -> T.tvar -> t

  val add_anon_tvar : t -> Position.t option -> string -> T.tvar -> unit

  val get_context : t -> ctx

  val lookup_tvar : t -> T.tvar -> PPTree.pp_result

  val is_anon_tvar_fresh : t -> string -> bool
end = struct
  type t = {
    local_names : string T.TVar.Map.t;
    pp_tree     : PPTree.t;
    context     : ctx
  }

  let create ctx env = {
    local_names = T.TVar.Map.empty;
    pp_tree     = Env.pp_tree env;
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

  let lookup_tvar env x =
    match T.TVar.Map.find_opt x env.local_names with
    | Some name -> PPTree.Found name
    | None ->
      begin match PPTree.lookup env.pp_tree (T.TVar.uid x) with
      | Found name -> PPTree.Found name
      | _ as result ->
        begin match T.TVar.Map.find_opt x env.context.tvar_map with
        | Some name -> PPTree.Found name
        | None -> result
        end
      end

  let is_anon_tvar_fresh env name =
    not (Hashtbl.mem env.context.used_tvars name)
end

(* ========================================================================= *)

let paren buf prec eprec pp =
  if prec > eprec then Buffer.add_char buf '(';
  pp ();
  if prec > eprec then Buffer.add_char buf ')'

(* ========================================================================= *)

let kuvar_to_string ctx u  =
  let kuvar_name (u', name) =
    if T.KUVar.equal u u' then Some name
    else None
  in
  match List.find_map kuvar_name ctx.kuvar_list with
  | Some name -> name
  | None ->
    let n = List.length ctx.kuvar_list in
    let name = "?k" ^ string_of_int n in
    ctx.kuvar_list <- (u, name) :: ctx.kuvar_list;
    name

let rec pp_kind ctx buf prec k =
  match T.Kind.view k with
  | KType   -> Buffer.add_string buf "type"
  | KEffect -> Buffer.add_string buf "effect"
  | KUVar u ->
    Buffer.add_string buf (kuvar_to_string ctx u)
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
  | Found name -> name
  | Anon(base, pos) ->
    let name = gen_tvar_name (PPEnv.is_anon_tvar_fresh env) ("#" ^ base) in
    PPEnv.add_anon_tvar env pos name x;
    name
  | Unbound _ ->
    let name = gen_tvar_name (PPEnv.is_anon_tvar_fresh env) "#?T" in
    PPEnv.add_anon_tvar env None name x;
    name

(* ========================================================================= *)

let fresh_for_tvar env name x =
  begin match PPEnv.lookup_tvar env x with
  | Found name' -> name <> name'
  | _ -> true
  end

let rec fresh_for_type env name tp =
  match T.Type.view tp with
  | TEffect | TUVar _ -> true
  | TVar x -> fresh_for_tvar env name x
  | TArrow(sch, tp, _) ->
    fresh_for_scheme env name sch && fresh_for_type env name tp
  | THandler(_, cap_tp, tp_in, tp_out) ->
    fresh_for_type env name cap_tp &&
    fresh_for_type env name tp_in &&
    fresh_for_type env name tp_out
  | TLabel delim_tp ->
    fresh_for_type env name delim_tp
  | TApp(tp1, tp2) ->
    fresh_for_type env name tp1 && fresh_for_type env name tp2

and fresh_for_scheme env name { T.sch_targs = _; sch_named; sch_body } =
  List.for_all (fun (_, sch) -> fresh_for_scheme env name sch) sch_named &&
  fresh_for_type env name sch_body

(* ========================================================================= *)

let rec pp_type buf env prec tp =
  match T.Type.view tp with
  | TEffect -> Buffer.add_string buf "[_]"
  | TUVar(_, u) -> Buffer.add_string buf (pp_uvar env u)
  | TVar x -> Buffer.add_string buf (pp_tvar env x)
  | TArrow(sch, tp, eff) ->
    paren buf prec 0 (fun () ->
      pp_scheme buf env 1 sch;
      Buffer.add_string buf
        (match eff with
        | Pure   -> " -> "
        | Impure -> " ->> ");
      pp_type buf env 0 tp)
  | THandler(x, cap_tp, tp_in, tp_out) ->
    paren buf prec 0 (fun () ->
      let name = gen_tvar_name
        (fun n ->
          fresh_for_type env n cap_tp &&
          fresh_for_type env n tp_in &&
          fresh_for_type env n tp_out)
        "E" in
      let env1 = PPEnv.add_tvar env name x in
      Buffer.add_string buf "handler ";
      Buffer.add_string buf name;
      Buffer.add_string buf " of ";
      pp_type buf env1 1 cap_tp;
      Buffer.add_string buf " with [_] ";
      pp_type buf env1 1 tp_in;
      Buffer.add_string buf " ->> ";
      pp_type buf env 1 tp_out)
  | TLabel delim_tp ->
    paren buf prec 0 (fun () ->
      Buffer.add_string buf "label _ [_] ";
      pp_type buf env 11 delim_tp)
  | TApp(tp1, tp2) ->
    paren buf prec 10 (fun () ->
      pp_type buf env 10 tp1;
      Buffer.add_string buf " ";
      pp_type buf env 11 tp2)

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

let additional_info ctx =
  let buf = Buffer.create 80 in
  List.rev ctx.anon_tvar_pos |> List.iter (fun (pos, name) ->
    Printf.bprintf buf "\n  Type %s is defined at %s"
      name
      (Position.to_string pos));
  Buffer.contents buf

(* ========================================================================= *)

let kind_to_string ctx k =
  let buf = Buffer.create 80 in
  pp_kind ctx buf 0 k;
  Buffer.contents buf

let tvar_to_string ctx env x =
  pp_tvar (PPEnv.create ctx env) x

let type_to_string ctx env tp =
  let buf = Buffer.create 80 in
  pp_type buf (PPEnv.create ctx env) 0 tp;
  Buffer.contents buf

let scheme_to_string ctx env sch =
  let buf = Buffer.create 80 in
  pp_scheme buf (PPEnv.create ctx env) 0 sch;
  Buffer.contents buf
