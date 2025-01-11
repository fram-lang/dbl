(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Partial environments created by patterns. *)

type t = unit

let empty = ()

let singleton_tvar ~public ~pos name x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let singleton_tvar_alias ~public ~pos name x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let singleton_var ~public ~pos name sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let singleton_implicit ~public ~pos name sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let singleton_method ~public ~pos owner name sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let singleton_module ~public ~pos ~types ~vars ~implicits ~methods modname =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_anon_tvar ~pos penv x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_tvar_alias ~public ~pos penv name x =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_var ~public ~pos penv name x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_implicit ~public ~pos penv name x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let add_method ~public ~pos penv owner name x sch =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let join penv1 penv2 =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let extend env penv =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let type_names penv =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let var_names penv =
  (* TODO: not implemented *)
  begin match None with Some x -> x end

let implicit_names penv =
  (* TODO: not implemented *)
  begin match None with Some x -> x end
