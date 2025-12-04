(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value


exception Runtime_error

(** External functions *)
(* ========================================================================= *)

let runtime_error msg =
  Printf.eprintf "Runtime error: %s\n%!" msg;
  raise Runtime_error

let runtime_error_with_postion file line msg =
  Printf.eprintf "runtime error at %s:%d:\n%s!\n" file line msg;
  raise Runtime_error

let pure_fun f = VFn (fun v cont -> cont (f v))

let unit_fun f = VFn (fun v cont -> cont (f ()))

let int_fun f = VFn (fun v cont ->
  match v with
  | VNum n -> cont (f n)
  | _ -> runtime_error "Not an integer")

let v_unit = VCtor(0, [])

(** Empty constructor with some abstract types *)
let v_abstr = VCtor(0, [])

let of_bool b =
  VCtor((if b then 1 else 0), [])

let rec of_list = function
  | [] -> VCtor(0, [])
  | x :: xs -> VCtor(1, [x; of_list xs]) 

let of_option opt =
  match opt with
  | Some x -> VCtor(1, [x])
  | None   -> VCtor(0, [])

let str_fun f = VFn (fun v cont ->
  match v with
  | VStr s -> cont (f s)
  | _ -> runtime_error "Not a string")


