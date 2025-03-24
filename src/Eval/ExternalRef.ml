(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open ExternalUtils
open Value

let ref_fun f = VFn (fun v cont ->
  match v with
  | VRef r -> cont (f r)
  | _ -> runtime_error "Not a reference")

let array_fun f = VFn (fun v cont ->
  match v with
  | VArray a -> cont (f a)
  | _ -> runtime_error "Not an array")

let extern_ref_seq =
  [ "dbl_ref",        pure_fun (fun x -> VRef (ref x));
    "dbl_refGet",     ref_fun (!);
    "dbl_refSet",     ref_fun (fun r -> pure_fun (fun v -> r := v; v_unit));
    "dbl_mkArray",    int_fun (fun n -> VArray(Array.make n v_unit));
    "dbl_arrayGet",   array_fun (fun a -> int_fun (fun n -> a.(n)));
    "dbl_arraySet",   array_fun (fun a -> int_fun (fun n -> pure_fun (fun v ->
                        a.(n) <- v; v_unit)));
    "dbl_arrayLength", array_fun (fun a -> VNum (Array.length a));
  ] |> List.to_seq

