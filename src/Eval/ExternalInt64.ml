(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open ExternalHelpers
open Value

let int64_fun f = VFn (fun v cont ->
  match v with
  | VNum64 n -> cont (f n)
  | _ -> runtime_error "Not a 64-bit integer")

let int64_fun2 f = int64_fun (fun x -> int64_fun (f x))

let int64_unop  op = int64_fun  (fun x -> VNum64 (op x))
let int64_binop op = int64_fun2 (fun x y -> VNum64 (op x y))
let int64_int_op op = int64_fun2 (fun x y -> VNum64 (op x (Int64.to_int y)))
let int64_cmpop op = int64_fun2 (fun x y -> of_bool (op x y))


let extern_int64_seq =
  [ "dbl_negInt64",      int64_unop Int64.neg;
    "dbl_addInt64",      int64_binop Int64.add;
    "dbl_subInt64",      int64_binop Int64.sub;
    "dbl_mulInt64",      int64_binop Int64.mul;
    "dbl_divInt64",      int64_binop Int64.div;
    "dbl_modInt64",      int64_binop Int64.rem;
    "dbl_andInt64",      int64_binop Int64.logand;
    "dbl_orInt64",       int64_binop Int64.logor;
    "dbl_xorInt64",      int64_binop Int64.logxor;
    "dbl_lslInt64",      int64_int_op Int64.shift_left;
    "dbl_lsrInt64",      int64_int_op Int64.shift_right_logical;
    "dbl_asrInt64",      int64_int_op Int64.shift_right;
    "dbl_eqInt64",       int64_cmpop ( = );
    "dbl_neqInt64",      int64_cmpop ( <> );
    "dbl_gtInt64",       int64_cmpop ( > );
    "dbl_ltInt64",       int64_cmpop ( < );
    "dbl_geInt64",       int64_cmpop ( >= );
    "dbl_leInt64",       int64_cmpop ( <= );
    "dbl_intToInt64",    int_fun (fun n -> VNum64 (Int64.of_int n));
    "dbl_int64ToInt",    int64_fun (fun n -> VNum (Int64.to_int n));
    "dbl_int64ToString", int64_fun (fun n -> VStr (Int64.to_string n));
  ] |> List.to_seq

