(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open ExternalUtils
open Value

let int_fun2 f = int_fun (fun x -> int_fun (f x))

let int_unop  op = int_fun  (fun x -> VNum (op x))
let int_binop op = int_fun2 (fun x y -> VNum (op x y))
let int_cmpop op = int_fun2 (fun x y -> of_bool (op x y))

let extern_int_seq =
  [ "dbl_maxInt",      VNum Int.max_int;
    "dbl_minInt",      VNum Int.min_int;
    "dbl_negInt",      int_unop ( ~- );
    "dbl_addInt",      int_binop ( + );
    "dbl_subInt",      int_binop ( - );
    "dbl_mulInt",      int_binop ( * );
    "dbl_divInt",      int_binop ( / );
    "dbl_modInt",      int_binop ( mod );
    "dbl_andInt",      int_binop ( land );
    "dbl_orInt",       int_binop ( lor );
    "dbl_xorInt",      int_binop ( lxor );
    "dbl_lslInt",      int_binop ( lsl );
    "dbl_lsrInt",      int_binop ( lsr );
    "dbl_asrInt",      int_binop ( asr );
    "dbl_eqInt",       int_cmpop ( = );
    "dbl_neqInt",      int_cmpop ( <> );
    "dbl_gtInt",       int_cmpop ( > );
    "dbl_ltInt",       int_cmpop ( < );
    "dbl_geInt",       int_cmpop ( >= );
    "dbl_leInt",       int_cmpop ( <= );
    "dbl_intToString", int_fun (fun n -> VStr (string_of_int n));
    "dbl_intToChr",   int_fun (fun n -> VNum n);
    "dbl_printInt",   int_fun (fun n -> print_int n; v_unit);
  ] |> List.to_seq

