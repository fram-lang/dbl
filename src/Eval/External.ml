(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value

(** External functions *)
(* ========================================================================= *)

let pure_fun f = VFn (fun v cont -> cont (f v))

let unit_fun f = VFn (fun v cont -> cont (f ()))

let int_fun f = VFn (fun v cont ->
  match v with
  | VNum n -> cont (f n)
  | _ -> failwith "Runtime error!")

let str_fun f = VFn (fun v cont ->
  match v with
  | VStr s -> cont (f s)
  | _ -> failwith "Runtime error!")

let list_chr_fun f = VFn (fun v cont ->
  let rec parse_list = function
  | VCtor(0, []) -> []
  | VCtor(1, [VNum x; xs]) -> Char.chr x :: parse_list xs 
  | _ -> failwith "Runtime error!" in
  cont (f @@ parse_list v))

let ref_fun f = VFn (fun v cont ->
  match v with
  | VRef r -> cont (f r)
  | _ -> failwith "Runtime error!")

let array_fun f = VFn (fun v cont ->
  match v with
  | VArray a -> cont (f a)
  | _ -> failwith "Runtime error!")

let v_unit = VCtor(0, [])

(** Empty constructor with some abstract types *)
let v_abstr = VCtor(0, [])

let of_bool b =
  VCtor((if b then 1 else 0), [])

let rec of_list = function
  | [] -> VCtor(0, [])
  | x :: xs -> VCtor(1, [x; of_list xs]) 

let int_fun2 f = int_fun (fun x -> int_fun (f x))

let int_unop  op = int_fun  (fun x -> VNum (op x))
let int_binop op = int_fun2 (fun x y -> VNum (op x y))
let int_cmpop op = int_fun2 (fun x y -> of_bool (op x y))

let int64_fun f = VFn (fun v cont ->
  match v with
  | VNum64 n -> cont (f n)
  | _ -> failwith "Runtime error!")

let int64_fun2 f = int64_fun (fun x -> int64_fun (f x))

let int64_unop  op = int64_fun  (fun x -> VNum64 (op x))
let int64_binop op = int64_fun2 (fun x y -> VNum64 (op x y))
let int64_int_op op = int64_fun2 (fun x y -> VNum64 (op x (Int64.to_int y)))
let int64_cmpop op = int64_fun2 (fun x y -> of_bool (op x y))

let str_cmpop op = str_fun (fun s1 -> str_fun (fun s2 -> of_bool (op s1 s2)))

let extern_map =
  [ "dbl_magic",       pure_fun Fun.id;
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
    "dbl_strCat",  str_fun (fun s1 -> str_fun (fun s2 -> VStr(s1 ^ s2)));
    "dbl_eqStr",   str_cmpop ( = );
    "dbl_neqStr",  str_cmpop ( <> );
    "dbl_gtStr",   str_cmpop ( > );
    "dbl_ltStr",   str_cmpop ( < );
    "dbl_geStr",   str_cmpop ( >= );
    "dbl_leStr",   str_cmpop ( <= );
    "dbl_strLen",  str_fun (fun s -> VNum (String.length s));
    "dbl_strGet",  str_fun (fun s -> int_fun (fun n -> VNum (Char.code s.[n])));
    "dbl_chrToString",  int_fun (fun c -> VStr (Char.escaped (Char.chr c)));
    "dbl_chrListToStr", list_chr_fun (fun xs -> VStr (List.to_seq xs |> String.of_seq));
    "dbl_chrCode",    int_fun (fun c -> VNum c);
    "dbl_intToChr",   int_fun (fun n -> VNum n);
    "dbl_printStrLn", str_fun (fun s -> print_endline s; v_unit);
    "dbl_printStr",   str_fun (fun s -> print_string s; v_unit);
    "dbl_printInt",   int_fun (fun n -> print_int n; v_unit);
    "dbl_readLine",   unit_fun (fun () -> VStr (read_line ()));
    "dbl_exit",       int_fun exit;
    "dbl_negInt64",      int64_unop Int64.neg;
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
    "dbl_abstrType",  unit_fun (fun () -> v_abstr);
    "dbl_ref",        pure_fun (fun x -> VRef (ref x));
    "dbl_refGet",     ref_fun (!);
    "dbl_refSet",     ref_fun (fun r -> pure_fun (fun v -> r := v; v_unit));
    "dbl_mkArray",    int_fun (fun n -> VArray(Array.make n v_unit));
    "dbl_arrayGet",   array_fun (fun a -> int_fun (fun n -> a.(n)));
    "dbl_arraySet",   array_fun (fun a -> int_fun (fun n -> pure_fun (fun v ->
                        a.(n) <- v; v_unit)));
    "dbl_arrayLength", array_fun (fun a -> VNum (Array.length a));
  ] |> List.to_seq |> Hashtbl.of_seq
