(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open ExternalUtils
open Value

let list_chr_fun f = VFn (fun v cont ->
  let rec parse_list = function
  | VCtor(0, []) -> []
  | VCtor(1, [VNum x; xs]) -> Char.chr x :: parse_list xs 
  | _ -> runtime_error "Not a list" in
  cont (f @@ parse_list v))

let str_cmpop op = str_fun (fun s1 -> str_fun (fun s2 -> of_bool (op s1 s2)))

let extern_str_seq =
  [ "dbl_strCat",  str_fun (fun s1 -> str_fun (fun s2 -> VStr(s1 ^ s2)));
    "dbl_eqStr",   str_cmpop ( = );
    "dbl_neqStr",  str_cmpop ( <> );
    "dbl_gtStr",   str_cmpop ( > );
    "dbl_ltStr",   str_cmpop ( < );
    "dbl_geStr",   str_cmpop ( >= );
    "dbl_leStr",   str_cmpop ( <= );
    "dbl_strLen",  str_fun (fun s -> VNum (String.length s));
    "dbl_strGet", 
      str_fun (fun s -> int_fun (fun n -> VNum (Char.code s.[n])));
    "dbl_chrToString",  int_fun (fun c -> VStr (Char.escaped (Char.chr c)));
    "dbl_chrListToStr",
      list_chr_fun (fun xs -> VStr (List.to_seq xs |> String.of_seq));
    "dbl_chrCode",    int_fun (fun c -> VNum c);
    "dbl_printStrLn", str_fun (fun s -> print_endline s; v_unit);
    "dbl_printStr",   str_fun (fun s -> print_string s; v_unit);
    "dbl_readLine",   unit_fun (fun () -> VStr (read_line ()));
  ] |> List.to_seq

