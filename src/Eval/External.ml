(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

open Value
open ExternalUtils

let extern_map =
  let extern_map =
    [ "dbl_runtimeError", str_fun (fun fname -> 
                            int_fun (fun line -> 
                              str_fun (fun msg -> 
                                runtime_error_with_postion fname line msg)));
      "dbl_magic",     pure_fun Fun.id;
      "dbl_toString",  pure_fun (fun v -> VStr (Value.to_string v));
      "dbl_exit",      int_fun exit;
      "dbl_abstrType", unit_fun (fun () -> v_abstr);
    ] |> List.to_seq |> Hashtbl.of_seq
  and lib_externs =
    [ ExternalInt.extern_int_seq;
      ExternalStr.extern_str_seq;
      ExternalInt64.extern_int64_seq;
      ExternalRef.extern_ref_seq;
      ExternalOs.extern_os_seq;
    ] |> List.to_seq |> Seq.concat in
  Hashtbl.add_seq extern_map lib_externs;
  extern_map
