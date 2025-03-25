(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** S-expressions *)

type t =
  | Sym  of string
  | Num  of int
  | List of t list

let max_length = 80
let max_indent = 40

let add_string buf str =
  Buffer.add_string buf str;
  Buffer.length buf < max_length

let rec try_fit buf s =
  match s with
  | Sym str -> add_string buf str
  | Num n   -> add_string buf (string_of_int n)
  | List [] -> add_string buf "()"
  | List (s :: ss) ->
    add_string buf "(" &&
    try_fit buf s &&
    List.for_all (fun s -> add_string buf " " && try_fit buf s) ss &&
    add_string buf ")"

let rec sexpr_to_rows ~indent prefix s suffix rest () =
  let ind = String.make indent ' ' ^ prefix in
  match s with
  | Sym s ->
    Seq.Cons(ind ^ s ^ suffix, rest)
  | Num n ->
    Seq.Cons(ind ^ string_of_int n ^ suffix, rest)
  | List [] ->
    Seq.Cons(ind ^ "()" ^ suffix, rest)
  | List (s1 :: ss) ->
    let buf = Buffer.create max_length in
    if
      add_string buf ind &&
      try_fit buf s &&
      add_string buf suffix
    then
      Seq.Cons(Buffer.contents buf, rest)
    else
      begin match ss with
      | [] ->
        sexpr_to_rows ~indent (prefix ^ "(") s1 (")" ^ suffix) rest ()
      | _ :: _ ->
        sexpr_to_rows ~indent (prefix ^ "(") s1 ""
          (sexprs_to_rows ~indent:(min (indent+2) max_indent)
            ss (")" ^ suffix) rest)
          ()
      end

and sexprs_to_rows ~indent ss suffix rest () =
  match ss with
  | []    -> assert false
  | [ s ] -> sexpr_to_rows ~indent "" s suffix rest ()
  | s :: ss ->
    sexpr_to_rows ~indent "" s ""
      (sexprs_to_rows ~indent ss suffix rest)
      ()

let pretty_rows s =
  sexpr_to_rows ~indent:0 "" s "" Seq.empty

let pretty_stdout s =
  Seq.iter print_endline (pretty_rows s)

let pretty_stderr s =
  Seq.iter (fun s -> Printf.eprintf "%s\n" s) (pretty_rows s)
