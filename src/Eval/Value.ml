(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Value of the evaluator *)
type value =
  | VNum of int
    (** Number *)

  | VNum64 of int64
    (** 64 bit number *)

  | VStr of string
    (** String *)

  | VFn of (value -> value comp)
    (** Function *)

  | VCtor of int * value list
    (** Constructor of ADT *)

  | VLabel of UID.t
    (** Runtime label of control operator *)

  | VRef of value ref
    (** Mutable reference *)

  | VArray of value array
    (** Mutable arrays *)

(** CPS Computations *)
and 'v comp = ('v -> ans) -> ans

(** Stack frame (related to control operators) *)
and frame =
  { f_label : UID.t
  ; f_vals  : value list
  ; f_ret   : (value -> value comp)
  ; f_cont  : (value -> ans)
  }

(** Answer type: depends on stack *)
and ans = frame list -> unit

let to_string (v : value) =
  match v with
  | VNum n   -> string_of_int n
  | VNum64 n -> Int64.to_string n
  | VStr s   -> Printf.sprintf "\"%s\"" (String.escaped s)
  | VFn    _ -> "<fun>"
  | VCtor  _ -> "<ctor>"
  | VLabel _ -> "<label>"
  | VRef   _ -> "<ref>"
  | VArray _ -> "<array>"
