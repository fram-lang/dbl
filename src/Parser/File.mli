(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Parse a single file with no handling of imports *)

type fname = string
type def_list = Lang.Surface.def list Lang.Surface.node

(** Parse a single source file into a list of imports and definitions. *)
val parse_defs : ?pos:Position.t -> fname -> Raw.import list * def_list
