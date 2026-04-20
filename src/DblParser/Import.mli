(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Find and parse imported modules *)

(** Type used to maintain the set of previously imported modules between
    calls to functions in this module. *)
type import_set

(** Empty set of imported modules. *)
val import_set_empty : import_set

(** Parse one import and its dependencies, and return the list of definitions
    required by the importer. *)
val import_one : import_set -> Raw.import -> import_set * Lang.Surface.def list

(** Parse a list of imports and their dependencies and return the list of
    definitions required by the importer. *)
val import_many :
  import_set -> Raw.import list -> import_set * Lang.Surface.def list

(** Parse the prelude and any dependencies and return the set of imported
    modules and list of definitions. *)
val import_prelude : unit -> import_set * Lang.Surface.def list

(** Load all definitions from startup files specified in the command-line
    arguments, returning the updated set of imported modules and list of
    definitions. *)
val load_startup_files : import_set -> import_set * Lang.Surface.def list

(** Parse imports and prepend them to a complete program. *)
val prepend_imports :
  use_prelude:bool -> Raw.import list -> Lang.Surface.program ->
  Lang.Surface.program
