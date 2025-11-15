(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Attribute handlers. *)

(** Resolves attributes for given definition list. *)
val tr_attrs :
  Raw.attribute list -> Lang.Surface.def list -> Lang.Surface.def list

(** Makes pattern public. *)
val make_vis_pattern : Lang.Surface.pattern -> Lang.Surface.pattern
