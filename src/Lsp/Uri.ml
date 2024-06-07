(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** URI helper functions *)

let to_path uri =
  String.sub uri (String.length "file://")
    (String.length uri - String.length "file://")
