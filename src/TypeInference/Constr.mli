(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Constraints generated during type inference *)

open Common

type t =
  | ResolveMethod : { (** Unknown implictly provided method *)
      hole : T.poly_fun option BRef.t;
        (** Hole to be filled with method implementation *)

      vset : int Var.Map.t;
        (** Set of variables that cannot be used during method resolution.
          This set is just passed as [~vset] to [ParamResolve.resolve_method].
          Do not use it directly.
        *)

      pos : Position.t;
        (** Position of the place where parameter resoultion is requested *)

      env : 'st Env.t;
        (** Environment in which resolution is requested *)

      method_env : 'st Env.t;
        (** Environment in which the method should be searched *)

      self_tp : T.typ;
        (** Type of the method's owner *)

      mname : S.method_name;
        (** Name of the method to be resolved *)

      sch : T.scheme;
        (** Scheme of the method to be resolved *)
    } -> t
