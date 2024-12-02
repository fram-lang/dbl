(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common definitions of type-checker *)
(*
module S = Lang.Surface
module T = Lang.Unif

(** Direction of type inference. We never use values of these types. They
  are only used for indexing [request] and [response] GADTs. *)
type infer = Dummy_Infer
type check = Dummy_Check

(** Request of the bidirectional type checking. It is indexed by a direction.
  *)
type ('a, _) request =
  | Infer : ('a, infer) request
    (** Type inference mode *)

  | Check : 'a -> ('a, check) request
    (** Type checking mode *)

(** Response of the bidirectional type checking. It is indexed by a direction.
  *)
type ('a, _) response =
  | Infered : 'a -> ('a, infer) response
    (** The result of type inference mode *)

  | Checked : ('a, check) response
    (** The result of type checking mode *)

(** Extract result type, from request and response *)
let bidir_result (type dir)
    (req : (_, dir) request) (resp : (_, dir) response) =
  match req, resp with
  | Infer, Infered tp -> tp
  | Check tp, Checked -> tp

(** Return information about effect. *)
type ret_effect =
  | Pure
    (** Expression is pure, i.e, it does not perform any effects, and always
      terminates *)

  | Impure
    (** Expression is inpure *)

let ret_effect_join eff1 eff2 =
  match eff1, eff2 with
  | Pure,   eff2   -> eff2
  | eff1,   Pure   -> eff1
  | Impure, Impure -> Impure

let ret_effect_joins effs =
  List.fold_left ret_effect_join Pure effs

(** Effect of match_clause *)
let match_effect reff (eff : T.effect) =
  match reff with
  | Pure   -> None
  | Impure -> Some eff
*)
