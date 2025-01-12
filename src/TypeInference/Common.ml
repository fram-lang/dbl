(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Common definitions of type-checker *)

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

(** Build an AST node with dummy position *)
let make_nowhere data =
  { T.data; T.pos = Position.nowhere }

(** Translate a type name *)
let tr_tname (name : S.tname) : T.tname =
  match name with
  | S.TNAnon  -> T.TNAnon
  | S.TNVar x -> T.TNVar x

(** Translate a name of parameter *)
let tr_name (name : S.name) : T.name =
  match name with
  | S.NVar x         -> T.NVar x
  | S.NOptionalVar x -> T.NOptionalVar x
  | S.NImplicit x    -> T.NImplicit x
  | S.NMethod x      -> T.NMethod x
