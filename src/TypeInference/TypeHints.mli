(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Extraction of type hints

  In order to avoid some ambiguities during type inference, unification
  variables of an effect kind are forbidden. This makes instantiation of
  polymorphic scheme much more complicated. Normally, type parameters that
  were not explicitly provided are guessed by instantiating them with fresh
  unification variables. However, for effect parameters we need another
  solution: some of type parameters are inferred based on types of actual
  parameters. *)

open Common
open TypeCheckFix

(** Type hints *)
type t = T.typ T.TVar.Map.t

(** Cached translated explicit instantiations. During extraction of type hints
  type of some of named parameters is inferred. This parameters are stored in
  the cache in order to not infer their types again. *)
type inst_cache = (T.name * (T.expr * T.typ * ret_effect)) list

(** Merge type hints *)
val merge_hints : t -> t -> t

(** Get type instantiation hints, by matching the method scheme with the
  actual type of the "self" value. Returned hints are a partial map from
  type variables bound by the scheme to types. It assumes that the scheme
  is valid for given self type, i.e., is a function scheme, where the
  argument is monomorphic neutral type with the same head as the self type. *)
val method_inst_hints : T.scheme -> T.typ -> t

(** Get type instantiation hints, by matching requested argument type
  (the second parameter) with its actual type (the third parameter).
  The first parameter is a list of types generalized by a scheme to be
  instantiated. *)
val type_inst_hints : T.named_tvar list -> T.typ -> T.typ -> t

(** Extracts type hints for a type scheme from environment and explicit
  instantiation. Returns hints and cached translated explicit
  instantiations. *)
val extract_implicit_type_hints : tcfix:tcfix ->
  pos:Position.t -> Env.t -> T.scheme -> S.inst list -> T.effrow ->
    t * inst_cache
