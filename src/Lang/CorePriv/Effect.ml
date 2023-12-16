(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on effects *)

(* Author: Piotr Polesiuk, 2023 *)

open TypeBase

(** Join of two effects. Same as TEffJoin constructor, but removes duplicates.
  *)
let rec join eff1 eff2 =
  match eff1, eff2 with
  | TEffPure, eff -> eff
  | eff, TEffPure -> eff

  | TEffJoin(eff_a, eff_b), _ ->
    join eff_a (join eff_b eff2)

  | TVar _, _ ->
    if Type.simple_subeffect eff1 eff2 then eff2
    else TEffJoin(eff1, eff2)

(** IO effect *)
let io = (* TODO *) TEffPure

(** Possible non-termination effect *)
let nterm = (* TODO *) TEffPure

(** Effect of a whole program: IO + #NTerm *)
let prog_effect = TEffJoin(io, nterm)
