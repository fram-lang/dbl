(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of names. It differs from the names in Unif in that
  the method owner is explicit. *)

open Common

type method_owner =
  | MO_Arrow
  | MO_Handler
  | MO_Label
  | MO_TVar of T.tvar

type t =
  | NVar         of string
  | NOptionalVar of string
  | NImplicit    of string
  | NMethod      of method_owner * string

type scheme =
  { sch_targs : T.named_tvar list;
    sch_named : (t * T.scheme) list;
    sch_body  : T.typ
  }

let to_unif name =
  match name with
  | NVar x         -> T.NVar x
  | NOptionalVar x -> T.NOptionalVar x
  | NImplicit x    -> T.NImplicit x
  | NMethod (_, x) -> T.NMethod x

let tr_scheme sch =
  { T.sch_targs = sch.sch_targs;
    T.sch_named = List.map (fun (n, s) -> (to_unif n, s)) sch.sch_named;
    T.sch_body  = sch.sch_body
  }

let to_string name =
  match name with
  | NVar x | NOptionalVar x | NImplicit x | NMethod (_, x) -> x

let compare_owner own1 own2 =
  match own1, own2 with
  | MO_Arrow, MO_Arrow -> 0
  | MO_Arrow, _ -> -1
  | _, MO_Arrow -> 1

  | MO_Handler, MO_Handler -> 0
  | MO_Handler, _ -> -1
  | _, MO_Handler -> 1

  | MO_Label, MO_Label -> 0
  | MO_Label, _ -> -1
  | _, MO_Label -> 1

  | MO_TVar x1, MO_TVar x2 -> T.TVar.compare x1 x2

let compare name1 name2 =
  match name1, name2 with
  | (NVar x1 | NOptionalVar x1), 
    (NVar x2 | NOptionalVar x2)  -> String.compare x1 x2
  | (NVar _ | NOptionalVar _), _ -> -1
  | _, (NVar _ | NOptionalVar _) -> 1

  | NImplicit x1, NImplicit x2 -> String.compare x1 x2
  | NImplicit _, _ -> -1
  | _, NImplicit _ -> 1

  | NMethod (own1, x1), NMethod (own2, x2) ->
    let c = compare_owner own1 own2 in
    if c = 0 then String.compare x1 x2 else c

let equal name1 name2 = compare name1 name2 = 0

let scheme_uvars sch =
  sch |> tr_scheme |> T.Scheme.uvars

let scheme_of_type tp =
  { sch_targs = [];
    sch_named = [];
    sch_body  = tp
  }

module Ordered = struct
  type nonrec t = t
  let compare = compare
end

module Set = Set.Make(Ordered)
module Map = Map.Make(Ordered)
