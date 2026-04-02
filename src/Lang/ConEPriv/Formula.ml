(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Internal representation of formulas of a logic enriched with effect
  modes.

Each formula is represented as a pair of formulas: one that says that
the effect contains non-affine component, and one that says that the
effect contains affine component. Since in Fram it is not possible to
express effects with only non-affine component, the first formula always
implies the second one. *)

type t =
  { non_affine : IncrSAT.Formula.t;
    (** Non-affine part of the formula. *)

    affine     : IncrSAT.Formula.t;
    (** Affine part of the formula. *)
  }

let bot =
  { non_affine = IncrSAT.Formula.bot;
    affine     = IncrSAT.Formula.bot;
  }

let top =
  { non_affine = IncrSAT.Formula.top;
    affine     = IncrSAT.Formula.top;
  }

let affine_f =
  { non_affine = IncrSAT.Formula.bot;
    affine     = IncrSAT.Formula.top;
  }

let of_mode (mode : EffectMode.t) =
  match mode with
  | Unrestricted -> top
  | Affine       -> affine_f

let fresh_var () =
  let p_unr = IncrSAT.Formula.fresh_var () in
  let p_aff = IncrSAT.Formula.fresh_var () in
  { non_affine = p_unr;
    affine     = IncrSAT.Formula.disj p_unr p_aff;
  }

let conj p1 p2 =
  { non_affine = IncrSAT.Formula.conj p1.non_affine p2.non_affine;
    affine     = IncrSAT.Formula.conj p1.affine     p2.affine;
  }

let disj p1 p2 =
  { non_affine = IncrSAT.Formula.disj p1.non_affine p2.non_affine;
    affine     = IncrSAT.Formula.disj p1.affine     p2.affine;
  }

let is_true p =
  IncrSAT.Formula.is_true p.non_affine &&
  IncrSAT.Formula.is_true p.affine

let is_false p =
  IncrSAT.Formula.is_false p.non_affine &&
  IncrSAT.Formula.is_false p.affine

let implies p1 p2 =
  IncrSAT.Formula.implies p1.non_affine p2.non_affine &&
  IncrSAT.Formula.implies p1.affine     p2.affine

let fix p =
  IncrSAT.Formula.fix p.non_affine ||
  IncrSAT.Formula.fix p.affine

let imp_to_cnf p1 p2 =
  IncrSAT.Formula.imp_to_cnf p1.non_affine p2.non_affine @
  IncrSAT.Formula.imp_to_cnf p1.affine     p2.affine

let to_sexpr p =
  if not (IncrSAT.Formula.is_false p.non_affine) then
    SExpr.List
      [ IncrSAT.Formula.to_sexpr p.non_affine;
        Sym "affine";
        IncrSAT.Formula.to_sexpr p.affine
      ]
  else if IncrSAT.Formula.is_true p.affine then
    SExpr.Sym "affine"
  else
    SExpr.List
      [ Sym "affine";
        IncrSAT.Formula.to_sexpr p.affine
      ]

(** Description of a fragment of the mode lattice used for pretty-printing.
  The description may contain only part of the lattice, with some modes
  collapsed to [Unrestricted], because the effect variable may be implicitly
  projected to a given mode. *)
type mode_tree =
  | Bot
  | Cons of EffectMode.t * IncrSAT.Formula.t * mode_tree
  | Join of mode_tree * mode_tree

let rec to_mode_list_aux tree =
  match tree with
  | Bot -> []
  | Cons(mode, f, rest) when IncrSAT.Formula.is_false f ->
    to_mode_list_aux rest
  | Cons(mode, f, _)    when IncrSAT.Formula.is_true f ->
    [(mode, true)]
  | Cons(mode, f, rest) ->
    (mode, false) :: to_mode_list_aux rest
  | Join(tree1, tree2) ->
    to_mode_list_aux tree1 @ to_mode_list_aux tree2

let to_mode_list ?(mode=EffectMode.Unrestricted) p =
  let open EffectMode in
  to_mode_list_aux
    (match mode with
    | Unrestricted ->
      Cons(Unrestricted, p.non_affine,
        Cons(Affine, p.affine, Bot))
    | Affine ->
      Cons(Unrestricted, p.affine, Bot))
