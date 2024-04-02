(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Scopes and operations on them *)

type t = {
  tvar_set : TVar.Set.t;
    (** Set of type variables allowed in this scope *)

  level    : int
    (** Level of the scope, i.e., the number which indicates how many places
      of implicit type generalization (let-definitions, generally) are
      available in this lexical scope. *)
}

let initial =
  { tvar_set = TVar.Set.empty;
    level    = 0
  }

let add scope x =
  { tvar_set = TVar.Set.add x scope.tvar_set;
    level    = scope.level
  }

let add_named scope (_, x) = add scope x

let filter lvl f scope =
  { tvar_set = TVar.Set.filter f scope.tvar_set;
    level    = min lvl scope.level
  }

let mem scope x = TVar.Set.mem x scope.tvar_set

let perm p scope =
  { tvar_set = TVar.Perm.map_set p scope.tvar_set;
    level    = scope.level
  }

let shrink_perm_dom scope p =
  TVar.Perm.shrink_dom scope.tvar_set p

let incr_level scope =
  { scope with level = scope.level + 1 }

let level scope = scope.level
