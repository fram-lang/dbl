(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Operations on constructor declarations *)

open TypeBase

let collect_gvars ~scope ctor gvs =
  let { ctor_name = _; ctor_targs = _; ctor_named; ctor_arg_schemes } = ctor
  in
  let gvs =
    List.fold_left
      (fun gvs (_, sch) -> Type.collect_scheme_gvars ~scope sch gvs)
      gvs
      ctor_named
  in
  List.fold_left
    (fun gvs sch -> Type.collect_scheme_gvars ~scope sch gvs)
    gvs
    ctor_arg_schemes

let subst = Subst.in_ctor_decl
