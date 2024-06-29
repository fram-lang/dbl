(* This file is part of DBL, released under MIT license.
 * See LICENSE for details.
 *)

(** Functions allowing the use of some of the types defined in the Prelude. *)

open Common

let mk_Option ~env ~pos tp_arg =
  match Env.lookup_tvar env (S.NPName "Option") with
  | None -> Error.fatal (Error.unbound_type_var ~pos (S.NPName "Option"))
  | Some tp ->
      if
        not
          (Unification.unify_kind (T.Type.kind tp)
             (T.Kind.k_arrow T.Kind.k_type T.Kind.k_type))
      then
        Error.fatal
          (Error.kind_mismatch ~pos (T.Type.kind tp)
             (T.Kind.k_arrow T.Kind.k_type T.Kind.k_type))
      else T.Type.t_app tp tp_arg

let option_to_whnf ~env ~pos option_tp =
  match T.Type.whnf option_tp with
  | Whnf_Neutral (NH_Var x, tp :: []) -> (tp, x)
  | _ -> Error.fatal (Error.invalid_whnf_form ~pos ~env option_tp)

let extr_arg_tp ~env ~pos option_tp =
  let tp, _ = option_to_whnf ~env ~pos option_tp in
  tp

let mk_Some ~env ~pos tp_arg expr_arg : T.expr =
  let make data pos = { T.data; T.pos } in
  let tp' = mk_Option ~env ~pos tp_arg in
  let _, x = option_to_whnf ~env ~pos tp' in
  match Env.lookup_adt env x with
  | Some adt_info -> (
      let { Module.adt_proof; adt_args; adt_ctors; adt_type } = adt_info in
      match
        List.find_index
          (fun (ctor_decl : T.ctor_decl) -> String.compare ctor_decl.ctor_name "Some" == 0)
          adt_ctors
      with
      | Some idx ->
          make
            (T.ECtor
               ( make (T.ETApp (adt_proof, tp_arg)) Position.nowhere,
                 idx,
                 [],
                 [ expr_arg ] ))
            Position.nowhere
      | None -> Error.fatal (Error.ctor_not_in_type ~pos ~env "Some" tp'))
  | None -> Error.fatal (Error.unbound_adt ~pos ~env x)

let mk_None ~env ~pos option_tp : T.expr =
  let make data pos = { T.data; T.pos } in
  let tp_arg, x = option_to_whnf ~env ~pos option_tp in
  match Env.lookup_adt env x with
  | Some adt_info -> (
      let { Module.adt_proof; adt_args; adt_ctors; adt_type } = adt_info in
      match
        List.find_index
          (fun (ctor_decl : T.ctor_decl) -> String.compare ctor_decl.ctor_name "None" == 0)
          adt_ctors
      with
      | Some idx ->
          make
            (T.ECtor
               (make (T.ETApp (adt_proof, tp_arg)) Position.nowhere, idx, [], []))
            Position.nowhere
      | None -> Error.fatal (Error.ctor_not_in_type ~pos ~env "None" option_tp))
  | None -> Error.fatal (Error.unbound_adt ~pos ~env x)
