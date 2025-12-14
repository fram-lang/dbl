(*let make_nowhere data =
  { Lang.Surface.pos  = Position.nowhere
  ; Lang.Surface.data = data
  }


let process_type_directive expr =
	let e = 
		make_nowhere (Lang.Surface.DReplDir(Lang.Surface.Type_Directive(Desugar.tr_expr expr))) in
	let (body_env, params) = TypeInference.Env.begin_generalize env in
           let expr   = infer_expr_type body_env e in
            let cs     = ConstrSolve.solve_partial expr.er_constr in
            let tp     = expr_result_type expr in
      let ctx = Lang.Unif.Pretty.empty_context () in
      let tp1 = Lang.Unif.Pretty.pp_type ctx expr.er_expr.pp tp in 
      tp1
    
   

*)

