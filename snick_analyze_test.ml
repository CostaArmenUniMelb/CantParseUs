open Snick_analyze
open Snick_ast

let _ =

	let typedef2 = Array(Bool,"a",[(1,8); (2,4)]) in
	Snick_analyze.check_typedef_range typedef2;;

	let typedef2 = Array(Bool,"a",[(1,8); (2,4)]) in
	Snick_analyze.check_typedef_range typedef2;;
	try  
		let typedef3 = Array(Bool,"a",[(1,8); (-3,4)]) in
		Snick_analyze.check_typedef_range typedef3;
		();
	with
		| _ as err -> let msg = Printexc.to_string err in print_string msg; 
	;;

	let exp1 = Eint ( 5, Expr_Int) in
	let exp2 = Eint ( 20, Expr_Int) in
	let exp_parent = Ebinop (exp1, Op_add, exp2, Expr_None) in
	Snick_analyze.check_expr_op exp_parent ;;

	let exp1 = Efloat ( 5.0, Expr_Float) in
	let exp2 = Eint ( 20, Expr_Int) in
	let exp_parent =  Ebinop (exp1, Op_add, exp2, Expr_None) in
	Snick_analyze.check_expr_op exp_parent ;;

	try  
		let exp1 = Ebool ( true, Expr_Bool) in
		let exp2 = Eint ( 20, Expr_Int) in
		let exp_parent =  Ebinop(exp1, Op_add, exp2, Expr_None) in
		Snick_analyze.check_expr_op exp_parent ;
		();
	with
		| _ as err -> let msg = Printexc.to_string err in print_string msg; 
	;;


		

