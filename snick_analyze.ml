open Printf 
open Snick_ast

exception Syntax_error of string;;

let debugging = true;;

(* Show  some messages if debugging *)
let debugmsg msg = 
	if debugging then 
		print_string msg;
	;;

let get_expr_type expr =
	match expr with
	  | Estring (string, expr_type)-> expr_type
	  | Ebool (bool, expr_type)-> expr_type
	  | Eint (int, expr_type)-> expr_type
	  | Efloat (float, expr_type)-> expr_type
	  | Elval (lvalue, expr_type)-> expr_type
	  | Ebinop (expr1, binop, expr2, expr_type)-> expr_type
	  | Eunop (unop, expr, expr_type)-> expr_type
	  | Eparens (expr, expr_type)-> expr_type
	;;

(* Convert the expression type to string. For raise error and debugging only *)
let expr_type_tostring expr_type=
	match  expr_type with
	  | Expr_Bool -> "Bool"
	  | Expr_Int -> "Int"
	  | Expr_Float -> "Float"
	  | Expr_String -> "String"
	  | Expr_None -> "None"
	 ;;


let raise_type_mismatch expr1 expr2 = 
	raise(Syntax_error (sprintf "Type mismatch: %s and %s" 
				(expr_type_tostring (get_expr_type expr1))
				(expr_type_tostring (get_expr_type expr2))
				)
			); 
;;

let raise_invalid_arraysize min max =
	raise(Syntax_error (sprintf "Invalid Array Size: %d and %d" min max));
;;

let get_op_type op =
	match op with
		(* Math operation *)
		|Op_add| Op_sub|Op_mul|Op_div |Op_lt|Op_gt|Op_lt_eq|Op_gt_eq-> Op_type_math
		|Op_and|Op_or|Op_eq |Op_not_eq -> Op_type_bool
	;;

(* let get_op_type op =
	match op with

		|Op_minus-> Op_type_math
		|Op_not-> Op_type_bool
	;; *)

let init_prog =
	 debugmsg "Initiated";
	;; 


let finalize_prog prog =
	debugmsg "Finalized"; 
	prog;
	;;


let check_expr expr  =
	let Ebinop (expr1,op,expr2,expr_type) = expr in
	let expr_type_final = ref Expr_Float in
	match get_op_type op with
		|Op_type_math -> 
			(* Check if they are int or float, if not then thow the error *)
			if (get_expr_type expr1 != Expr_Int && get_expr_type expr1 != Expr_Float) 
			|| (get_expr_type expr2 != Expr_Int && get_expr_type expr2 != Expr_Float) then
				raise_type_mismatch expr1 expr2;
(* 			(* If strict, not allow assigning float to in (strict should be true for Assigning only) *)
			if strict && (get_expr_type expr1 == Expr_Int && get_expr_type expr1 == Expr_Float) then 
				raise(Syntax_error (sprintf "Type mismatch" )); *)

			(* Set the expr type *)
			if (get_expr_type expr1 == Expr_Float || get_expr_type expr2 == Expr_Float) then
				expr_type_final :=  Expr_Float
			else
				expr_type_final := Expr_Int
			;

		|Op_type_bool -> 
			if get_expr_type expr1 != Expr_Bool || get_expr_type expr2 != Expr_Bool then
				raise_type_mismatch expr1 expr2;
			expr_type_final := Expr_Bool;
		;

	debugmsg ("Type =" ^ (expr_type_tostring !expr_type_final) );
	Ebinop (expr1,op,expr2,!expr_type_final);
	;;

let check_typedef_range ttypedef =
 	match ttypedef with
		| Array(datatype,identifier,range_list) -> 
			for i = 0 to List.length(range_list) -1  do 
				let range =(List.nth range_list i)in
				let (min,max) = range in
				debugmsg (sprintf "Array size %d %d\n" min max) ;
				if min < 0 || max <0 || min > max then
					raise_invalid_arraysize min max; 
			done;
			
		| _ -> debugmsg "Not an array\n"; 
		;
		ttypedef;
	;;

(* Assign expr_type for Eunop, just get the value from chile and put to parent*)
let assign_expr_unop eunop =
	let Eunop (op,expr,expr_type) = eunop in
	Eunop (op,expr,get_expr_type expr);
;;

let assign_expr_paren eparen =
	let Eparens (expr,expr_type) = eparen in
	expr_type = get_expr_type expr;
	Eparens (expr,get_expr_type expr);
;;

