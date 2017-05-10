open Printf 
open Snick_ast

exception Syntax_error of string;;

(* ----Debugging---- *)
let debugging = true;;

(* Show  some messages for debugging *)
let debugmsg msg = 
	if debugging then 
		print_string msg;
;;
(* ----END Debugging---- *)

(* ----Type management---- *)
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

let get_expr_type_for_datatype datatype =
	match datatype with
		| Bool -> Expr_Bool
	  	| Int -> Expr_Int
	  	| Float -> Expr_String
;;

let get_expr_type_for_typedef typedef =
	match typedef with
		|Single (datatype,id) -> get_expr_type_for_datatype datatype
		|Array (datatype,id,ranges) -> get_expr_type_for_datatype datatype

;;

(* Get the type of operation for checking expr type, see the comment in snick_ast for more details*)
let get_op_type op =
	match op with
		|Op_add | Op_sub |Op_mul |Op_div -> Op_type_math
		|Op_lt |Op_gt |Op_lt_eq |Op_gt_eq-> Op_type_math_to_bool
		|Op_and |Op_or |Op_eq |Op_not_eq -> Op_type_bool
;;
(* ----END Type management---- *)

(* ----Error raising----*)
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

let raise_out_of_bound min max index=
	raise(Syntax_error (sprintf "Index out of bound, possible values is %d and %d 
						but the index is %d" min max index));
;;

let raise_not_exist id =
	raise(Syntax_error (sprintf "'%s' does not exist" id));
;;

let raise_already_exist id =
	raise(Syntax_error (sprintf "'%s' has been declared" id));
;;

let raise_zero_division dummy =
	raise(Syntax_error (sprintf "Cannot divide by zero"));
;;

let raise_expect_bool expr =
	raise(Syntax_error (sprintf "The expected expression type must be boolean 
						but the current expression is %s" (expr_type_tostring (get_expr_type expr)) ));
;;

let raise_no_main dummy =
	raise(Syntax_error (sprintf "No defined 'main' procedure"));
;;

let raise_main_mustnothave_params dummy =
	raise(Syntax_error (sprintf "The 'main' procedure must not contain any parameters"));
;;
(* ----END Error raising----*)

(* ----Symbol table management----*)
(*For keeping the current parsing procedure name (we define current_tblname = procedure name)*)
(*It changes every time we read a new procedure*)
let current_tblname = "";;

let get_tblname tbl_type =
	match tbl_type with
	  | Proc -> "-__proc__-"
	  | Invoke -> "-__invoke__-"
	  | Current -> current_tblname
;;

let add_tbl tbl_type =
	();
;;

let insert_symbol tbl_type id obj =
	();
;;

let check_exist tbl_type id =
	();
;;

let check_not_exist tbl_type id =
	();
;;

(* ----END Symbol table management----*)

(* ----Functions for parser----*)
(* Call before parsing, prepare the symbol tables (main and invoke)*)
let init_prog =
	 debugmsg "Initiated";
;; 

(* The last function to run for checking "main" procedure and checking invoked procedures*)
let finalize_prog prog =
	debugmsg "Finalized"; 
	prog;
;;

let check_proc proc =
	proc;
;;

let check_assign assign =
	(* similar to  check_expr_op but a little simpler*)
	assign;
;;

let check_invoke invoke =
	invoke;
;;

let check_lvalue lvalue =
	lvalue;
;;

(* Check if the expr has the type bool, used in IF and While *)
let check_expr_bool expr  =
	if get_expr_type expr != Expr_Bool then
		raise_expect_bool expr;
	expr;
;;

(* check and assign expr type for Ebinop operations *)
let check_expr_op expr  =
	match expr with
	| Ebinop (expr1,op,expr2,expr_type) -> 
		let expr_type_final = ref Expr_None in (* for assigning the expr_type for the parent expr*)
		match get_op_type op with
			| Op_type_math | Op_type_math_to_bool -> 
				(* Check if they are int or float, if not then thow the error *)
				if (get_expr_type expr1 != Expr_Int && get_expr_type expr1 != Expr_Float) 
				|| (get_expr_type expr2 != Expr_Int && get_expr_type expr2 != Expr_Float) then
					raise_type_mismatch expr1 expr2;
	(* 			(* If strict, not allow assigning float to in (strict should be true for Assigning only) *)
				if strict && (get_expr_type expr1 == Expr_Int && get_expr_type expr1 == Expr_Float) then 
					raise(Syntax_error (sprintf "Type mismatch" )); *)
				(* Check divide by zero *)
				if op == Op_div then
					match expr2 with
					  | Eint (int, expr_type)-> if int == 0 then raise_zero_division "";
					  | Efloat (float, expr_type)-> if float == 0.0 then raise_zero_division "";
					  | _ -> ();
		  		;

		  		(* set the expr type for the parent expr*)
		  		match get_op_type op with
			  		|Op_type_math ->
						(* Set the expr type, if any is float then the parent is also float *)
						if (get_expr_type expr1 == Expr_Float || get_expr_type expr2 == Expr_Float) then
							expr_type_final :=  Expr_Float
						else
							expr_type_final := Expr_Int
						;
					|Op_type_math_to_bool ->
						(* for some operations such as lt ,gt the result must be bool*)
						expr_type_final := Expr_Bool;
					|_ -> ();
				;

			| Op_type_bool -> 
				if get_expr_type expr1 != Expr_Bool || get_expr_type expr2 != Expr_Bool then
					raise_type_mismatch expr1 expr2;
				expr_type_final := Expr_Bool;
			;
		debugmsg ("Type =" ^ (expr_type_tostring !expr_type_final) );
		Ebinop (expr1,op,expr2,!expr_type_final);
	| _ -> expr;
;;

(* check whether the array size is valid when define*)
let check_typedef_range ttypedef =
 	match ttypedef with
		| Array (datatype,identifier,range_list) -> 
			for i = 0 to List.length(range_list) -1  do 
				let range =(List.nth range_list i)in
				let (min,max) = range in
				debugmsg (sprintf "Array size %d %d\n" min max) ;
				if min < 0 || max <0 || min > max then
					raise_invalid_arraysize min max; 
			done;
			
		| _ -> debugmsg "Not an array\n"; 
		;
;;

(* The differences between dec and param is 
the params can be REF or VAL while the dec is only VAL *)
let check_param param =
	let (reftype, typedef,expr_type) =param in
	(* check_typedef_range(typedef);  no chance tat param will be an array*)
	(* save to symbol table*)
	(reftype, typedef, get_expr_type_for_typedef typedef);
;;

let check_dec dec =
	let (typedef,expr_type) =dec in

	check_typedef_range(typedef);
	(* save to symbol table*)
	(typedef,get_expr_type_for_typedef typedef);
;;

(* Assign expr_type for Eunop, just get the value from child and put to parent*)
let assign_expr_unop eunop =
	match eunop with
	 | Eunop (op,expr,expr_type) -> Eunop (op,expr,get_expr_type expr);
	 | _ -> eunop ;
;;

let assign_expr_paren eparen =
	match eparen with
	| Eparens (expr,expr_type) -> Eparens (expr,get_expr_type expr);
	| _ -> eparen ;
;;

(* ----END Functions for parser----*)

