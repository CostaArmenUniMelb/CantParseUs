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

let get_expr_type_for_expr expr =
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

let get_expr_type_for_param param =
	let (reftype, typedef,expr_type) = param in
	get_expr_type_for_typedef typedef;
;;

let get_typedef_id typedef =
	match typedef with
		|Single (datatype,id) -> id
		|Array (datatype,id,ranges) -> id
;;

let get_proc_id proc =
	let (identifier , parameter_def_list , procedure_body) = proc in
	identifier;
;;

let get_lvalue_id lvalue =
	match lvalue with
	| LId(id) -> id
  	| LArrayElement (id , expr_list) ->id
;;

let get_rvalue_expr rvalue =
	let Rexpr(expr) = rvalue in
	expr;
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

let raise_syn_err msg = 
	raise(Syntax_error  (msg) );
;;


let raise_type_mismatch expr1 expr2 = 
	raise_syn_err (sprintf "Expression Type Mismatch: %s and %s" 
				(expr_type_tostring (get_expr_type_for_expr expr1))
				(expr_type_tostring (get_expr_type_for_expr expr2))
			); 
;;

let raise_assign_type_mismatch expr_type1 expr_type2 = 
	raise_syn_err (sprintf "Assigning Type Mismatch: %s and %s" 
				(expr_type_tostring expr_type1)
				(expr_type_tostring expr_type2)
			); 
;;

let raise_param_type_mismatch expr_type1 expr_type2 = 
	raise_syn_err (sprintf "The formal and actual parameter type Mismatch: %s and %s" 
				(expr_type_tostring expr_type1)
				(expr_type_tostring expr_type2)
			); 
;;

let raise_invalid_arraysize min max =
	raise_syn_err (sprintf "Invalid Array Size: %d and %d" min max);
;;

let raise_out_of_bound min max index=
	raise_syn_err (sprintf "Index out of bound, possible values is %d and %d 
						but the index is %d" min max index);
;;

let raise_num_of_param_mismatch expected actual =
	raise_syn_err (sprintf "Number of params does not match. The expected is %d but the actual is %d" expected actual);
;;

let raise_not_exist id =
	raise_syn_err (sprintf "'%s' does not exist" id);
;;

let raise_already_exist id =
	raise_syn_err (sprintf "'%s' has been declared" id);
;;

let raise_zero_division dummy =
	raise_syn_err (sprintf "Cannot divide by zero");
;;

let raise_expect_bool expr =
	raise_syn_err (sprintf "The expected expression type must be Bool
						but the current expression is %s" (expr_type_tostring (get_expr_type_for_expr expr)) );
;;

let raise_expect_int expr =
	raise_syn_err (sprintf "The expected expression type must be Int 
						but the current expression is %s" (expr_type_tostring (get_expr_type_for_expr expr)) );
;;

let raise_main_mustnothave_params dummy =
	raise_syn_err (sprintf "The 'main' procedure must not contain any parameters");
;;

(* ----END Error raising----*)

(* ----Symbol table management----*)

(*For keeping the current parsing procedure name (we define current_tblname = procedure name)*)
(*It changes every time we read a new procedure*)
let current_tblname = ref "";;

let get_tblname tbl_type =
	match tbl_type with
	  | Proc -> "proc"
	  | Invoke -> "-__invoke__-" (*The name should be unique*)
	  | Current -> !current_tblname
;;

let set_current_tbl tblname =
	current_tblname := tblname;
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

let get_param_type_from_tbl tbl_type id =
	Expr_None;
;;

(* ----END Symbol table management----*)

(* ----Utility----*)
(* Check type and trhow type mis match error base on the assign_or_param value 
	0 for assign
	1 for param checking*)
let check_lhs_rhs_type_match lexpr_type rexpr_type assign_or_param =
	if (lexpr_type == Expr_Int && rexpr_type == Expr_Float) 
	||(lexpr_type == Expr_Bool && rexpr_type != Expr_Bool) 
	||(lexpr_type != Expr_Bool && rexpr_type == Expr_Bool) then 

		if assign_or_param == 0 then
			raise_assign_type_mismatch lexpr_type rexpr_type
		else
			raise_param_type_mismatch lexpr_type rexpr_type
		;
;;

(* ----END Utility----*)


(* ----Functions for parser----*)

(* Call before parsing, prepare the symbol tables (main and invoke)*)
let init_prog =
	 debugmsg "Initiated";
	 (* Add symbol tbls for Proc and invoke *)

(* 	 Snick_symbol.add_table (get_tblname Proc);
	 Snick_symbol.add_table (get_tblname Invoke); *)
;; 

(* The last function to run for checking "main" procedure and checking invoked procedures*)
let finalize_prog prog =
	(* Check if "main" proc exist *)
	check_exist Proc "main";
	(* 1. if main exist, check param*)

(* 	let main = (Snick_symbol.find Proc "main") in
	let (id,params,proc_body) = main in
	if List.length (params) >0 then
		raise_main_mustnothave_params ""; *)

	(*2. Check if the invoked procs is valid *)


	(* let invoke_list = (Snick_symbol.get_list Proc) in
	for i = 0 to List.length(invoke_list) - 1  do 
		let invoke = (List.nth invoke_list i) in
		let InvokeProc (invoke_id, invoke_exprs) = invoke in

		(*2.1 Check id invoked proc is exist *)
		check_exist Proc invoke_id;

		(*2.2 check size of params is matched *)
		let target_proc = Snick_symbol.find Proc invoke_id in
		let ( _, formal_params, _ ) = target_proc in
		
		if List.length(formal_params) != List.length(invoke_exprs) then
			raise_num_of_param_mismatch List.length(formal_params) List.length(invoke_exprs);

		(* 2.3 check Type of expr and formal param *)
		for j = 0 to List.length(formal_params) - 1  do 
			let formal_param_type = (get_expr_type_for_param (List.nth formal_params j)) in
			let invoke_expr_type = (get_expr_type_for_expr  (List.nth invoke_exprs j)) in

			check_lhs_rhs_type_match formal_param_type invoke_expr_type 1;
		done;

	done; *)

	debugmsg "Finalized"; 
	prog;
;;

let check_proc proc =
	let proc_id = (get_proc_id proc) in
	check_not_exist Proc proc_id;
	debugmsg ("Insert new proc " ^ proc_id ^ "\n");

	(* Add the new sym table for this proc*)
	(* Snick_symbol.add_table proc_id ; *)

	(* Set  this proc to the current proc*)
	set_current_tbl proc_id;

	(* insert this proc info to the Proc table*)
	insert_symbol Proc proc_id proc;
	proc;
;;

let check_assign assign =
	(* similar to  check_expr_op but a little simpler*)
	match assign with
	| Assign (lvalue, rvalue) -> 
		let lid = (get_lvalue_id lvalue) in
		let lexpr_type = (get_param_type_from_tbl Current lid) in
		let rexpr_type = (get_expr_type_for_expr (get_rvalue_expr rvalue)) in
		(*check if param is exist*)
		check_exist Current lid;
		(* Check type match*)
		(* Possible invlid assign
			-float cannot be assigned to int 
			-LHS is bool but RHS is not bool and vice versa*)
		check_lhs_rhs_type_match lexpr_type rexpr_type 0;

		assign;
	| _ -> debugmsg "Invalid Assign\n"; assign;
;;

let check_invoke invoke =
	(* We can't really check the invoked procedure since all procedures have not been declard yet.
	 All we can do is the save them in the symbol table and wait until we have already read all procedures
	  then we can check this *)
	match invoke with
	| InvokeProc (id, exprs) ->
		debugmsg ("Insert invoked proc " ^ id ^ "\n");
		insert_symbol Invoke id invoke;
		invoke;

	|_ -> invoke;
;;

let check_lvalue lvalue =
	check_exist Current (get_lvalue_id lvalue);
	(* check if the input is array with exprs, the expr_type of all exprs must be int only*)
	match lvalue with
  	| LArrayElement (id , expr_list) ->
  		for i = 0 to List.length(expr_list) -1  do 
			let expr = (List.nth expr_list i) in
			if (get_expr_type_for_expr expr) != Expr_Int then
				raise_expect_int expr; 
		done;
		lvalue;
	| _ -> debugmsg "Not an array\n"; lvalue;
;;

(* Check if the expr has the type bool, used in IF and While *)
let check_expr_bool expr  =
	if get_expr_type_for_expr expr != Expr_Bool then
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
				if (get_expr_type_for_expr expr1 != Expr_Int && get_expr_type_for_expr expr1 != Expr_Float) 
				|| (get_expr_type_for_expr expr2 != Expr_Int && get_expr_type_for_expr expr2 != Expr_Float) then
					raise_type_mismatch expr1 expr2;

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
						if (get_expr_type_for_expr expr1 == Expr_Float || get_expr_type_for_expr expr2 == Expr_Float) then
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
				if get_expr_type_for_expr expr1 != Expr_Bool || get_expr_type_for_expr expr2 != Expr_Bool then
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
	let (reftype, typedef,expr_type) = param in
	let id = (get_typedef_id typedef) in
	check_not_exist Current id;
	(* save to symbol table*)
	insert_symbol Current id (reftype, typedef, get_expr_type_for_typedef typedef);

	(reftype, typedef, get_expr_type_for_typedef typedef);
;;

let check_dec dec =
	let (typedef,expr_type) = dec in
	let id = (get_typedef_id typedef) in
	check_not_exist Current id;
	(* Check array size *)
	check_typedef_range typedef;
	(* cast to param and save to symbol table*)
	insert_symbol Current id (Value, typedef,get_expr_type_for_typedef typedef);
	(* return dec*)
	(typedef, get_expr_type_for_typedef typedef);
;;

(* Assign expr_type for Eunop, just get the value from child and put to parent*)
let assign_expr eunop =
	match eunop with
	 | Eunop (op,expr,expr_type) -> Eunop (op,expr,get_expr_type_for_expr expr);
	 | Eparens (expr,expr_type) -> Eparens (expr,get_expr_type_for_expr expr);
	 | _ -> eunop ;
;;

(* ----END Functions for parser----*)

