(*
Author: Songpoom Rasdussadee 
Purpose: Snick_analyze is used for syntax checking e.g. type matching between expressions
as well as  storing procedures and their parameters in the appropriate symbol tables so that 
the Snick_codegen  can use it for generateing the IR code
*)
open Printf 
open Snick_ast

exception Syntax_error of string;;
exception Dev_error of string;;

let raise_syn_err msg = 
	raise(Syntax_error  (msg) );
;;

let raise_dev_err msg = 
	raise(Dev_error  (msg) );
;;


(* ----Symbol table---- *)
let hash_size = 1024;;

let init_main_tbl  =
	let symbol_tbl = Hashtbl.create hash_size in
	let dummy_data = Invoke_symbol(InvokeProc("p",[Elval(LId("n",Expr_None),Expr_None)]) )in 
	(* Define type by adding a value as a dummy, then remove it (if don't do this the Ocaml compiler won't compile this) *)
	Hashtbl.add symbol_tbl "dummy" dummy_data ;
	Hashtbl.remove symbol_tbl "dummy" ;
	let main_sym_tbl =  Hashtbl.create hash_size in
	Hashtbl.add main_sym_tbl "dummy" symbol_tbl;
	Hashtbl.remove main_sym_tbl "dummy" ;
	main_sym_tbl;
;;

let add_tbl main_sym_tbl tbl_name =
	let symbol_tbl = Hashtbl.create hash_size in
	Hashtbl.add main_sym_tbl tbl_name symbol_tbl;
;;

let insert_symbol_to_tbl main_sym_tbl tbl_name id symbol_object =
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	Hashtbl.add target_tbl id symbol_object;
;;

let exist_symbol main_sym_tbl tbl_name id  =
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	Hashtbl.mem target_tbl id;
;;

let find_symbol main_sym_tbl tbl_name id  =
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	Hashtbl.find target_tbl id;
;;

let get_list main_sym_tbl tbl_name = 
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	let a = ref [] in
	let f s elem =
	    a:=List.append !a [(s,elem)];
	    in
	    Hashtbl.iter f target_tbl;
	!a;
;;

(* ----END Symbol table---- *)


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
	  	| Float -> Expr_Float
;;

let get_expr_type_for_typedef typedef =
	match typedef with
		|Single (datatype,id) -> get_expr_type_for_datatype datatype
		|Array (datatype,id,ranges) -> get_expr_type_for_datatype datatype
;;

let get_param_from_sym_tbl param =
	match param with
	| Param_symbol parameter_def-> parameter_def;
	| _ -> raise_dev_err "get_param_from_sym_tbl : type is not param"; 
;;

let get_proc_from_sym_tbl param =
	match param with
	| Proc_symbol proc-> proc;
	| _ -> raise_dev_err "get_proc_from_sym_tbl : type is not proc"; 
;;

let get_invoke_from_sym_tbl invoke =
	match invoke with
	| Invoke_symbol stmt-> 
		match stmt with
		| InvokeProc (identifier , expr_list) -> InvokeProc (identifier , expr_list) ;
		| _ -> raise_dev_err "get_invoke_from_sym_tbl : stmt type is not invoke"; 
		;
	| _ -> raise_dev_err "get_invoke_from_sym_tbl : type is not invoke"; 
;;


let get_expr_type_for_param param =
	let (reftype, typedef,expr_type) = param in
	get_expr_type_for_typedef typedef;
;;

let get_expr_type_for_lvalue lvalue =
	match lvalue with
	| LId(id,expr_type) -> expr_type
  	| LArrayElement (id , expr_list,expr_type) ->expr_type
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
	| LId(id,_) -> id
  	| LArrayElement (id , expr_list,_) ->id
;;

let get_rvalue_expr rvalue =
	let Rexpr(expr) = rvalue in
	expr;
;;

let get_ranges_from_param param =
	let (passby , type_def , expr_type)= param in
	match type_def with
	|Array (datatype,id,ranges) -> List.rev ranges;
	| _ -> [];
;;

let is_lvalue_array lvalue =
	match lvalue with
	| LId(id,_) -> false
  	| LArrayElement (id , expr_list,_) ->true
;;
let is_param_array param =
	let (reftype, typedef,expr_type) = param in
	match typedef with
	| Single  (datatype , identifier) ->false
  	| Array  (datatype, identifier , range_list)->true
;;


(* Get the type of operation for checking expr type, see the comment in snick_ast for more details*)
let get_op_type op =
	match op with
		|Op_add | Op_sub |Op_mul |Op_div -> Op_type_math_to_math
		|Op_lt |Op_gt |Op_lt_eq |Op_gt_eq-> Op_type_math_to_bool
		|Op_and |Op_or  -> Op_type_bool_to_bool
		|Op_eq |Op_not_eq -> Op_type_both_to_bool
;;


(* ----END Type management---- *)

(* ----Error raising----*)

let raise_expr_type_mismatch expr1 expr2 = 
	raise_syn_err (sprintf "Expression Type Mismatch: %s and %s" 
				(expr_type_tostring (get_expr_type_for_expr expr1))
				(expr_type_tostring (get_expr_type_for_expr expr2))
			); 
;;

let raise_assign_type_mismatch expr_type1 expr_type2 = 
	raise_syn_err (sprintf "Assigning Type Mismatch: cannot assign %s to %s" 
				(expr_type_tostring expr_type2)
				(expr_type_tostring expr_type1)
			); 
;;

let raise_invok_param_type_mismatch invokeid expr_type1 expr_type2 = 
	raise_syn_err (sprintf "The formal and actual parameter type mismatch for procedure '%s'.Expected %s but the actual is %s" 
				invokeid
				(expr_type_tostring expr_type1)
				(expr_type_tostring expr_type2)
			); 
;;

let raise_invalid_arraysize min max =
	raise_syn_err (sprintf "Invalid Array Size: %d and %d" min max);
;;

let raise_need_index id =
	raise_syn_err (sprintf "'%s' needs an index" id);
;;

let raise_no_need_index id =
	raise_syn_err (sprintf "'%s' does not need an index" id);
;;

let raise_out_of_bound min max index=
	raise_syn_err (sprintf "Index out of bound, possible values are %d and %d but the index is %d"
			 min max index);
;;

let raise_num_of_param_mismatch id expected actual =
	raise_syn_err (sprintf "Number of params for procedure '%s' does not match. The expected is %d but the actual is %d"
			id expected actual);
;;

let raise_num_of_indices_mismatch expected actual =
	raise_syn_err (sprintf "Number of indices does not match. The expected is %d but the actual is %d" 
			expected actual);
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
	raise_syn_err (sprintf "The expected expression type must be Bool but the current expression is %s" 
		(expr_type_tostring (get_expr_type_for_expr expr)) );
;;

let raise_expect_int expr =
	raise_syn_err (sprintf "The expected expression type must be Int but the current expression is %s" 
		(expr_type_tostring (get_expr_type_for_expr expr)) );
;;

let raise_main_mustnothave_params dummy =
	raise_syn_err (sprintf "The 'main' procedure must not contain any parameters");
;;

(* ----END Error raising----*)

(* ----Symbol table management----*)

(*For keeping the current parsing procedure name (we define current_tblname = procedure name)*)
(*It changes every time we read a new procedure*)
let current_tblname = ref "";;
let main_tbl = init_main_tbl;;

let get_tblname tbl_type =
	match tbl_type with
	  | Proc -> "proc"
	  | Invoke -> "-__invoke__-" (*The name should be unique*)
	  | Current -> !current_tblname
;;

let set_current_tbl tblname =
	current_tblname := tblname;
;;

(* Three possible object types, Param, Proc, Invoke *)
let insert_symbol tbl_type id obj =
	insert_symbol_to_tbl main_tbl (get_tblname tbl_type) id obj;
	(* (); *)
;;

(* ----END Symbol table management----*)

(* ----Utility----*)

let get_symbol tbl_type id =
	find_symbol main_tbl (get_tblname tbl_type) id;
;;

let get_symbol_list tbl_type =
	(get_list main_tbl (get_tblname tbl_type));
;;


let check_exist tbl_type id =
	let sym = exist_symbol main_tbl (get_tblname tbl_type) id in

	if not sym then
		raise_not_exist id;
;;

let check_not_exist tbl_type id =
	let sym = exist_symbol main_tbl (get_tblname tbl_type) id in

	if sym then
		raise_already_exist id;
;;

(* Check type and trhow type mis match error base on the assign_or_param value 
	0 for assign
	1 for param checking*)
let check_lhs_rhs_type_match id lexpr_type rexpr_type assign_or_param =
	if (lexpr_type == Expr_Int && rexpr_type == Expr_Float) 
	||(lexpr_type == Expr_Bool && rexpr_type != Expr_Bool) 
	||(lexpr_type != Expr_Bool && rexpr_type == Expr_Bool) then 

		if assign_or_param == 0 then
			raise_assign_type_mismatch lexpr_type rexpr_type
		else
			raise_invok_param_type_mismatch id lexpr_type rexpr_type
		;
;;

(* ----END Utility----*)


(* ----Functions for parser----*)

(* Call before parsing, prepare the symbol tables (main and invoke)*)
let init_prog =
	 debugmsg "Initiated";
	 (* Add symbol tbls for Proc and invoke *)

	 add_tbl main_tbl (get_tblname Proc);
	 add_tbl main_tbl (get_tblname Invoke);

	 debugmsg "Initiated success";
;; 

(* The last function to run for checking "main" procedure and checking invoked procedures*)
let finalize_prog prog =
	debugmsg "Finalizing prog\n";
	(* Check if "main" proc exist *)
	check_exist Proc "main";

	(* 1. if main exist, check param*)

	let main = (get_symbol Proc "main") in
	let (id,params,proc_body) = get_proc_from_sym_tbl main in
	if List.length (params) >0 then
		raise_main_mustnothave_params "";

	(*2. Check if the invoked procs is valid *)
	(* get all ivoked procedure *)
	let invoke_list = (get_symbol_list Invoke) in
	let  num_of_invoke=  List.length(invoke_list) in
	debugmsg (sprintf "Num of func %d\n" num_of_invoke);

	for i = 0 to List.length(invoke_list) - 1  do 
		debugmsg (sprintf "check proc index %d\n" i);
 		let (invoke_sym_id,invoke_sim_object) = (List.nth invoke_list i) in
 		match  get_invoke_from_sym_tbl invoke_sim_object with

		|InvokeProc (invoke_id, invoke_exprs) ->
			(*2.1 Check id invoked proc is exist *)
			check_exist Proc invoke_id;

			(* 2.2 check size of params is matched  *)
			let target_proc =get_proc_from_sym_tbl (get_symbol Proc invoke_id) in
			let ( _, formal_params, _ ) = target_proc in
			let proc_param_length =List.length(formal_params) in 
			let invoke_param_length =List.length(invoke_exprs) in 
			
			if proc_param_length != invoke_param_length then
				raise_num_of_param_mismatch invoke_id proc_param_length invoke_param_length;

			(* 2.3 check Type of expr and formal param *)
			for j = 0 to List.length(formal_params) - 1  do 
				let formal_param_type = (get_expr_type_for_param (List.nth formal_params j)) in
				let invoke_expr_type = (get_expr_type_for_expr  (List.nth invoke_exprs j)) in

				check_lhs_rhs_type_match invoke_id formal_param_type invoke_expr_type 1;
			done; 
		| _ -> raise_dev_err "invalid match";

	done;

	debugmsg "Finalized"; 
	prog;
;;

let add_proc proc_id=
	debugmsg "Adding Proc\n";

	check_not_exist Proc proc_id;
	debugmsg ("Insert new proc " ^ proc_id ^ "\n");

	(* Add the new sym table for this proc*)
	add_tbl main_tbl proc_id ;

	(* Set  this proc to the current proc*)
	set_current_tbl proc_id;

	debugmsg "Adding Proc success\n";
;;
(* Run check proc before if parse other part *)
let check_proc proc =
	let proc_id = (get_proc_id proc) in
	debugmsg "Checking Proc";


	(* insert this proc info to the Proc table*)
	insert_symbol Proc proc_id (Proc_symbol proc);

	debugmsg "Checking Proc success";
	proc;
;;	

let check_assign assign =
	(* similar to  check_expr_op but a little simpler*)

 	match assign with
	| Assign (lvalue, rvalue) -> 
		let lid = (get_lvalue_id lvalue) in
		let param = get_param_from_sym_tbl (get_symbol Current lid) in
		let lexpr_type =  get_expr_type_for_param  param in
		let rexpr_type = get_expr_type_for_expr (get_rvalue_expr rvalue) in
		(*check if param is exist*)
		check_exist Current lid;
		(* Check type match*)
		(* Possible invlid assign
			-float cannot be assigned to int 
			-LHS is bool but RHS is not bool and vice versa*)
		check_lhs_rhs_type_match lid lexpr_type rexpr_type 0;

	| _ -> debugmsg "Invalid Assign\n"; 
	;
	assign; 

;;

let check_invoke invoke =
	(* We can't really check the invoked procedure since all procedures have not been declard yet.
	 All we can do is the save them in the symbol table and wait until we have already read all procedures
	  then we can check this *)
	match invoke with
	| InvokeProc (id, exprs) ->
		debugmsg ("Insert invoked proc " ^ id ^ "\n");
		insert_symbol Invoke id (Invoke_symbol invoke);
		invoke;

	|_ -> invoke;
;;

let check_lvalue lvalue =
	let lid =  (get_lvalue_id lvalue) in 
	debugmsg ("Checking lvalue " ^ lid ^"\n");
	check_exist Current lid;

	(*assign expr_type*)
	let param = get_param_from_sym_tbl (get_symbol Current lid) in
	let paramtype = get_expr_type_for_param param in
	debugmsg ("Lvalue type " ^ (expr_type_tostring paramtype) ^"\n");

	(* Both must have a mathced type, array or single *)
	(* if the decleared type is an array, the lvalue must also be id with index *)
	if (is_param_array param) && not (is_lvalue_array lvalue) then
			raise_need_index lid;
	if  not (is_param_array param) &&  (is_lvalue_array lvalue) then
		raise_no_need_index lid;

	
	match lvalue with
  	| LArrayElement (id , expr_list, _) ->
		(* Get current param arraysize *)
		let formal_param = get_param_from_sym_tbl (get_symbol Current id) in 
		let ranges =  get_ranges_from_param  formal_param in
		let num_of_lval_indices = List.length(expr_list) in
		let num_of_param_indices = List.length(ranges) in

		(* Check number of indices *)
		if num_of_lval_indices !=num_of_param_indices then
			raise_num_of_indices_mismatch num_of_param_indices num_of_lval_indices ;

		
		for i = 0 to List.length(expr_list) - 1  do 
			let expr = (List.nth expr_list i) in
			(* check if the expr_type of all exprs must be int only*)
			if (get_expr_type_for_expr expr) != Expr_Int then
				raise_expect_int expr; 
				
			(*If expr is an int constamt, check out of bound*)
			match expr with
			| Eint (index,_) ->
				let (min,max) = (List.nth ranges i) in
				if index < min || index > max then
					raise_out_of_bound min max index;
			| _ ->  ();

		done;

		LArrayElement (id , expr_list, paramtype) ;
	| LId (id,_) ->  LId (id,paramtype);
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
		let optype = ref ( get_op_type op) in
		let expr_type1 = (get_expr_type_for_expr expr1) in
		let expr_type2 = (get_expr_type_for_expr expr2) in
		(* if optype is both to bool (the = or != op), we have to decide
		 whether it is Op_type_bool_to_bool or Op_type_math_to_bool *)
		if !optype  = Op_type_both_to_bool then
			if expr_type1 == Expr_Int || expr_type1 == Expr_Float then
				optype := Op_type_math_to_bool
			else
				 if expr_type1 == Expr_Bool then
					optype := Op_type_bool_to_bool
		;

		match !optype with
		| Op_type_math_to_math | Op_type_math_to_bool -> 
			(* Check if they are int or float, if not then thow the error *)
			if (expr_type1 != Expr_Int && expr_type1 != Expr_Float) 
			|| (expr_type2 != Expr_Int && expr_type2 != Expr_Float) then
				raise_expr_type_mismatch expr1 expr2;

			(* Check divide by zero *)
			if op == Op_div then
				match expr2 with
				| Eint (int, expr_type)-> if int == 0 then raise_zero_division "";
				| Efloat (float, expr_type)-> if float == 0.0 then raise_zero_division "";
				| _ -> ();
	  		;

	  		(* Set the expr type for the parent expr*)
	  		match !optype with
	  		| Op_type_math_to_math ->
				(* Set the expr type, if any is float then the parent is also float *)
				if (expr_type1 == Expr_Float || expr_type2 == Expr_Float) then
					Ebinop (expr1,op,expr2,Expr_Float)
				else
					Ebinop (expr1,op,expr2,Expr_Int)
				;
			| Op_type_math_to_bool ->
				(* for some operations such as lt ,gt the result must be bool*)
				Ebinop (expr1,op,expr2,Expr_Bool);

			| _ -> Ebinop (expr1,op,expr2,Expr_None);
			;

		| Op_type_bool_to_bool -> 
			if expr_type1 != Expr_Bool || expr_type2!= Expr_Bool then
				raise_expr_type_mismatch expr1 expr2;
			Ebinop (expr1,op,expr2,Expr_Bool);
		| _ -> raise_dev_err "Invalid op type match";
		;

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
	debugmsg "Checking param\n"; 
	check_not_exist Current id;
	(* save to symbol table*)
	insert_symbol Current id (Param_symbol(reftype, typedef, get_expr_type_for_typedef typedef) );
	debugmsg (sprintf "Param Type %s\n" ( expr_type_tostring ( get_expr_type_for_typedef typedef) ) ); 
	debugmsg "Checking param success\n"; 
	(reftype, typedef, get_expr_type_for_typedef typedef);
;;

let check_dec dec =
	let (typedef,expr_type) = dec in
	let id = (get_typedef_id typedef) in
	let final_expr_type = get_expr_type_for_typedef typedef in
	debugmsg (sprintf "Checking dec:%s\n" id); 
	check_not_exist Current id;
	(* Check array size *)
	check_typedef_range typedef;
	(* cast to param and save to symbol table*)
	insert_symbol Current id (Param_symbol(Value, typedef,final_expr_type));
	debugmsg (sprintf "Dec Type %s\n" ( expr_type_tostring ( final_expr_type) ) ); 
	debugmsg "Checking dec success\n"; 
	(typedef, get_expr_type_for_typedef typedef);
;;

(* Assign expr_type for Eunop, just get the value from child and put to parent*)
let assign_expr eunop =
	match eunop with
	 | Eunop (op,expr,expr_type) -> Eunop (op,expr,get_expr_type_for_expr expr);
	 | Eparens (expr,expr_type) -> Eparens (expr,get_expr_type_for_expr expr);
	 | Elval (lvalue,expr_type) -> Elval (lvalue,get_expr_type_for_lvalue lvalue);
	 | _ -> eunop ;
;;

(* ----END Functions for parser----*)





