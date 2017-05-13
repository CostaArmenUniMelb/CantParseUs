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

(* let init_main_tbl  =
	let symbol_tbl = Hashtbl.create 1024 in
	let dummy_data = Invoke_symbol(InvokeProc("p",[Elval(LId("n"),Expr_None)]) )in 
	(* Define type by adding a value as a dummy, then remove it (if don't do this the Ocaml compiler won't compile this) *)
	Hashtbl.add symbol_tbl "dummy" dummy_data ;
	Hashtbl.remove symbol_tbl "dummy" ;
	let main_sym_tbl =  Hashtbl.create 1024 in
	Hashtbl.add main_sym_tbl "dummy" symbol_tbl;
	Hashtbl.remove main_sym_tbl "dummy" ;
	main_sym_tbl;
;;

let add_tbl main_sym_tbl tbl_name =
	let my_hash = Hashtbl.create 1024 in
(* 	let dummy_data = Invoke_symbol(InvokeProc("p",[Elval(LId("n"),Expr_None)]) )in 

	Hashtbl.add my_hash "dummy" dummy_data ;
	Hashtbl.remove my_hash "dummy" ; *)

	Hashtbl.add main_sym_tbl tbl_name my_hash;
;;

let insert_symbol main_sym_tbl tbl_name id symbol_object =
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
	let a = [] in
	let f s elem =
	    (s,elem)::a;
	    (* Printf.printf "Sym_VAL element %s ,val %s\n" s elem; *)
	    ();
	    in
	    Hashtbl.iter f target_tbl;
	  a;
;;
 *)



let my_hash = init_main_tbl;;

add_tbl my_hash "xxx";;


let dummy_data2 = Invoke_symbol(InvokeProc("p",[Elval(LId("n"),Expr_None)]) );;
insert_symbol my_hash "xxx" "anid" dummy_data2;;

if exist_symbol my_hash "xxx" "anid" then Printf.printf "Exist\n";;

if exist_symbol my_hash "xxx" "should not exist" then Printf.printf "Exist\n" else Printf.printf "Not Exist\n";;

let found_data = find_symbol my_hash "xxx" "anid" ;;

if found_data==dummy_data2 then Printf.printf "EqUAL\n";;


let dummy_data3 = Invoke_symbol(InvokeProc("xxx",[Elval(LId("skdlkl"),Expr_None)]) );;
insert_symbol my_hash "xxx" "anid2" dummy_data3;;
let list_data = get_list my_hash "xxx" ;;

(* Printf.printf List.length(list_data) ;; *)



(* 	(* let ht2 = Hashtbl.create 4;; *)
	Hashtbl.add my_hash "xxx" "message" ;;
	Hashtbl.add my_hash "xxx2" "message2" ;;


let ht2 = Hashtbl.create 456;;

Hashtbl.add ht2 "mat" my_hash ;;

let xhash = Hashtbl.find ht2 "mat";;

print_string (Hashtbl.find xhash "xxx");;
(* let str_list = [];;
	List.append str_list "cc";; *)
let f s elem =

    Printf.printf "I'm looking at element %s val %s now\n" s elem in
    Hashtbl.iter f my_hash;;
	(* Hashtbl.remove my_hash "xxx";; *) *)

		

