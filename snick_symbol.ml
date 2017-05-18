(*
Author: Songpoom Rasdussadee 
Purpose: Snick_symbol is used for storing objects Procedure, InvokeProc and Parameter 
for syntax checking and IR Code generation
*)
open Snick_ast

let hash_size = 1024;;(* set maximum size to 1024 values *)

(* Create and return the default structure of the table 
One main Hashtbl store all symbol tables (other Hashtbl or let's say Hash in Hash)*)
let init_main_tbl  =
	let symbol_tbl = Hashtbl.create hash_size in
	let dummy_data = Invoke_symbol(InvokeProc("p",[Elval(LId("n",Expr_None),Expr_None)]) )in 
	let dummy_name = "dummy" in
	(* Define type by adding a value as a dummy, then remove it (if don't do this the Ocaml compiler won't compile this) *)
	(* Should fix this if there is more time*)
	Hashtbl.add symbol_tbl dummy_name dummy_data ;
	Hashtbl.remove symbol_tbl dummy_name ;
	let main_sym_tbl =  Hashtbl.create hash_size in
	Hashtbl.add main_sym_tbl dummy_name symbol_tbl;
	Hashtbl.remove main_sym_tbl dummy_name ;
	main_sym_tbl;
;;

(* add a new symbol table to main_sym_tbl *)
let add_tbl main_sym_tbl tbl_name =
	let symbol_tbl = Hashtbl.create hash_size in
	Hashtbl.add main_sym_tbl tbl_name symbol_tbl;
;;
(* insert a new symbol(object) to a symbol table*)
let insert_symbol_to_tbl main_sym_tbl tbl_name id symbol_object =
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	Hashtbl.add target_tbl id symbol_object;
;;

(* Check if the id in a symbol table exist *)
let exist_symbol main_sym_tbl tbl_name id  =
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	Hashtbl.mem target_tbl id;
;;

(* Return a symbol object from a table *)
let find_symbol main_sym_tbl tbl_name id  =
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	Hashtbl.find target_tbl id;
;;

(* Return all symbols in the table *)
let get_list main_sym_tbl tbl_name = 
	let target_tbl = Hashtbl.find main_sym_tbl tbl_name in
	let a = ref [] in
	let f s elem =
	    a:=List.append !a [(s,elem)];
	    in
	    Hashtbl.iter f target_tbl;
	!a;
;;