open Printf 
open Snick_ast
exception Syntax_error of string;;

let debugging = true;;
(* Show  some messages if debugging *)
let debugmsg msg = 
	if debugging then 
		print_string msg;
	;;

let init_prog =
	 printf "Initiated";; 


let finalize prog =
	(* printf "Finalized"; *)
(* 	raise(Syntax_error "TestSynERROR"); *)
	prog;;


let check_typedef_range ttypedef =
 	match ttypedef with
		|Array(datatype,identifier,range_list) -> 
			for i = 0 to List.length(range_list) -1  do 
				let range =(List.nth range_list i)in
				let (min,max) = range in
				debugmsg (sprintf "Array size %d %d\n" min max) ;
				if min < 0 || max <0 || min > max then
					raise(Syntax_error (sprintf "Invalid Array Size %d:%d" min max)); 
			done;
			ttypedef;
		| _ -> debugmsg "Not an array\n"; ttypedef;
		;
	;;