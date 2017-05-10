open Snick_analyze
open Snick_ast

let _ =
	let typedef2 = Array(Bool,"a",[(1,8); (2,4)]) in
	Snick_analyze.check_typedef_range typedef2;;

	let typedef2 = Single(Int,"n") in
	Snick_analyze.check_typedef_range typedef2;;
	

	let typedef3 = Array(Bool,"a",[(1,8); (-3,4)]) in
	Snick_analyze.check_typedef_range typedef3;;