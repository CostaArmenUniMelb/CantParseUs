open Snack_ast
open Snack_pprint
  
(*procedure q*)
let procedure_q_identifier = "q" 
let procedure_q_parameters = [(Value,Single(Float,"x")); (Reference,Single(Int,"k"))]
  
let procedure_q_declerations = [Single(Int,"n"); Single(Float,"y"); Array(Bool,"a",[(1,8)])]
  
let procedure_q_statement1 = Assign(LArrayElement("a",[Eint(8)]),Rexpr(Ebool(true)))
let procedure_q_statement2 = Assign(LId("k"),Rexpr(Eint(42)))
let procedure_q_statements = [procedure_q_statement1; procedure_q_statement2]

let procedure_q_body = (procedure_q_declerations,procedure_q_statements)

let procedure_q = (procedure_q_identifier,procedure_q_parameters,procedure_q_body)
  
(*let test_procedure_q = print_string (format_procedure procedure_q)*)
  
(*procedure p*)
let procedure_p_identifier = "p" 
let procedure_p_parameters = [(Reference,Single(Int,"i"))]
  
let procedure_p_declerations = []
  
let procedure_p_statement1 = Assign(LId("i"),Rexpr(Ebinop(Ebinop(Eint(6),Op_mul,Elval(LId("i"))),Op_add,Eint(4))))
let procedure_p_statements = [procedure_p_statement1]
  
let procedure_p_body = (procedure_p_declerations,procedure_p_statements)

let procedure_p = (procedure_p_identifier,procedure_p_parameters,procedure_p_body)
  
(*let test_procedure_p = print_string (format_procedure procedure_p)*)
  

let test_program = [procedure_q; procedure_p]
    
let _ = print_string (format_program test_program)
  
  
  