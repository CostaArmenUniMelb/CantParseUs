open Snick_ast
open Snick_pprint
  
(*procedure q*)
let procedure_q_identifier = "q" 
let procedure_q_parameters = [(Value,Single(Float,"x")); (Reference,Single(Int,"k"))]
  
let procedure_q_declerations = [Single(Int,"n"); Single(Float,"y"); Array(Bool,"a",[(1,8)])]
  
let procedure_q_statement1 = Assign(LArrayElement("a",[Eint(8)]),Rexpr(Ebool(true)))
let procedure_q_statement2 = Assign(LId("k"),Rexpr(Eint(42)))
let procedure_q_statements = [procedure_q_statement1; procedure_q_statement2]

let procedure_q_body = (procedure_q_declerations,procedure_q_statements)

let procedure_q = (procedure_q_identifier,procedure_q_parameters,procedure_q_body)
  
(*procedure p*)
let procedure_p_identifier = "p" 
let procedure_p_parameters = [(Reference,Single(Int,"i"))]
  
let procedure_p_declerations = []
  
let procedure_p_statement1 = Assign(LId("i"),Rexpr(Ebinop(Ebinop(Eint(6),Op_mul,Elval(LId("i"))),Op_add,Eint(4))))
let procedure_p_statements = [procedure_p_statement1]
  
let procedure_p_body = (procedure_p_declerations,procedure_p_statements)

let procedure_p = (procedure_p_identifier,procedure_p_parameters,procedure_p_body)
  
(*procedure main*)
let procedure_main_identifier = "main" 
let procedure_main_parameters = []
  
let procedure_main_declerations = [Single(Int,"m"); Single(Int,"n")]
  
let read_n = Read(LId("n"))
let n_gt_1 = Ebinop(Elval(LId("n")),Op_gt,Eint(1))
let assign_m_n = Assign(LId("m"),Rexpr(Elval(LId("n"))))
let m_gt_0 = Ebinop(Elval(LId("m")),Op_gt,Eint(0))
let assign_n_n_minus_1 = Assign(LId("n"),Rexpr(Ebinop(Elval(LId("n")),Op_sub,Eint(1))))
let assign_m_m_minus_1 = Assign(LId("m"),Rexpr(Ebinop(Elval(LId("m")),Op_sub,Eint(1))))
let m_eq_0 = Ebinop(Elval(LId("m")),Op_eq,Eint(0))
let proc_p_invoke = InvokeProc("p",[Elval(LId("n"))])
let if_then_1 = IfThen(m_eq_0, [proc_p_invoke])
let if_then_else_1_then_statements = [assign_n_n_minus_1; assign_m_m_minus_1; if_then_1]
let assign_m_n_minus_m = Assign(LId("m"),Rexpr(Ebinop(Elval(LId("n")),Op_sub,Elval(LId("m")))))
let if_then_else_1_else_statements = [assign_m_n_minus_m; assign_m_m_minus_1]
let if_then_else_1 = IfThenElse(m_gt_0,if_then_else_1_then_statements,if_then_else_1_else_statements)
let whiledo_2 = WhileDo(m_gt_0,[if_then_else_1])
let whiledo_1 = WhileDo(n_gt_1,[assign_m_n; whiledo_2])
  
let procedure_main_statements = [read_n; whiledo_1]
  
let procedure_main_body = (procedure_main_declerations,procedure_main_statements)

let procedure_main = (procedure_main_identifier, procedure_main_parameters, procedure_main_body)
  
(*all procedures*)
let program = [procedure_q; procedure_p; procedure_main]
  
(*print test*)
let _ = print_string (print_program program)
  
  
  