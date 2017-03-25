open Snack_ast
open Format
  
(*helper functions - indenting and 'reductions'*)
let indent num = 
  let rec indentation count spaces  =
    match spaces with
      | _ when count > 0 -> indentation (count-1) (" "^spaces)
      | _ -> spaces
  in
  let result = indentation num "" in
  if num <=0 then "" else result
    
let indent_list ind lst = lst |> List.map (fun p -> (indent ind) ^ p)
  
let add_indent ind = ind + 4

let reduce str lst =
  match lst with
    | [] -> ""
    | _ -> List.fold_left (fun s1 s2 -> s1 ^ str ^ s2) (List.hd lst) (List.tl lst)
      
(*formatted types*)
let fmt_dataType bt = 
  match bt with
    | Bool -> "bool"
    | Int -> "int"
    | Float -> "float"
      
let fmt_ident_dataType td =  
  let (ident, datatype) = td
  in
  (fmt_dataType datatype) ^ " " ^ ident
    
let fmt_int_range ir = 
  let (min, max) = ir in
  (string_of_int min) ^ ".." ^ (string_of_int max)
    
let fmt_int_range_list irl = 
  List.map (fun i -> fmt_int_range i) irl
  |> reduce ","
    
let fmt_typedef td = 
  match td with
    | Single(i,b as s) -> fmt_ident_dataType s
    | Array(i,b,il) -> 
      (fmt_ident_dataType (i,b)) ^ "[" ^ (fmt_int_range_list il) ^ "]"
        
  
let fmt_binop bo =
  match bo with
    | Op_add -> "+"
    | Op_sub -> "-" 
    | Op_mul -> "*" 
    | Op_div -> "/"
    | Op_eq ->  "="
    | Op_lt -> "<"
    | Op_or ->   "or"
    | Op_and -> "and"
    | Op_not_eq -> "!="
    | Op_lt_eq -> "<="
    | Op_gt -> ">"
    | Op_gt_eq -> ">=" 
      
let fmt_unop uo = 
  match uo with
    | Op_minus -> "-"
    | Op_not -> "not "
      
let fmt_bool b = 
  match b with
    | true -> "true"
    | false -> "false" 
  
let rec fmt_lvalue lval =
  match lval with
    | LId(i) -> i
    | LArrayIndex(id,ind) -> 
      let expr_list = fmt_expr_list ind in
      id ^ "[" ^ expr_list ^ "]"
    
and fmt_expr e =
  match e with
    | Ebool(b) -> fmt_bool b
    | Eint(i) -> string_of_int i
    | Efloat(f) -> string_of_float f
    | Elval(lval) -> fmt_lvalue lval
    | Ebinop(eb) -> 
      let (e1, b, e2) = eb
      in
      reduce " " [(fmt_expr e1);(fmt_binop b);(fmt_expr e2)]
    | Eunop(eu) -> 
      let (u, e) = eu
      in
      (fmt_unop u) ^ (fmt_expr e)
    | Eparens(e) -> "(" ^ fmt_expr e ^ ")"
        
and fmt_expr_list lst = List.map (fun e -> fmt_expr e) lst |> reduce ","
        
let fmt_rvalue rval =
  match rval with
    | Rexpr(e) -> fmt_expr e
      
let fmt_decl ind d = (indent ind) ^ fmt_typedef d ^ ";"

let fmt_decl_list ind lst = 
  List.map (fun s -> fmt_decl ind s) lst 
  |> List.filter (fun d -> d <> "")
  |> reduce "\n"
  
let rec fmt_stmt ind s =
  let current_indent = indent ind 
  in
  match s with
    | Assign(lval,rval) -> reduce " " [(fmt_lvalue lval);":=";(fmt_rvalue rval)] ^ ";"
    | Read(lval) -> "read" ^ " " ^ (fmt_lvalue lval) ^ ";"
    | Write(e) -> "write" ^ " " ^ (fmt_expr e) ^ ";" 
    | InvokeProc(id,el) -> id ^ "(" ^ fmt_expr_list el ^ ")" ^ ";"
    | IfThen(e,sl) -> 
      "if " ^ fmt_expr e ^ " then\n" ^ 
      fmt_stmt_list (add_indent ind) sl ^ "\n" ^ 
      current_indent ^ "fi"
    | IfThenElse(e,sl1,sl2) -> 
      "if " ^ fmt_expr e ^ " then\n" ^ 
      fmt_stmt_list (add_indent ind) sl1 ^ "\n" ^ 
      current_indent ^ "else\n" ^ 
        fmt_stmt_list (add_indent ind) sl2 ^ "\n" ^ 
      current_indent ^ "fi"
    | WhileDo(e,sl) -> 
      "while " ^ fmt_expr e ^ " do\n" ^
       fmt_stmt_list (add_indent ind) sl ^ "\n" ^
       current_indent ^ "od"       
and fmt_stmt_list ind lst = 
  lst 
  |> List.map (fun s -> fmt_stmt ind s) 
  |> indent_list ind 
  |> reduce "\n"
    
let fmt_proc_body ind p =
  let decls = p.decls in
  let stmts = p.stmts in
  let fmt_decls = (fmt_decl_list (add_indent ind) decls)
  in
  let fmt_stmts = fmt_stmt_list (add_indent ind) stmts
  in
  match fmt_decls with
    | "" -> fmt_stmts
    | _ -> (reduce "\n\n" [fmt_decls;fmt_stmts])
    
let fmt_passby pb =
  match pb with
    | Value -> "val"
    | Reference -> "ref"
   
let fmt_param p =
  let (passby, typedef) = p
  in
  (fmt_passby passby) ^ " " ^ (fmt_typedef typedef)
    
let fmt_proc p =
  let (ident, params, program) = p
  in
  let fmt_params = List.map (fun p -> fmt_param p) params
  in
  let params_string = reduce ", " fmt_params
  in
  "proc " ^ ident ^ " (" ^ params_string ^ ")\n" ^ (fmt_proc_body 1 program) ^ "\nend"
    
let fmt_program pl = 
  let result = List.map (fun p -> fmt_proc p) pl |> reduce "\n\n"
  in
  result ^ "\n"

    
(*tests*)
let test_ast_print = 
  print_string 
    (
       fmt_program
       [
       ( 
          "proc1",
          [
            Value,Single("declIdentBool1",Bool);
            Reference,Single("declIdentInt2",Int)
          ],
         {
            decls = 
            [
              Single("declIdentBool1",Bool);
              Single("declIdentInt2",Int);
              Single("declIdentFloat3",Float);
              Array("declIdentFloatArray3",Float,[(1,2);(1,3)])
            ]; 
            stmts = 
              [
                Assign(LId("assignIdent1"),Rexpr(Eparens(Ebinop(Eunop(Op_not,Ebool(true)),Op_or,Ebool(false)))));
                Assign(LId("assignIdent2"),Rexpr(Eint(1)));
                Assign(LId("assignIdent3"),Rexpr(Elval(LId("declIdentBool1"))));
                Assign(LId("assignIdent4"),Rexpr(Ebinop(Eint(1),Op_add,Eunop(Op_minus,Efloat(2.1)))));
                Assign(LArrayIndex("assignIdentFloatArray5",[Ebinop(Eint(1),Op_sub,Eint(2));Eint(5)]),Rexpr(Efloat(10.1)));
                Assign(LArrayIndex("assignIdentFloatArray6",[Elval(LId("assignIdent2"))]),Rexpr(Efloat(10.1)));
                Assign(LArrayIndex("assignIdentFloatArray6",[]),Rexpr(Efloat(10.1)));
                Read(LId("readIdent3")); 
                Write(Eunop(Op_minus,Elval(LId("assignIdent1"))));
                InvokeProc("proc1",[Elval(LId("assignIdent1"));Eunop(Op_minus,Efloat(9.2))]);
                InvokeProc("proc1",[]);
                WhileDo(
                   Eint(1),           
                   [
                     IfThenElse(
                       Ebinop(Eint(1),Op_lt_eq,Efloat(2.1)),
                       [Read(LId("readIdent3"));Write(Eunop(Op_minus,Elval(LId("assignIdent1"))))],
                       [IfThen(Ebinop(Ebool(true),Op_not_eq,Eparens(Eunop(Op_not,Ebool(false)))),[Read(LId("readIdent3"));Write(Eunop(Op_minus,Elval(LId("assignIdent1"))))])]
                     )
                   ]                       
                );
                
              ]
         }
       );
         (
          "proc2"
          ,
          [
            
          ],
          {decls = [] ; stmts = [Read(LId("readIdent3"))]}
         )
       ]
    )
      
let _ = test_ast_print
  
  
  