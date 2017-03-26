open Snack_ast
open Snack_pprint

let test_ast_print = 
  print_string 
    (
       format_program
       [
       ( 
          "proc1",
          [
            Value,Single(Bool,"declIdentBool1");
            Reference,Single(Int,"declIdentInt2")
          ],
        (
            
            [
              Single(Bool,"declIdentBool1");
              Single(Int,"declIdentInt2");
              Single(Float,"declIdentFloat3");
              Array(Float,"declIdentFloatArray3",[(1,2);(1,3)])
            ], 
             
              [
                Assign(LId("assignIdent1"),Rexpr(Eparens(Ebinop(Eunop(Op_not,Ebool(true)),Op_or,Ebool(false)))));
                Assign(LId("assignIdent2"),Rexpr(Eint(1)));
                Assign(LId("assignIdent3"),Rexpr(Elval(LId("declIdentBool1"))));
                Assign(LId("assignIdent4"),Rexpr(Ebinop(Eint(1),Op_add,Eunop(Op_minus,Efloat(2.1)))));
                Assign(LArrayElement("assignIdentFloatArray5",[Ebinop(Eint(1),Op_sub,Eint(2));Eint(5)]),Rexpr(Efloat(10.1)));
                Assign(LArrayElement("assignIdentFloatArray6",[Elval(LId("assignIdent2"))]),Rexpr(Efloat(10.1)));
                Assign(LArrayElement("assignIdentFloatArray6",[]),Rexpr(Efloat(10.1)));
                Read(LId("readIdent3")); 
                Write(Eunop(Op_minus,Elval(LId("assignIdent1"))));
                Write(Estring("\"hello\""));
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
        )
       );
         (
          "proc2"
          ,
          [
            
          ],
          (
             [],
             [Read(LId("readIdent3"))]
          )
         )
       ]
    )
      
let _ = test_ast_print