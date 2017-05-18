open Codegen
open Snick_ast  
module Test = struct
  let read_inputs_proc : procedure =
      (
      "read_inputs"
      ,
      [
      ]
      ,
      (
      [
      (Single(Int,"x"), Expr_None);
      (Single(Int, "y"), Expr_None);
      (Single(Int, "sum"), Expr_None)
      ]
      ,
      [
      Write(Estring("Input two positive integers to be added up: ", Expr_None));
      Read(LId("x", Expr_None));
      Read(LId("y", Expr_None));
      Write(Estring("\\n", Expr_None));
      Assign(LId("sum", Expr_None),Rexpr(Ebinop(Elval(LId("x", Expr_None), Expr_None),Op_add,Elval(LId("y", Expr_None), Expr_None), Expr_None)));
      Write(Estring("The result is: ", Expr_None));
      Write(Elval(LId("sum", Expr_None), Expr_None));
      Write(Estring("\\n", Expr_None))
      ]
      )
      )
        
  let test_ref_proc : procedure =
      (
      "test_ref"
      ,
      [
      (Reference,Single(Int,"a"), Expr_None);
      (Reference,Single(Int,"b"), Expr_None)
      ]
      ,
      (
      []
      ,
      [
      Assign(LId("b", Expr_None),Rexpr(Ebinop(Elval(LId("b", Expr_None), Expr_None),Op_mul,Eint(2, Expr_None), Expr_None)));
      Assign(LId("a", Expr_None),Rexpr(Ebinop(Elval(LId("a", Expr_None), Expr_None),Op_mul,Elval(LId("b", Expr_None), Expr_None), Expr_None)))
      ]
      )
      )
        
  let write_bool_proc : procedure =
      (
      "write_bool"
      ,
      [
      (Value,Single(Bool,"value"), Expr_None)
      ]
      ,
      (
      []
      ,
      [
      IfThenElse(
       Ebinop(Elval(LId("value", Expr_None), Expr_None),Op_eq,Ebool(true, Expr_None), Expr_None),
       [Write(Estring("TRUE", Expr_None))],
       [Write(Estring("FALSE", Expr_None))]
      )
      ]
      )
      )
        
  let assert_bool_proc : procedure = 
      (
      "assert_bool"
      ,
      [
      (Value,Single(Bool,"expecting_val"), Expr_None);
      (Value,Single(Bool,"value"), Expr_None)
      ]
      ,
      (
      []
      ,
      [
      Write(Estring("The expecting value is:", Expr_None));
      Write(Elval(LId("expecting_val", Expr_None), Expr_None));
      Write(Estring(" , Result:", Expr_None));
      Write(Elval(LId("value", Expr_None), Expr_None));
      Write(Estring("\\n", Expr_None))
      ]
      )
      )
        
  let assert_float_proc : procedure =
      (
      "assert_float"
      ,
      [
      (Value,Single(Float,"expecting_val"), Expr_None);
      (Value,Single(Float,"value"), Expr_None)
      ]
      ,
      (
      []
      ,
      [
      Write(Estring("The expecting value is:", Expr_None));
      Write(Elval(LId("expecting_val", Expr_None), Expr_None));
      Write(Estring(" , Result:", Expr_None));
      Write(Elval(LId("value", Expr_None), Expr_None));
      Write(Estring("\\n", Expr_None))
      ]
      )
      )

  let assert_int_proc : procedure = 
      (
      "assert_int"
      ,
      [
      (Value,Single(Int,"expecting_val"), Expr_None);
      (Value,Single(Int,"value"), Expr_None)
      ]
      ,
      (
      []
      ,
      [
      Write(Estring("The expecting value is:", Expr_None));
      Write(Elval(LId("expecting_val", Expr_None), Expr_None));
      Write(Estring(" , Result:", Expr_None));
      Write(Elval(LId("value", Expr_None), Expr_None));
      Write(Estring("\\n", Expr_None))
      ]
      )
      )

  let do_while_proc : procedure =
      (
      "do_while"
      ,
      []
      ,
      (
      [
      (Single(Int,"i"), Expr_None);
      (Single(Int,"j"), Expr_None)
      ]
      ,
      [
      Assign(LId("i", Expr_None),Rexpr(Eint(0, Expr_None)));
      Assign(LId("j", Expr_None),Rexpr(Eint(1, Expr_None)));
      WhileDo(
          Ebinop(Elval(LId("i", Expr_None), Expr_None),Op_lt,Eint(5, Expr_None), Expr_None),
          [
          Assign(LId("i", Expr_None),Rexpr(Ebinop(Elval(LId("i", Expr_None), Expr_None),Op_add,Eint(1, Expr_None), Expr_None)));
          WhileDo(
              Ebinop(Elval(LId("j", Expr_None), Expr_None),Op_lt,Eint(5, Expr_None), Expr_None),
              [Assign(LId("j", Expr_None),Rexpr(Ebinop(Elval(LId("j", Expr_None), Expr_None),Op_add,Eint(1, Expr_None), Expr_None)))]
          );
          Assign(LId("j", Expr_None),Rexpr(Eint(0, Expr_None)))
          ]
      );
      InvokeProc("assert_int",[Eint(5, Expr_None);Elval(LId("i", Expr_None), Expr_None)]);
      InvokeProc("assert_int",[Eint(0, Expr_None);Elval(LId("j", Expr_None), Expr_None)])
      ]
      )
      )

  let if_else_proc : procedure =
      (
      "if_else"
      ,
      []
      ,
      (
      [
      (Single(Int,"a"), Expr_None);
      (Single(Int,"j"), Expr_None)
      ]
      ,
      [
      Assign(LId("j", Expr_None),Rexpr(Eint(20, Expr_None)));
      IfThenElse(
          Ebinop(
              Ebinop(Elval(LId("a", Expr_None), Expr_None),Op_eq,Eint(0, Expr_None), Expr_None),
              Op_and,
              Ebinop(Elval(LId("j", Expr_None), Expr_None),Op_eq,Eint(20, Expr_None), Expr_None),
              Expr_None
          ),
          [
          Assign(LId("a", Expr_None),Rexpr(Eint(10, Expr_None)));
          IfThenElse(
              Ebinop(Elval(LId("a", Expr_None), Expr_None),Op_lt,Eint(5, Expr_None), Expr_None),
              [Assign(LId("a", Expr_None),Rexpr(Eint(20, Expr_None)))],
              [
              Assign(LId("a", Expr_None),Rexpr(Eint(50, Expr_None)));
              IfThenElse(
                  Ebinop(Elval(LId("a", Expr_None), Expr_None),Op_lt,Eint(30, Expr_None), Expr_None),
                  [Assign(LId("a", Expr_None),Rexpr(Eint(40, Expr_None)))],
                  [
                  Assign(LId("a", Expr_None),Rexpr(Eint(555, Expr_None)));
                  Write(Estring("Correct IF-ELSE\\nn", Expr_None))
                  ]
              )
              ]
          )
          ],
          [
          Assign(LId("a", Expr_None),Rexpr(Eint(20, Expr_None)))
          ]
      );
      InvokeProc("assert_int",[Eint(555, Expr_None);Elval(LId("a", Expr_None), Expr_None)])
      ]
      )
      )

  let logic_operation_proc : procedure =
      (
      "logic_operation"
      ,
      []
      ,
      (
      [(Single(Bool, "a"), Expr_None)]
      ,
      [
      InvokeProc("assert_bool",[Ebool(false, Expr_None); Elval(LId("a", Expr_None), Expr_None)]);
      InvokeProc("assert_bool",[Ebool(true, Expr_None); Ebinop(Ebool(true, Expr_None),Op_and, Ebool(true, Expr_None), Expr_None)]);
      InvokeProc("assert_bool",[Ebool(false, Expr_None); Ebinop(Ebool(true, Expr_None),Op_and, Ebool(false, Expr_None), Expr_None)]);
      InvokeProc("assert_bool",[Ebool(true, Expr_None); Ebinop(Ebool(true, Expr_None),Op_or,Ebinop(Ebool(false, Expr_None),Op_and, Ebool(true, Expr_None), Expr_None), Expr_None)])
      ]
      )
      )

  let math_operation_proc =
      (
      "math_operation"
      ,
      []
      ,
      (
      []
      ,
      [
      InvokeProc("assert_int",[Ebinop(Eint(3, Expr_None),Op_add,Eint(4, Expr_None), Expr_None); Eint(7, Expr_None)]);
      InvokeProc("assert_int",[Ebinop(Eint(3, Expr_None),Op_sub,Eint(4, Expr_None), Expr_None); Eint(-1, Expr_None)]);
      InvokeProc("assert_int",[Eunop(Op_minus,Eparens(Ebinop(Eint(3, Expr_None),Op_sub,Eint(4, Expr_None), Expr_None), Expr_None), Expr_None); Eint(1, Expr_None)]);
      InvokeProc("assert_int",[Ebinop(Eint(3, Expr_None),Op_mul,Eint(4, Expr_None), Expr_None); Eint(12, Expr_None)]);
      InvokeProc("assert_int",[Ebinop(Eint(3, Expr_None),Op_div,Eint(4, Expr_None), Expr_None); Eint(0, Expr_None)]);

      InvokeProc("assert_float",[Ebinop(Efloat(3.0, Expr_None),Op_div,Efloat(4.0, Expr_None), Expr_None); Efloat(0.75, Expr_None)]);
      InvokeProc("assert_float",[Ebinop(Efloat(3.0, Expr_None),Op_div,Eint(4, Expr_None), Expr_None); Efloat(0.75, Expr_None)]);
      InvokeProc("assert_float",[Ebinop(Eint(3, Expr_None),Op_div,Efloat(4.0, Expr_None), Expr_None); Efloat(0.75, Expr_None)]);

      InvokeProc("assert_int",[Ebinop(Eint(3, Expr_None),Op_add,Ebinop(Eint(4, Expr_None),Op_mul,Eint(2, Expr_None), Expr_None), Expr_None); Eint(11, Expr_None)]);
      InvokeProc("assert_int",[Ebinop(Eint(3, Expr_None),Op_sub,Ebinop(Eint(4, Expr_None),Op_mul,Eint(2, Expr_None), Expr_None), Expr_None); Eint(-5, Expr_None)])
      ]
      )
      )

  let array_process_proc : procedure =
      (
      "array_process"
      ,
      []
      ,
      (
      [
      (Array(Int,"a",[(1,20)]), Expr_None);
      (Array(Int,"b",[(0,4);(2,6);(3,8)]), Expr_None)
      ]
      ,
      [
      Assign(LArrayElement("a",[Eint(2, Expr_None)], Expr_None),Rexpr(Eint(20, Expr_None)));
      Assign(LArrayElement("b",[Eint(0, Expr_None);Eint(3, Expr_None);Eint(4, Expr_None)], Expr_None),Rexpr(Eint(30, Expr_None)));

      Assign(LArrayElement("a",[Eint(3, Expr_None)], Expr_None),Rexpr(Elval(LArrayElement("b",[Eint(0, Expr_None);Eint(3, Expr_None);Eint(5, Expr_None)], Expr_None), Expr_None)));
      InvokeProc("assert_int",[Eint(0, Expr_None);Elval(LArrayElement("a",[Eint(3, Expr_None)], Expr_None), Expr_None)]);

      Assign(LArrayElement("a",[Eint(4, Expr_None)], Expr_None),Rexpr(Elval(LArrayElement("b",[Eint(0, Expr_None);Eint(3, Expr_None);Eint(4, Expr_None)], Expr_None), Expr_None)));
      InvokeProc("assert_int",[Eint(30, Expr_None);Elval(LArrayElement("a",[Eint(4, Expr_None)], Expr_None), Expr_None)])
      ]
      )
      )

  let simple_decleration_proc : procedure =
      (
      "simple_decleration"
      ,
      [
      (Value,Single(Int,"input_1"), Expr_None);
      (Value,Single(Float,"input_2"), Expr_None);
      (Value,Single(Bool,"input_3"), Expr_None)
      ]
      ,
      (
      [
      (Single(Int,"a"), Expr_None);
      (Single(Float,"b"), Expr_None);
      (Single(Bool,"c"), Expr_None)
      ]
      ,
      [
      Assign(LId("a", Expr_None),Rexpr(Elval(LId("input_1", Expr_None), Expr_None)));
      InvokeProc("assert_int",[Elval(LId("input_1", Expr_None), Expr_None);Elval(LId("a", Expr_None), Expr_None)]);

      Assign(LId("b", Expr_None),Rexpr(Elval(LId("input_2", Expr_None), Expr_None)));
      InvokeProc("assert_float",[Elval(LId("input_2", Expr_None), Expr_None);Elval(LId("b", Expr_None), Expr_None)]);

      Assign(LId("c", Expr_None),Rexpr(Elval(LId("input_3", Expr_None), Expr_None)));
      InvokeProc("assert_bool",[Elval(LId("input_3", Expr_None), Expr_None);Elval(LId("c", Expr_None), Expr_None)]);

      Assign(LId("b", Expr_None),Rexpr(Eint(20, Expr_None)));
      InvokeProc("assert_float",[Efloat(20.0, Expr_None);Elval(LId("b", Expr_None), Expr_None)])
      ]
      )
      )

  let main =
      [
      (
      "main"
      ,
      []
      ,
      (
      [
      (Single(Int,"a"), Expr_None);
      (Single(Int,"b"), Expr_None)
      ]
      ,
      [
      Assign(LId("a", Expr_None),Rexpr(Eint(3, Expr_None)));
      Assign(LId("b", Expr_None),Rexpr(Eint(2, Expr_None)));

      InvokeProc("test_ref",[Elval(LId("a", Expr_None), Expr_None); Elval(LId("b", Expr_None), Expr_None)]);
      InvokeProc("assert_int",[Eint(4, Expr_None); Elval(LId("b", Expr_None), Expr_None)]);
      InvokeProc("assert_int",[Eint(12, Expr_None); Elval(LId("a", Expr_None), Expr_None)]);

      InvokeProc("simple_decleration",[Eint(2, Expr_None); Efloat(3.5, Expr_None); Ebool(true, Expr_None)]);

      InvokeProc("array_process",[]);
      InvokeProc("math_operation",[]);
      InvokeProc("logic_operation",[]);
      InvokeProc("if_else",[]);
      InvokeProc("do_while",[]);
      InvokeProc("read_inputs",[])
      ]
      )
      );
      test_ref_proc;
      assert_int_proc;
      assert_bool_proc;
      assert_float_proc;
      simple_decleration_proc;
      array_process_proc;
      math_operation_proc;
      logic_operation_proc;
      if_else_proc;
      do_while_proc;
      read_inputs_proc;
      write_bool_proc
      ]
end
open Test

let _ = 
	let code = Test.main |> generate
	in
	print_string (code)

