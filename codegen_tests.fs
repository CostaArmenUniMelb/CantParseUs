namespace Snick
module Tests =
    module CantParseUs =
        let read_inputs_proc =
            (
            "read_inputs"
            ,
            [
            ]
            ,
            (
            [
            Single(Int,"x")
            Single(Int, "y")
            Single(Int, "sum")
            ]
            ,
            [
            Write(Estring("Input two positive integers to be added up: "))
            Read(LId("x"))
            Read(LId("y"))
            Write(Estring("\\n"))
            Assign(LId("sum"),Rexpr(Ebinop(Elval(LId("x")),binop.Op_add,Elval(LId("y")))))
            Write(Estring("The result is: "))
            Write(Elval(LId("sum")))
            Write(Estring("\\n"))
            ]
            )
            )

        let read_inputs =
            [
            (
            "main"
            ,
            []
            ,
            (
            [] 
            ,
            [
            InvokeProc("read_inputs",[])
            ] 
            )
            )
            read_inputs_proc
            ]
            |> codegen.Generate
        
        let test_ref_proc =
            (
            "test_ref"
            ,
            [
            (Reference,Single(Int,"a"))
            (Reference,Single(Int,"b"))
            ]
            ,
            (
            []
            ,
            [
            Assign(LId("b"),Rexpr(Ebinop(Elval(LId("b")),binop.Op_mul,Eint(2))))
            Assign(LId("a"),Rexpr(Ebinop(Elval(LId("a")),binop.Op_mul,Elval(LId("b")))))
            ]
            )
            )

        let test_ref =
            [
            (
            "main"
            ,
            []
            ,
            (
            [
            Single(Int,"c")
            Single(Int,"d")
            ] 
            ,
            [
            Assign(LId("c"),Rexpr(Eint(2)))
            Assign(LId("d"),Rexpr(Eint(8)))
            InvokeProc("test_ref",[Elval(LId("c")); Elval(LId("d"))])
            Write(Estring("c="))
            Write(Elval(LId("c")))
            Write(Estring("\\nd="))
            Write(Elval(LId("d")))
            ] 
            )
            )
            test_ref_proc
            ]
            |> codegen.Generate

        
        let write_bool_proc =
            (
            "write_bool"
            ,
            [
            (Value,Single(Bool,"value"))
            ]
            ,
            (
            []
            ,
            [
            IfThenElse(
             Ebinop(Elval(LId("value")),binop.Op_eq,Ebool(true)),
             [Write(Estring("TRUE"))],
             [Write(Estring("FALSE"))]
            )
            ]
            )
            )

        let write_bool =
            [
            (
            "main"
            ,
            []
            ,
            (
            [] 
            ,
            [
            InvokeProc("write_bool",[Ebool(true)])
            ] 
            )
            )
            write_bool_proc
            ]
            |> codegen.Generate
        
        let assert_bool_proc = 
            (
            "assert_bool"
            ,
            [
            (Value,Single(Bool,"expecting_val"))
            (Value,Single(Bool,"value"))
            ]
            ,
            (
            []
            ,
            [
            Write(Estring("The expecting value is:"))
            Write(Elval(LId("expecting_val")))
            Write(Estring(" , Result:"))
            Write(Elval(LId("value")))
            Write(Estring("\\n"))
            ]
            )
            )
        
        let assert_bool = 
            [
            (
            "main"
            ,
            []
            ,
            (
            [] 
            ,
            [
            InvokeProc("assert_bool",[Ebool(true); Ebool(true)])
            ] 
            )
            )
            assert_bool_proc
            ]
            |> codegen.Generate
        
        let assert_float_proc =
            (
            "assert_float"
            ,
            [
            (Value,Single(Float,"expecting_val"))
            (Value,Single(Float,"value"))
            ]
            ,
            (
            []
            ,
            [
            Write(Estring("The expecting value is:"))
            Write(Elval(LId("expecting_val")))
            Write(Estring(" , Result:"))
            Write(Elval(LId("value")))
            Write(Estring("\\n"))
            ]
            )
            )
        
        let assert_float = 
            [
            (
            "main"
            ,
            []
            ,
            (
            [] 
            ,
            [
            InvokeProc("assert_float",[Efloat(2.0); Efloat(2.0)])
            ] 
            )
            )
            assert_float_proc
            ]
            |> codegen.Generate

        let assert_int_proc = 
            (
            "assert_int"
            ,
            [
            (Value,Single(Int,"expecting_val"))
            (Value,Single(Int,"value"))
            ]
            ,
            (
            []
            ,
            [
            Write(Estring("The expecting value is:"))
            Write(Elval(LId("expecting_val")))
            Write(Estring(" , Result:"))
            Write(Elval(LId("value")))
            Write(Estring("\\n"))
            ]
            )
            )

        let assert_int = 
            [
            (
            "main"
            ,
            []
            ,
            (
            [] 
            ,
            [
            InvokeProc("assert_int",[Eint(2); Eint(2)])
            ] 
            )
            )
            assert_int_proc
            ]
            |> codegen.Generate

        let do_while_proc =
            (
            "do_while"
            ,
            []
            ,
            (
            [
            Single(Int,"i")
            Single(Int,"j")
            ]
            ,
            [
            Assign(LId("i"),Rexpr(Eint(0)))
            Assign(LId("j"),Rexpr(Eint(1)))
            WhileDo(
                Ebinop(Elval(LId("i")),binop.Op_lt,Eint(5)),
                [
                Assign(LId("i"),Rexpr(Ebinop(Elval(LId("i")),binop.Op_add,Eint(1))))
                WhileDo(
                    Ebinop(Elval(LId("j")),binop.Op_lt,Eint(5)),
                    [Assign(LId("j"),Rexpr(Ebinop(Elval(LId("j")),binop.Op_add,Eint(1))))]
                )
                Assign(LId("j"),Rexpr(Eint(0)))
                ]
            )
            InvokeProc("assert_int",[Eint(5);Elval(LId("i"))])
            InvokeProc("assert_int",[Eint(0);Elval(LId("j"))])
            ]
            )
            )

        let do_while =
            [
            (
            "main"
            ,
            []
            ,
            (
            [] 
            ,
            [
            InvokeProc("do_while",[])
            ] 
            )
            )
            do_while_proc
            assert_int_proc
            ]
            |> codegen.Generate
    
    module Assignment =
        let assignment_gcd =
            [
            (
            "main" //identifier
            ,
            [] //parameter_def list
            ,
            (
            [
            //declerations
            Single(Int,"x")
            Single(Int,"y")
            Single(Int,"temp")
            Single(Int,"quotient") 
            Single(Int,"remainder") 
            ]
            ,
            [
            //statements
            Write(Estring("Input two positive integers: "))
            Read(LId("x"))
            Read(LId("y"))
            Write(Estring("\\n"))
            IfThen(
                Ebinop(Elval(LId("x")),binop.Op_lt,Elval(LId("y"))),
                [
                Assign(LId("temp"),Rexpr(Elval(LId("x"))))
                Assign(LId("x"),Rexpr(Elval(LId("y"))))
                Assign(LId("y"),Rexpr(Elval(LId("temp"))))
                ]
            )
            Write(Estring("The gcd of "))
            Write(Elval(LId("x")))
            Write(Estring(" and "))
            Write(Elval(LId("y")))
            Write(Estring(" is "))
            Assign(LId("quotient"),Rexpr(Ebinop(Elval(LId("x")),binop.Op_div,Elval(LId("y")))))
            Assign(LId("remainder"),Rexpr(Ebinop(Elval(LId("x")),binop.Op_sub,Ebinop(Elval(LId("quotient")),binop.Op_mul,Elval(LId("y"))))))
            WhileDo(
                Ebinop(Elval(LId("remainder")),binop.Op_gt,Eint(0)),
                [
                Assign(LId("x"),Rexpr(Elval(LId("y"))))
                Assign(LId("y"),Rexpr(Elval(LId("remainder"))))
                Assign(LId("quotient"),Rexpr(Ebinop(Elval(LId("x")),binop.Op_div,Elval(LId("y")))))
                Assign(LId("remainder"),Rexpr(Ebinop(Elval(LId("x")),binop.Op_sub,Ebinop(Elval(LId("quotient")),binop.Op_mul,Elval(LId("y"))))))
                ]
            )
            Write(Elval(LId("y")))
            Write(Estring("\\n"))
            ]
            )
            )
            ]
            //|> codegen.Generate
    
    module Tutorials = 
        let q46 = 
            [
            (
            "main"
            ,
            [] //params
            ,
            (
            [Single(Int, "k")] //decls
            ,
            //stmts
            [
            Assign(LId("k"),Rexpr(Eint(41)))
            InvokeProc("incr",[Elval(LId("k"))])
            Write(Elval(LId("k")))
            Write(Estring("\\n"))
            ] 
            )
            )
            (
            "incr"
            ,
            [(Reference,Single(Int,"n"))] //params
            ,
            (
            [] //decls
            ,
            //stmts
            [
            Assign(LId("n"),Rexpr(Ebinop(Elval(LId("n")),binop.Op_add,Eint(1))))
            ]
            )
            )
            ]
            |> codegen.Generate

        let q45 =
            [
            (
            "f"
            ,
            [
            (Value,Single(Int,"w"))
            (Value,Single(Int,"x"))
            (Value,Single(Int,"y"))
            (Value,Single(Int,"z"))
            ]
            ,
            (
            [
            Array(Int, "a", [(1,2);(2,4)])
            ]
            ,
            [
            Assign(LArrayElement("a",[Elval(LId("w"));Elval(LId("x"))]),Rexpr(Eint(42)))
            Assign(LArrayElement("a",[Elval(LId("y"));Elval(LId("z"))]),Rexpr(Elval(LArrayElement("a",[Elval(LId("w"));Elval(LId("x"))]))))
            Write(Elval(LArrayElement("a",[Elval(LId("y"));Elval(LId("z"))])))
            Write(Estring("\\n"))
            ]
            )
            )
            (
            "main"
            ,
            []
            ,
            (
            []
            ,
            [
            InvokeProc("f", [Eint(2);Eint(3);Eint(1);Eint(4)])
            ]
            )
            )
            ]
            //|> codegen.Generate
        


