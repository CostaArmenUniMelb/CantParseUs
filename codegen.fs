namespace Snick
(*converts a program ast into brill code*)
module codegen =
    
    (*creates new labels with a unique integer as a suffix*)
    module Label =
        let private Counter = ref 0
        let New(labelName:string) : string = 
            Counter := !Counter + 1
            labelName + string(!Counter)

    (*All types relevant to code generation*)
    module Types =
        type Reg = int

        type SlotNum = int

        (*brill built in function types*)
        type BuiltIn =
            | PrintString
            | PrintInt
            | PrintReal
            | PrintBool
            | ReadInt
            | ReadReal
            | ReadBool

        type Label = string

        (*all brill instructions*)
        type Instr =
            | Call of string
            | Halt
            | Label of string   
            | PushStackFrame of int
            | PopStackFrame of int
            | IntConst of Reg * int 
            | StringConst of Reg * string
            | RealConst of Reg * float
            | Store of SlotNum * Reg 
            | Return
            | AddInt of Reg * Reg * Reg
            | AddReal of Reg * Reg * Reg
            | IntToReal of Reg * Reg
            | SubInt of Reg * Reg * Reg
            | SubReal of Reg * Reg * Reg
            | MulInt of Reg * Reg * Reg
            | MulReal of Reg * Reg * Reg
            | DivInt of Reg * Reg * Reg
            | DivReal of Reg * Reg * Reg
            | CallBuiltIn of BuiltIn
            | Load of Reg * SlotNum
            | CmpEqInt of Reg * Reg * Reg
            | CmpNeInt of Reg * Reg * Reg
            | CmpGtInt of Reg * Reg * Reg
            | CmpGeInt of Reg * Reg * Reg
            | CmpLtInt of Reg * Reg * Reg
            | CmpLeInt of Reg * Reg * Reg
            | CmpEqReal of Reg * Reg * Reg
            | CmpNeReal of Reg * Reg * Reg
            | CmpGtReal of Reg * Reg * Reg
            | CmpGeReal of Reg * Reg * Reg
            | CmpLtReal of Reg * Reg * Reg
            | CmpLeReal of Reg * Reg * Reg
            | And of Reg * Reg * Reg
            | Or of Reg * Reg * Reg
            | Not of Reg * Reg
            | BranchOnFalse of Reg * Label
            | BranchOnTrue of Reg * Label
            | BranchUncond of Label
            | Comment of string
            | StoreIndirect of Reg * Reg
            | LoadIndirect of Reg * Reg
            | LoadAddress of Reg * SlotNum
            | AddOffset of Reg * Reg * Reg
            | SubOffset of Reg * Reg * Reg
            | Move of Reg * Reg 

        type Code = Instr list

        type VarId = string

        (*variables (declerations and parameters) in a procedures environment*)
        type Var = {datatype:datatype; slotNum:SlotNum; passby:passby; typedef:type_def}

        (*environment containing all variables*)
        type Env = Map<VarId, Var>

        (*expression attributes. Parent nodes require data type and code from children during 
        expresssion code generation*)
        type ExprAttr = {datatype:datatype; code:Code}

        type ProcId = string

        (*holds all procedures and there parameter definitions. 
        Used in code generation for procedure invocation with reference parameters*)
        type ProcParams = Map<ProcId, parameter_def list>

    open Types

    module ProcParams = 
        let Create : ProcParams = Map.empty

        (*get paremeter definitions for a procedure*)
        let Find(procId:ProcId, procParams:ProcParams) : parameter_def list = 
            match procParams.TryFind procId with
            | Some(paramdefs) -> paramdefs
            | None ->  []

        (*add parameter definitions to a procedure*)
        let AddMany(procId:ProcId, paramdefs:parameter_def list, procParams:ProcParams ref) =
            let existing_paramdefs = Find(procId, !procParams)
            procParams := (!procParams).Add(procId, existing_paramdefs @ paramdefs)

        module PassBy =
            (*gets the passby type (i.e. ref or val) for a procedure at a given argument index*)
            let ArgIndex(procId:ProcId, argIndex:int, procParams:ProcParams) : passby =
                let paramdefs = Find(procId, procParams)
                let paramdef = List.item(argIndex) paramdefs
                let (passby, _ ) = paramdef
                passby

            (*is argument index in procedure passby ref*)
            let Ref(procId:ProcId, argIndex:int, procParams:ProcParams) : bool =
                let passby = ArgIndex(procId, argIndex, procParams)
                match passby with
                | Value -> false
                | Reference -> true
    
    module BuiltIn =
        module To =
            (*converts builtin brill types to it's string code form*)
            let String(builtin:BuiltIn) : string =
                match builtin with
                | BuiltIn.PrintInt -> "print_int"
                | BuiltIn.PrintReal -> "print_real"
                | BuiltIn.PrintBool -> "print_bool"
                | BuiltIn.PrintString -> "print_string"
                | BuiltIn.ReadInt -> "read_int"
                | BuiltIn.ReadReal -> "read_float"
                | BuiltIn.ReadBool -> "read_bool"

    module Instruction =
        module To =
            (*takes a single instruction and converts it to a list of instructions (Code)*)
            let Code(instr:Instr) : Code = [instr]

            (*converts brill instruction type into it's printable brill code form*)
            let String(instr:Instr) : string =
                match instr with
                | IntConst(r, i) -> sprintf "int_const r%i, %i" r i
                | RealConst(r, f) -> sprintf "real_const r%i, %.2f" r f
                | StringConst(r, s) -> sprintf "string_const r%i, \"%s\"" r s
                | Store(sn,r) -> sprintf "store %i, r%i" sn r
                | Label(l) -> sprintf "%s:" l
                | PushStackFrame(i) -> sprintf "push_stack_frame %i" i
                | PopStackFrame(i) -> sprintf "pop_stack_frame %i" i
                | Call(n) -> sprintf "call %s" n
                | Halt -> "halt"
                | Return -> "return"
                | CallBuiltIn(bi) -> sprintf "call_builtin %s" (BuiltIn.To.String(bi))
                | AddInt(r1,r2,r3) -> sprintf "add_int r%i, r%i, r%i" r1 r2 r3
                | AddReal(r1,r2,r3) -> sprintf "add_real r%i, r%i, r%i" r1 r2 r3
                | IntToReal(r1,r2) -> sprintf "int_to_real r%i, r%i" r1 r2
                | SubInt(r1,r2,r3) -> sprintf "sub_int r%i, r%i, r%i" r1 r2 r3
                | SubReal(r1,r2,r3) -> sprintf "sub_real r%i, r%i, r%i" r1 r2 r3
                | DivInt(r1,r2,r3) -> sprintf "div_int r%i, r%i, r%i" r1 r2 r3
                | DivReal(r1,r2,r3) -> sprintf "div_real r%i, r%i, r%i" r1 r2 r3
                | MulInt(r1,r2,r3) -> sprintf "mul_int r%i, r%i, r%i" r1 r2 r3
                | MulReal(r1,r2,r3) -> sprintf "mul_real r%i, r%i, r%i" r1 r2 r3
                | Load(r,sn) -> sprintf "load r%i, %i" r sn
                | CmpEqInt(r1,r2,r3) -> sprintf "cmp_eq_int r%i, r%i, r%i" r1 r2 r3
                | CmpNeInt(r1,r2,r3) -> sprintf "cmp_ne_int r%i, r%i, r%i" r1 r2 r3
                | CmpGtInt(r1,r2,r3) -> sprintf "cmp_gt_int r%i, r%i, r%i" r1 r2 r3
                | CmpGeInt(r1,r2,r3) -> sprintf "cmp_ge_int r%i, r%i, r%i" r1 r2 r3
                | CmpLtInt(r1,r2,r3) -> sprintf "cmp_lt_int r%i, r%i, r%i" r1 r2 r3
                | CmpLeInt(r1,r2,r3) -> sprintf "cmp_le_int r%i, r%i, r%i" r1 r2 r3
                | CmpEqReal(r1,r2,r3) -> sprintf "cmp_eq_real r%i, r%i, r%i" r1 r2 r3
                | CmpNeReal(r1,r2,r3) -> sprintf "cmp_ne_real r%i, r%i, r%i" r1 r2 r3
                | CmpGtReal(r1,r2,r3) -> sprintf "cmp_gt_real r%i, r%i, r%i" r1 r2 r3
                | CmpGeReal(r1,r2,r3) -> sprintf "cmp_ge_real r%i, r%i, r%i" r1 r2 r3
                | CmpLtReal(r1,r2,r3) -> sprintf "cmp_lt_real r%i, r%i, r%i" r1 r2 r3
                | CmpLeReal(r1,r2,r3) -> sprintf "cmp_le_real r%i, r%i, r%i" r1 r2 r3
                | And(r1,r2,r3) -> sprintf "and r%i, r%i, r%i" r1 r2 r3
                | BranchOnFalse(r1,l) -> sprintf "branch_on_false r%i, %s" r1 l 
                | BranchOnTrue(r1,l) -> sprintf "branch_on_true r%i, %s" r1 l 
                | BranchUncond(l) -> sprintf "branch_uncond %s" l 
                | Comment(c) -> sprintf "# %s" c
                | Not(r1,r2) -> sprintf "not r%i, r%i" r1 r2
                | Or(r1,r2,r3) -> sprintf "or r%i, r%i, r%i" r1 r2 r3
                | StoreIndirect(r1,r2) -> sprintf "store_indirect r%i, r%i" r1 r2
                | LoadIndirect(r1,r2) -> sprintf "load_indirect r%i, r%i" r1 r2
                | LoadAddress(r,slotnum) -> sprintf "load_address r%i, %i" r slotnum
                | AddOffset(r1,r2,r3) -> sprintf "add_offset r%i, r%i, r%i" r1 r2 r3
                | SubOffset(r1,r2,r3) -> sprintf "sub_offset r%i, r%i, r%i" r1 r2 r3
                | Move(r1, r2) -> sprintf "move r%i, r%i" r1 r2
                |> (fun str -> 
                    match instr with
                    | Comment(_) -> str
                    | Label(_) -> str
                    | _ -> "   " + str
                    )
    
    module Var =
        (*helper function to create var record*)
        let Create(datatype:datatype, slotNum:SlotNum, passby:passby, typedef:type_def) : Var = 
            {datatype = datatype; slotNum = slotNum; passby = passby; typedef=typedef}

    module Env =
        let Init : Env = Map.empty 

        (*get variable in procedure's environment*)
        let Find(varId:identifier, env:Env) : Var = 
            match env.TryFind(varId) with
            | Some(var) -> var
            | None -> failwith "cannot find var in environment"

        (*add variable to environment*)
        let Add(varId:identifier, var:Var, env:Env) : Env =
            match env.TryFind(varId) with
            | Some(_) -> failwith (sprintf "var %s already exists" varId)
            | None -> env.Add(varId, var)

        module SlotNum =
            (*gets slotnumber count in environment*)
            let Max(env:Env) : int = env.Count
        
        module To =
            (*helper function for viewing the contents of an environment*)
            let String(env:Env) : string = 
                env
                |> Map.toList
                |> List.map (fun (varId,var) -> sprintf "%s %A %A" varId (var.datatype) (var.slotNum))
                |> String.concat "\n"
                |> (fun str -> sprintf "\n-----Env-----\n%s\n-----Env-----\n" str)
        
        module Var =
            (*creates a variable and adds it to the environment*)
            let Create(id:identifier, datatype:datatype, env:Env ref, passby:passby, slotNum:SlotNum ref, typedef:type_def) =
                let decl_var = Var.Create(datatype,!slotNum, passby, typedef)
                env := Add(id,decl_var,(!env))
                slotNum := !slotNum + 1

    module Decleration =
        (*gets datatype in decleration*)
        let Datatype(decl:decleration) : datatype = 
            match decl with
            | Single(datatype,_) -> datatype
            | Array(datatype,_,_) -> datatype

        (*gets the identifier in a decleration*)
        let Id(decl:decleration) : identifier = 
            match decl with
            | Single(_,id) -> id
            | Array(_,id,_) -> id

        module Single =
            (*generates code for the initialisation of single (not arrays) declared variables only.
            Note that arrays are converted to singles prior to initialisation. 
            i.e. Array(a[1..n]) = Single(a[1]),..,Single(a[n]).
            Declared variables are initialised (i.e. int -> 0, bool -> false) into registers and 
            then stored in a stack slot. Stackslots are pre-determined in the environment*)
            let Code(varId:VarId, env:Env) : Code = 
                let decl_var = Env.Find(varId, env)
                let decl_var_slotnum = decl_var.slotNum
                let decl_datatype = decl_var.datatype
                let reg = 0
                let store_instr = Store(decl_var_slotnum, reg)
                let const_instr =
                    match decl_datatype with
                    | Int -> IntConst(reg, 0)
                    | Float -> RealConst(reg, 0.0)
                    | String -> StringConst(reg, "")
                    | Bool -> IntConst(reg, 0)
                [
                const_instr
                store_instr
                ]
            
            (*Overloaded function for Single.Code above*)
            let Code1(decl:decleration, env:Env) : Code =
                let decl_id = Id(decl)
                Code(decl_id, env)
        
        module Array =
            (*extracts out the ranges for an array decleration*)
            let Ranges(decl:decleration) : range list =
                match decl with
                | Single(_,_) -> failwith "singles do not have ranges"
                | Array(_,_,ranges) -> ranges
            
            (*extracts out all the min parts within a set of ranges*)
            let Mins(ranges:range list) = 
                ranges 
                |> List.map (fun (s,_) -> s)

            (*for a set of ranges, calculates the size of each range*)
            let Sizes(ranges:range list) = 
                ranges 
                |> List.map (fun (s,e) -> e - s + 1)

            module Offset =
                (*creates a snick expression for the row-major index into an 
                array from a set of snick expressions which describe the position in a 
                multidimensional array.*)
                let Expr(decl:decleration, exprs:expr list) : expr = 
                    let exprs_count = exprs.Length
                    let ranges = Ranges(decl)
                    let ranges_mins = Mins(ranges)
                    let ranges_sizes = Sizes(ranges)
                    // Sum[1,d]( Prod[j=i+1,d]( N[j] ) * n[i]) where N[j] is size of dim, n[i] is index in array, d = dim number
                    let offset_inner_product(j, d, ranges_sizes) = 
                        if 
                            j > d
                        then 
                            Eint(1)
                        else
                            [for x in j..d -> x]
                            |> List.map (fun n -> List.item(n-1) ranges_sizes)
                            |> List.reduce (fun n1 n2 -> n1 * n2)
                            |> (fun n -> Eint(n))
                    let offset_outer_sum(d, exprs, ranges_sizes) = 
                        let i_to_d = [for i in 1..d -> i] 
                        let index_exprs = 
                            List.zip exprs ranges_mins
                            |> List.map (fun (e, s) -> Ebinop(e, binop.Op_sub, Eint(s)))
                        List.zip index_exprs i_to_d
                        |> List.map (fun (e, i) -> Ebinop(e, binop.Op_mul, offset_inner_product(i+1, d, ranges_sizes)))
                        |> List.reduce (fun e1 e2 -> Ebinop(e1, binop.Op_add, e2))
                    offset_outer_sum(exprs_count, exprs, ranges_sizes)
                    
            (*calculates the size of a multidimensional array*)
            let Size(ranges:range list) : int =
                ranges 
                |> List.map (fun (rmin, rmax) -> rmax - rmin + 1) 
                |> List.reduce (fun n1 n2 -> n1 * n2)
            
            (*creates a set of var names for all elements of a multidimensional array.
            the first var has no suffix and all var names after it have a sequential number
            as a suffix. i.e. varName, varName2, varName3,.., varNameN *)
            let VarNames(varName:string, ranges:range list) : string list =
                [for i in 0..Size(ranges) -> i]
                |> List.map(fun i ->
                    let varName = 
                        if i = 0 
                        then varName
                        else sprintf "%s%i" varName i
                    varName
                    )

            (*creates variables for the environment for all elements of a multidimensional array*)
            let Env(id:identifier,datatype:datatype,env:Env ref, passby:passby, slotNum:SlotNum ref,ranges:range list) =
                VarNames(id, ranges)
                |> List.iter(fun varName ->
                    Env.Var.Create(varName,datatype, env, passby, slotNum, Array(datatype, id, ranges))
                    )

            (*generates code for the intialisation of all elements in a multidimensional array*)
            let Code(decl:decleration, env:Env) : Code =
                let decl_id = Id(decl)
                let decl_ranges = Ranges(decl)
                VarNames(decl_id, decl_ranges)
                |> List.map(fun varName -> Single.Code(varName, env) )
                |> List.concat
        
        (*determines the size of a decleration taking into account the size of multidimensional array types*)
        let Count(decl:decleration): int = 
            match decl with
            | Single(_,_) -> 1
            | Array(_,_,ranges) -> Array.Size(ranges)
                

    module Declerations =           
        (*generates code for all declared variable initialisations*)
        let Code(decls:decleration list, env:Env) : Code =
            decls
            |> List.map (fun decl -> 
                match decl with
                | Single(_,_) -> Decleration.Single.Code1(decl, env)
                | Array(_,_,_) -> Decleration.Array.Code(decl, env)
                )
            |> List.concat

        (*adds declared variables to the environment*)
        let Env(decls:decleration list, env:Env ref) =
            let slotNum = ref (Env.SlotNum.Max(!env))
            decls 
            |> List.iter (fun decl -> 
                match decl with
                | Single(datatype,id) -> 
                    Env.Var.Create(id,datatype, env, Value, slotNum, Single(datatype, id))
                | Array(datatype,id,ranges) -> 
                    Decleration.Array.Env(id, datatype, env, Value, slotNum, ranges)
                )

        (*determines the total number of declerations with multidimensional arrays flattened*)
        let Count(decls:decleration list): int = 
            match decls with
            | [] -> 0
            | _ -> 
                decls
                |> List.map (fun decl -> Decleration.Count(decl))
                |> List.reduce (fun n1 n2 -> n1 + n2)

    module LValue =
        (*extracts out the identifier in the lvalue type*)
        let Id(lv:lvalue) : identifier = 
            match lv with
            | LId(id) -> id
            | LArrayElement(id,_) -> id

    module Expr =
        module Unop =
            module Int = 
                (*generates code for a unary integer expression*)
                let Code(unop:unop, reg:Reg, reg2:Reg) : Code =
                    match unop with
                    | Op_minus -> [IntConst(reg2,-1);MulInt(reg,reg,reg2)]
                    | _ -> failwith "cannot unop a non minus with int"

            module Real =
                (*generates code for a unary real expression*)
                let Code(unop:unop, reg:Reg, reg2:Reg) : Code =
                    match unop with
                    | Op_minus -> [RealConst(reg2,-1.0);MulReal(reg,reg,reg2)]
                    | _ -> failwith "cannot unop a non minus with real"

            module Bool =
                (*generates code for a unary bool expression*)
                let Code(unop:unop, reg:Reg, reg2:Reg) : Code =
                    match unop with
                    | Op_not -> [Not(reg,reg)]
                    | _ -> failwith "cannot unop a non not with bool"
            
            (*delegates code generation based on type of expression*)
            let Code(datatype1:datatype, unop:unop, reg:Reg) : Code =
                let reg2 = reg + 1
                match datatype1 with
                | datatype.Int -> Int.Code(unop, reg, reg2)
                | datatype.Float -> Real.Code(unop, reg, reg2)
                | datatype.Bool -> Bool.Code(unop, reg, reg2)
                | datatype.String -> failwith "cannot unop on string"

        module Binop =
            module Number =
                (*determines resultant type of binary expression based on the types
                of it's sub expressions. *)
                let Type(datatype1:datatype, binop:binop, datatype2:datatype) : datatype =
                    match datatype1, datatype2 with
                    | datatype.Int, datatype.Int -> datatype.Int
                    | datatype.Float, datatype.Int -> datatype.Float
                    | datatype.Int, datatype.Float -> datatype.Float
                    | datatype.Float, datatype.Float -> datatype.Float
                    | datatype.Bool, datatype.Bool -> datatype.Bool
                    | _ -> failwith "cannot perform arithmetic and comparison on non numbers"

                module Int =    
                    (*generates code for integer binary expressions*)
                    let Code(binop:binop, reg1:Reg, reg2:Reg) : Code =
                        match binop with
                        | Op_add -> AddInt(reg1, reg1, reg2)
                        | Op_sub -> SubInt(reg1, reg1, reg2)
                        | Op_div -> DivInt(reg1, reg1, reg2)
                        | Op_mul -> MulInt(reg1, reg1, reg2)
                        | Op_eq -> CmpEqInt(reg1, reg1, reg2)
                        | Op_lt -> CmpLtInt(reg1, reg1, reg2)
                        | Op_not_eq -> CmpNeInt(reg1, reg1, reg2)
                        | Op_lt_eq -> CmpLeInt(reg1, reg1, reg2)
                        | Op_gt -> CmpGtInt(reg1, reg1, reg2)
                        | Op_gt_eq -> CmpGeInt(reg1, reg1, reg2)
                        | Op_and -> And(reg1, reg1, reg2)
                        | Op_or -> Or(reg1, reg1, reg2)
                        |> Instruction.To.Code

                module Real =
                    (*generates code for real binary expressions*)
                    let Code(binop:binop, reg1:Reg, reg2:Reg) : Code =
                        match binop with
                        | Op_add -> AddReal(reg1, reg1, reg2)
                        | Op_sub -> SubReal(reg1, reg1, reg2)
                        | Op_div -> DivReal(reg1, reg1, reg2)
                        | Op_mul -> MulReal(reg1, reg1, reg2)
                        | Op_eq -> CmpEqReal(reg1, reg1, reg2)
                        | Op_lt -> CmpLtReal(reg1, reg1, reg2)
                        | Op_not_eq -> CmpNeReal(reg1, reg1, reg2)
                        | Op_lt_eq -> CmpLeReal(reg1, reg1, reg2)
                        | Op_gt -> CmpGtReal(reg1, reg1, reg2)
                        | Op_gt_eq -> CmpGeReal(reg1, reg1, reg2)
                        | _ -> failwith "cannot non arithmetic/comparison binop with real"
                        |> Instruction.To.Code

            module Logical =
                (*determines resultant type of binary logical expression based on the types
                of it's sub expressions. *)
                let Type(datatype1:datatype, binop:binop, datatype2:datatype) : datatype =
                    match datatype1, datatype2 with
                    | datatype.Bool, datatype.Bool -> datatype.Bool
                    | datatype.Int, datatype.Int -> datatype.Int
                    | _ -> failwith "cannot perform logical ops on non booleans/integers"

                (*generates code for logical binary expressions*)
                let Code(binop:binop, reg1:Reg, reg2:Reg) : Code =
                    match binop with
                    | Op_and -> And(reg1, reg1, reg2)
                    | Op_or -> Or(reg1, reg1, reg2)
                    | Op_eq -> CmpEqInt(reg1, reg1, reg2)
                    | Op_not_eq -> CmpNeInt(reg1, reg1, reg2)
                    | _ -> failwith "cannot non bool binop with bool"
                    |> Instruction.To.Code

            (*determines the type for a binary expression*)
            let Type(datatype1:datatype, binop:binop, datatype2:datatype) : datatype =
                match binop with
                | Op_add | Op_sub | Op_div | Op_mul | Op_not_eq 
                | Op_lt_eq | Op_gt | Op_gt_eq | Op_eq | Op_lt -> 
                    Number.Type(datatype1, binop, datatype2)
                | Op_or | Op_and -> 
                    Logical.Type(datatype1, binop, datatype2)
            
            (*generates code for binary expressions. Note that integers are converted to floats in cases
            where there is a mix of float and int. This function delegates the code generation based on 
            the type of expression (int, float or bool)*)
            let Code(datatype1:datatype, binop:binop, datatype2:datatype, reg1:Reg, reg2:Reg) : Code =
                match datatype1, datatype2 with
                | datatype.Int, datatype.Int -> Number.Int.Code(binop, reg1, reg2)
                | datatype.Float, datatype.Int -> IntToReal(reg2,reg2) :: Number.Real.Code(binop, reg1, reg2)
                | datatype.Int, datatype.Float -> IntToReal(reg1,reg1) :: Number.Real.Code(binop, reg1, reg2)
                | datatype.Float, datatype.Float -> Number.Real.Code(binop, reg1, reg2)
                | datatype.Bool, datatype.Bool -> Logical.Code(binop, reg1, reg2)
                | _ -> failwith "binops require bool-bool, int-int, int-float, float-int, or float-float datatypes"
        
        module Attr =
            (*helper function for creating expression attribute (datatype and code)*)
            let Create(code:Code, datatype:datatype) : ExprAttr =
                {
                    code = code
                    datatype = datatype
                }
        
        (*traverses an expression tree and returns an annotated version with datatypes
        and brill code (expression attribute tree) *)
        let rec ExprAttr(expr:expr, reg:Reg, env:Env, proc_arg_passby:passby) : ExprAttr = 
            match expr with
            (*bool expressions are represented by integer constants in brill*)
            | expr.Ebool(b) ->
                let bool_as_int =
                    match b with
                    | true -> 1
                    | false -> 0
                let int_const_code = [IntConst(reg, bool_as_int)]
                Attr.Create(int_const_code, datatype.Bool)

            | expr.Estring(s) ->
                let string_const_code = [StringConst(reg, s)]
                Attr.Create(string_const_code,datatype.String)

            | expr.Eint(i) -> 
                let int_const_code = [IntConst(reg, i)]
                Attr.Create(int_const_code, datatype.Int)
                
            | expr.Efloat(f) -> 
                let real_const_code = [RealConst(reg, f)]
                Attr.Create(real_const_code, datatype.Float)
                
            | expr.Ebinop(expr1,binop,expr2) -> 
                let place_1 = reg
                let place_2 = reg + 1
                let expr1_attr = ExprAttr(expr1, place_1, env, proc_arg_passby)
                let expr2_attr = ExprAttr(expr2, place_2, env, proc_arg_passby)
                let expr1_code = expr1_attr.code
                let expr2_code = expr2_attr.code
                let expr1_datatype = expr1_attr.datatype
                let expr2_datatype = expr2_attr.datatype
                let binop_code = Binop.Code(expr1_datatype, binop, expr2_datatype, place_1, place_2)
                let expr_code = expr1_code @ expr2_code @ binop_code
                let expr_datatype = Binop.Type(expr1_attr.datatype, binop, expr2_attr.datatype)
                Attr.Create(expr_code, expr_datatype)

            | expr.Eunop(unop, expr) -> 
                let expr_attr = ExprAttr(expr, reg, env, proc_arg_passby)
                let expr_code = expr_attr.code
                let expr_datatype = expr_attr.datatype
                let unop_code = Unop.Code(expr_attr.datatype, unop, reg)
                let code = expr_code @ unop_code
                Attr.Create(code, expr_datatype)
                
            | expr.Elval(lv) ->
                let lv_var = Env.Find(LValue.Id(lv),env)
                let lv_var_typedef = lv_var.typedef
                let lv_var_datatype = lv_var.datatype
                let lv_var_slotNum = lv_var.slotNum
                let lv_var_passby = lv_var.passby
                match lv with
                | LId(id) -> 
                    let load_code = 
                        (*if the variable is passed by reference then the variable must be dereferenced 
                        (using LoadIndirect) before use*)
                        match lv_var_passby with
                        | Value -> [Load(reg, lv_var_slotNum)]
                        | Reference -> [Load(reg, lv_var_slotNum);LoadIndirect(reg, reg)]
                    Attr.Create(load_code, lv_var_datatype)
                | LArrayElement(id, exprs) -> 
                    let offset_expr = Decleration.Array.Offset.Expr(lv_var_typedef, exprs)
                    let offset_expr_attr = ExprAttr(offset_expr, reg, env, proc_arg_passby)
                    let offset_expr_code = offset_expr_attr.code
                    let address_reg = reg + 1
                    (*if the array element is being used in a procedure invocation where
                    the argument is pass by reference, then the address must be used.*)
                    let proc_arg_passby_load_code =
                        match proc_arg_passby with
                        | Value -> LoadIndirect(reg, address_reg)
                        | Reference -> Move(reg, address_reg)
                    let load_code = 
                        [
                        LoadAddress(address_reg, lv_var_slotNum)
                        SubOffset(address_reg, address_reg, reg)
                        proc_arg_passby_load_code
                        ]
                    let code = offset_expr_code @ load_code
                    Attr.Create(code, lv_var_datatype)

            | expr.Eparens(expr) -> ExprAttr(expr, reg, env, proc_arg_passby)
    
    module RValue =
        let Expr(rv:rvalue) : expr =
            match rv with
            | Rexpr(expr) -> expr

    module Statement =
        module Write =
            let Instr(datatype:datatype) : Instr =
                match datatype with
                | datatype.Bool -> CallBuiltIn(BuiltIn.PrintBool)
                | datatype.Float -> CallBuiltIn(BuiltIn.PrintReal)
                | datatype.Int -> CallBuiltIn(BuiltIn.PrintInt)
                | datatype.String -> CallBuiltIn(BuiltIn.PrintString)
        
        module Read = 
            let Instr(datatype:datatype) : Instr =
                match datatype with
                | datatype.Bool -> CallBuiltIn(BuiltIn.ReadBool)
                | datatype.Float -> CallBuiltIn(BuiltIn.ReadReal)
                | datatype.Int -> CallBuiltIn(BuiltIn.ReadInt)
                | datatype.String -> failwith "cannot read string"

        let rec Code(stmt:statement, env:Env, procParams:ProcParams) : Code =
            (*generates code for multiple statements*)
            let stmts_code(stmts : statement list) =
                stmts 
                |> List.map (fun stmt -> Code(stmt, env, procParams))
                |> List.concat
           
            match stmt with
            | Write(expr) -> 
                let expr_attr = Expr.ExprAttr(expr, 0, env, Value)
                let expr_code = expr_attr.code
                let write_code = [Write.Instr(expr_attr.datatype)]
                let comment_code = [Comment("write")]
                [
                comment_code
                expr_code
                write_code
                ]
                |> List.concat
            | Assign(lv, rv) -> 
                let lv_var = Env.Find(LValue.Id(lv), env)
                let lv_var_slotNum = lv_var.slotNum
                let lv_var_datatype = lv_var.datatype
                let lv_var_decl = lv_var.typedef
                let rv_reg = 0
                let rv_expr_attr = Expr.ExprAttr(RValue.Expr(rv),rv_reg,env,Value)
                let rv_datatype = rv_expr_attr.datatype
                (*ensures that integers can be assigned to reals*)
                let rv_convert_code = 
                    match lv_var_datatype, rv_datatype with
                    | datatype.Float, datatype.Int -> [IntToReal(rv_reg,rv_reg)]
                    | _ -> []
                let rv_code = rv_expr_attr.code @ rv_convert_code
                let comment_code = [Comment("assign")]
                let store_code = 
                    match lv with
                    | LId(_) -> 
                        (*storing/assign instructions depend on whether the identifier is
                        a parameter that's passed by value or reference*)
                        match lv_var.passby with
                        | Value -> [Store(lv_var_slotNum, rv_reg)]
                        | Reference -> [Load(1,lv_var_slotNum);StoreIndirect(1,rv_reg)]
                    | LArrayElement(_,exprs) -> 
                        let offset_expr = Decleration.Array.Offset.Expr(lv_var_decl, exprs)
                        let offset_expr_reg = 1
                        let offset_expr_attr = Expr.ExprAttr(offset_expr, offset_expr_reg, env, Value)
                        let offset_expr_code = offset_expr_attr.code
                        let array_address_reg = 3
                        offset_expr_code
                        @
                        [
                        LoadAddress(array_address_reg,lv_var_slotNum)
                        SubOffset(array_address_reg,array_address_reg,offset_expr_reg)
                        StoreIndirect(array_address_reg, rv_reg)
                        ]
                [
                comment_code
                rv_code
                store_code
                ]
                |> List.concat
            | Read(lv) ->
                let lv_var = Env.Find(LValue.Id(lv), env)
                let lv_var_datatype = lv_var.datatype
                let lv_var_slotNum = lv_var.slotNum
                (*convert address to value if identifier is a parameter passed by reference*)
                let store_code = 
                    match lv_var.passby with
                    | Value -> []
                    | Reference -> [StoreIndirect(0,0)]
                    @
                    [Store(lv_var.slotNum, 0)]
                let read_code = [Read.Instr(lv_var_datatype)]
                let comment_code = [Comment("read")]
                [
                    comment_code 
                    read_code 
                    store_code
                ]
                |> List.concat
            | IfThen(expr,stmts) ->
                let expr_code = Expr.ExprAttr(expr, 0, env, Value).code
                let then_label_string = Label.New("then")
                let then_label_code = [Instr.Label(then_label_string)]
                let branch_on_false_code = [BranchOnFalse(0, then_label_string)]
                let stmts_code = stmts_code(stmts)
                let comment_code = [Comment("ifThen")]
                [
                    comment_code
                    expr_code
                    branch_on_false_code
                    stmts_code
                    then_label_code
                ]
                |> List.concat
            | IfThenElse(expr,then_stmts,else_stmts) ->
                let expr_code = Expr.ExprAttr(expr, 0, env, Value).code
                let else_label_string = Label.New("else")
                let else_label_code = [Instr.Label(else_label_string)]
                let branch_on_false_code = [BranchOnFalse(0, else_label_string)]
                let after_label_string = Label.New("after")
                let after_label_code = [Instr.Label(after_label_string)]
                let then_stmts_code = stmts_code(then_stmts)
                let else_stmts_code = stmts_code(else_stmts)
                let branch_uncond_code = [BranchUncond(after_label_string)]
                [
                    expr_code
                    branch_on_false_code
                    then_stmts_code
                    branch_uncond_code
                    else_label_code
                    else_stmts_code
                    after_label_code
                ]
                |> List.concat
                |> (fun c -> [Comment("ifThenElse")]@c)
            | WhileDo(expr, stmts) ->
                let expr_code = Expr.ExprAttr(expr, 0, env, Value).code
                let after_label_string = Label.New("after")
                let after_label_code = [Instr.Label(after_label_string)]
                let branch_on_false_code = [BranchOnFalse(0, after_label_string)]
                let stmts_code = stmts_code(stmts)
                let begin_label_string = Label.New("begin")
                let begin_label_code = [Instr.Label(begin_label_string)]
                let branch_uncond_code = [BranchUncond(begin_label_string)]
                let comment_code = [Comment("whileDo")]
                [
                    comment_code
                    begin_label_code
                    expr_code
                    branch_on_false_code
                    stmts_code
                    branch_uncond_code
                    after_label_code
                ]
                |> List.concat
            | InvokeProc(id, exprs) -> 
                let call_code = [Call(id)]
                (*the index of each expression argument in the procedure invocation must match the
                register number. i.e. argument 1's value/address must go in register 1.*)
                let reg = ref -1
                let exprs_code = 
                    exprs 
                    |> List.map (fun expr -> 
                        reg := !reg + 1
                        (*gets the passby for the current argument index*)
                        let proc_arg_passby = ProcParams.PassBy.ArgIndex(id, !reg, procParams)
                        match expr with
                        | Elval(lv) -> 
                            match lv with
                            | LId(_) -> 
                                let lv_var = Env.Find(LValue.Id(lv),env)
                                let lv_var_slotNum = lv_var.slotNum
                                (*if the argument index is a passby reference, an address must be stored in
                                the register*)
                                match proc_arg_passby with
                                | Value -> [Load(!reg, lv_var_slotNum)]
                                | Reference -> [LoadAddress(!reg, lv_var_slotNum)]
                            | LArrayElement(_,_) -> Expr.ExprAttr(expr, !reg, env, proc_arg_passby).code
                        | _ -> Expr.ExprAttr(expr, !reg, env, Value).code
                        )    
                    |> List.concat
                let comment_code = [Comment("InvokeProc")]
                [
                comment_code
                exprs_code        
                call_code
                ]
                |> List.concat
    module Statements =
        (*generates code for all statements*)
        let Code(stmts:statement list, env:Env, procParams:ProcParams) : Code = 
            stmts 
            |> List.map (fun s -> Statement.Code(s, env, procParams))
            |> List.concat

    module ProcedureBody =
        (*extracts out the declerations from a procedure body*)
        let Decls(pb:procedure_body) : decleration list =
            let (decls, _) = pb
            decls

        (*extracts out the statements from a procedure body*)
        let Stmts(pb:procedure_body) : statement list = 
            let (_,stmts) = pb
            stmts
            
    module ParameterDef =   
        (*extracts out the passby type for a parameter definition*)
        let PassBy(paramDef:parameter_def) : passby = 
            let (passby,_) = paramDef
            passby
        
        (*extracts out the type definition for a parameter definition*)
        let TypeDef(paramDef:parameter_def) : type_def = 
            let (_,typeDef) = paramDef
            typeDef

        (*extracts out the identifier for a parameter definition*)
        let Id(paramDef:parameter_def) : identifier =
            let paramdef_typedef = TypeDef(paramDef)
            Decleration.Id(paramdef_typedef)
        
        module Single =
            (*when a procedure is invoked, parameters in the calling procedure are
            stored in rgeisters. In the callee procedure, these values are stored in
            the stack for reuse.*)
            let Code(id:identifier, reg:Reg ref, env:Env) : Code = 
                reg := !reg + 1
                let var = Env.Find(id, env)
                let var_slotNum = var.slotNum
                [Store(var_slotNum, !reg)]

    module ParameterDefs =
        (*determines the total number of single parameters taking into account 
        multidimensional arrays.*)
        let Count(paramDefs:parameter_def list) : int =
            match paramDefs with
            | [] -> 0
            | _ -> 
                paramDefs
                |> List.map (fun paramDef -> 
                    let paramdef_typedef = ParameterDef.TypeDef(paramDef)
                    match paramdef_typedef with
                    | Single(_,_) -> 1
                    | Array(_,_,ranges) -> Decleration.Array.Size(ranges)                
                    )
                |> List.reduce (fun n1 n2 -> n1 + n2)

        (*adds parameter definitions to the environment*)
        let Env(paramDefs:parameter_def list, env:Env ref) = 
            let slotNum = ref 0
            paramDefs
            |> List.iter (fun paramdef -> 
                let paramdef_typedef = ParameterDef.TypeDef(paramdef)
                let param_passby = ParameterDef.PassBy(paramdef)
                match paramdef_typedef with
                | Single(datatype,id) -> 
                    Env.Var.Create(id, datatype, env, param_passby, slotNum, Single(datatype,id))
                | Array(_,_,_) -> failwith "cannot pass array as param"
                )

        (*generates code for single and array parameters (register to stack instructions)*)
        let Code(paramDefs:parameter_def list, env:Env) : Code = 
            let reg = ref -1
            paramDefs
            |> List.map (fun paramDef -> 
                let paramdef_typedef = ParameterDef.TypeDef(paramDef)
                match paramdef_typedef with
                | Single(datatype,id) -> 
                    ParameterDef.Single.Code(id, reg, env)
                | Array(datatype,id,ranges) ->
                    let slotNums = Decleration.Array.Size(ranges)
                    [for i in 0..slotNums -> i]
                    |> List.map(fun i ->
                        let varName = 
                            if i = 0 
                            then id
                            else sprintf "%s%i" id i
                        ParameterDef.Single.Code(varName, reg, env)
                        )
                    |> List.concat
                )
            |> List.concat

    module Procedure =
        (*extracts out the identifier for a procedure*)
        let Id(p:procedure) : identifier = 
            let (id,_,_) = p
            id

        (*extracts out the procedure body from a procedure*)
        let Body(p:procedure): procedure_body =
            let (_,_,pb) = p
            pb

        (*extracts out the declerations from a procedure*)
        let Decls(p:procedure) : decleration list = ProcedureBody.Decls(Body(p))
        
        (*extracts out the statements from a procedure*)
        let Stmts(p:procedure) : statement list = ProcedureBody.Stmts(Body(p))
        
        (*extracts out the parameter definitions from a procedure*)
        let ParamDefs(p:procedure) : parameter_def list = 
            let (_,paramdefs,_) = p
            paramdefs

        (*adds a procedure and it's parameters to the procedure-parameters data map*)
        let ProcParam(p:procedure, procParams:ProcParams ref) = 
            let procId = Id(p)
            let paramdefs = ParamDefs(p)
            ProcParams.AddMany(procId, paramdefs, procParams)

        (*generates code for a procedure. calculates the stack frame size for
        push/pop and adds parameters/declerations to an environment*)
        let Code(p:procedure, procParams:ProcParams) : Code = 
            let decls = Decls(p)
            let stmts = Stmts(p)
            let env = ref Env.Init
            let paramdefs = ParamDefs(p)
            ParameterDefs.Env(paramdefs, env)
            Declerations.Env(decls, env)
            let var_count = (!env).Count
            let decl_code = Declerations.Code(decls, !env)
            [
            [
            Label(Id(p))
            Comment("prologue")
            PushStackFrame(var_count)
            Comment("parameters")
            ]
            ParameterDefs.Code(paramdefs, !env)
            [Comment("declerations")]
            decl_code
            [Comment("statements")]
            Statements.Code(stmts, !env, procParams)
            [
            Comment("epilogue")
            PopStackFrame(var_count)
            Return
            ]
            ]
            |> List.concat

    module Procedures =
        (*creates procedure-parameters data map for all procedures in program*)
        let ProcParam(procs:procedure list, procParams:ProcParams ref) =
            procs
            |> List.iter (fun proc -> Procedure.ProcParam(proc, procParams)) 

        (*generates code for all procedures*)
        let Code(procs:procedure list) : Code =
            (*creates procedure-parameters map for use in procedure invocation*)
            let procParams = ref (ProcParams.Create)
            ProcParam(procs, procParams)
            procs
            |> List.map (fun proc -> Procedure.Code(proc, !procParams)) 
            |> List.concat

    module Program =
        (*generates code for entire program*)
        let Code(program:program) : Code = 
            [
            Call("main")
            Halt
            ]
            @
            Procedures.Code(program)
    
    (*generates code and formats as string*)
    let Generate(program:program) : string =
        Program.Code(program)
        |> List.map (fun i -> Instruction.To.String(i)) 
        |> String.concat "\n"
        



