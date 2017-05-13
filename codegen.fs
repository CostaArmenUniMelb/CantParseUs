namespace Snick
module codegen =
    let Debug = false

    module Label =
        let private Counter = ref 0
        let New(labelName:string) : string = 
            Counter := !Counter + 1
            labelName + string(!Counter)

    module Types =
        type Reg = int

        type SlotNum = int

        type BuiltIn =
            | PrintString
            | PrintInt
            | PrintReal
            | PrintBool
            | ReadInt
            | ReadReal
            | ReadBool

        type Label = string

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

        type Code = Instr list

        type VarId = string

        type Var = {datatype:datatype; slotNum:SlotNum; passby:passby; typedef:type_def}

        type Env = Map<VarId, Var>

        type ExprAttr = {datatype:datatype; code:Code}

        type ProcId = string

        type ProcParams = Map<ProcId, parameter_def list>

    open Types

    module ProcParams = 
        let Create : ProcParams = Map.empty

        let Find(procId:ProcId, procParams:ProcParams) : parameter_def list = 
            match procParams.TryFind procId with
            | Some(paramdefs) -> paramdefs
            | None ->  []

        let Add(procId:ProcId, paramdef:parameter_def, procParams:ProcParams ref) = 
            let paramdefs = Find(procId, !procParams)
            procParams := (!procParams).Add(procId, paramdefs @ [paramdef])

        let AddMany(procId:ProcId, paramdefs:parameter_def list, procParams:ProcParams ref) =
            let existing_paramdefs = Find(procId, !procParams)
            procParams := (!procParams).Add(procId, existing_paramdefs @ paramdefs)

        module PassBy =
            let ArgIndex(procId:ProcId, argIndex:int, procParams:ProcParams) : passby =
                let paramdefs = Find(procId, procParams)
                let paramdef = List.item(argIndex) paramdefs
                let (passby, _ ) = paramdef
                passby

            let Ref(procId:ProcId, argIndex:int, procParams:ProcParams) : bool =
                let passby = ArgIndex(procId, argIndex, procParams)
                match passby with
                | Value -> false
                | Reference -> true
    
    module BuiltIn =
        module To =
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
        module Single =
            module Store =
                let Code(passby:passby, reg:Reg, slotNum:SlotNum) : Code = 
                    match passby with
                    | Value -> []
                    | Reference -> [StoreIndirect(reg,reg)]
                    @
                    [Store(slotNum, reg)]

            module Load = 
                let Code(passby:passby, reg:Reg, slotNum:SlotNum) : Code =
                    match passby with
                    | Value -> [Load(reg, slotNum)]
                    | Reference -> [Load(reg, slotNum);LoadIndirect(reg, reg)]


        module To =
            let Code(instr:Instr) : Code = [instr]

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
                |> (fun str -> 
                    match instr with
                    | Comment(_) -> str
                    | Label(_) -> str
                    | _ -> "   " + str
                    )

    module Var =
        let Create(datatype:datatype, slotNum:SlotNum, passby:passby, typedef:type_def) : Var = 
            {datatype = datatype; slotNum = slotNum; passby = passby; typedef=typedef}

    module Env =
        let Init : Env = Map.empty 

        let Find(varId:identifier, env:Env) : Var = 
            match env.TryFind(varId) with
            | Some(var) -> var
            | None -> failwith "cannot find var in environment"

        let Add(varId:identifier, var:Var, env:Env) : Env =
            match env.TryFind(varId) with
            | Some(_) -> failwith (sprintf "var %s already exists" varId)
            | None -> env.Add(varId, var)

        module SlotNum =
            let Max(env:Env) : int = env.Count
        
        module To =
            let String(env:Env) : string = 
                env
                |> Map.toList
                |> List.map (fun (k,v) -> sprintf "%s %A %A" k (v.datatype) (v.slotNum))
                |> String.concat "\n"
                |> (fun s -> sprintf "\n-----Env-----\n%s\n-----Env-----\n" s)

    module Decleration =
        let Datatype(decl:decleration) : datatype = 
            match decl with
            | Single(datatype,_) -> datatype
            | Array(datatype,_,_) -> datatype

        let Id(decl:decleration) : identifier = 
            match decl with
            | Single(_,id) -> id
            | Array(_,id,_) -> id

        module Single =
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
            
            let Code1(decl:decleration, env:Env) : Code =
                let decl_id = Id(decl)
                Code(decl_id, env)

            let Env(id:identifier, datatype:datatype, env:Env ref, passby:passby, slotNum:SlotNum ref, typedef:type_def) =
                let decl_var = Var.Create(datatype,!slotNum, passby, typedef)
                env := Env.Add(id,decl_var,(!env))
                slotNum := !slotNum + 1
        
        module Array =
            let Ranges(decl:decleration) : range list =
                match decl with
                | Single(_,_) -> failwith "singles do not have ranges"
                | Array(_,_,ranges) -> ranges
            
            let Mins(ranges:range list) = 
                ranges 
                |> List.map (fun (s,_) -> s)

            let Sizes(ranges:range list) = 
                ranges 
                |> List.map (fun (s,e) -> e - s + 1)

            module Offset =
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
                    
                
                let Int(ranges:range list) : int =
                    ranges 
                    |> List.map (fun (rmin, rmax) -> rmax - rmin + 1) 
                    |> List.reduce (fun n1 n2 -> n1 * n2)

            let VarNames(varName:string, ranges:range list) : string list =
                [for i in 0..Offset.Int(ranges) -> i]
                |> List.map(fun i ->
                    let varName = 
                        if i = 0 
                        then varName
                        else sprintf "%s%i" varName i
                    varName
                    )


            let Env(id:identifier,datatype:datatype,env:Env ref, passby:passby, slotNum:SlotNum ref,ranges:range list) =
                VarNames(id, ranges)
                |> List.iter(fun varName ->
                    Single.Env(varName,datatype, env, passby, slotNum, Array(datatype, id, ranges))
                    )

            let Code1(decl:decleration, env:Env) : Code =
                let decl_id = Id(decl)
                let decl_ranges = Ranges(decl)
                VarNames(decl_id, decl_ranges)
                |> List.map(fun varName -> Single.Code(varName, env) )
                |> List.concat
        
        let Count(decl:decleration): int = 
            match decl with
            | Single(_,_) -> 1
            | Array(_,_,ranges) -> Array.Offset.Int(ranges)
                

    module Declerations =           
        let Code(decls:decleration list, env:Env) : Code =
            decls
            |> List.map (fun decl -> 
                match decl with
                | Single(_,_) -> Decleration.Single.Code1(decl, env)
                | Array(_,_,_) -> Decleration.Array.Code1(decl, env)
                )
            |> List.concat

        let Env(decls:decleration list, env:Env ref) =
            let slotNum = ref (Env.SlotNum.Max(!env))
            decls 
            |> List.iter (fun decl -> 
                match decl with
                | Single(datatype,id) -> 
                    Decleration.Single.Env(id,datatype, env, Value, slotNum, Single(datatype, id))
                | Array(datatype,id,ranges) -> 
                    Decleration.Array.Env(id, datatype, env, Value, slotNum, ranges)
                )

        let Count(decls:decleration list): int = 
            match decls with
            | [] -> 0
            | _ -> 
                decls
                |> List.map (fun decl -> Decleration.Count(decl))
                |> List.reduce (fun n1 n2 -> n1 + n2)

    module LValue =
        let Id(lv:lvalue) : identifier = 
            match lv with
            | LId(id) -> id
            | LArrayElement(id,_) -> id

    module Expr =
        
        module Unop =
            module Int = 
                let Code(unop:unop, reg:Reg, reg2:Reg) : Code =
                    match unop with
                    | Op_minus -> [IntConst(reg2,-1);MulInt(reg,reg,reg2)]
                    | _ -> failwith "cannot unop a non minus with int"

            module Real =
                let Code(unop:unop, reg:Reg, reg2:Reg) : Code =
                    match unop with
                    | Op_minus -> [RealConst(reg2,-1.0);MulReal(reg,reg,reg2)]
                    | _ -> failwith "cannot unop a non minus with real"

            module Bool =
                let Code(unop:unop, reg:Reg, reg2:Reg) : Code =
                    match unop with
                    | Op_not -> [Not(reg,reg)]
                    | _ -> failwith "cannot unop a non not with bool"

            let Code(datatype1:datatype, unop:unop, reg:Reg) : Code =
                let reg2 = reg + 1
                match datatype1 with
                | datatype.Int -> Int.Code(unop, reg, reg2)
                | datatype.Float -> Real.Code(unop, reg, reg2)
                | datatype.Bool -> Bool.Code(unop, reg, reg2)
                | datatype.String -> failwith "cannot unop on string"

        module Binop =
            module Number =
                let Type(datatype1:datatype, binop:binop, datatype2:datatype) : datatype =
                    match datatype1, datatype2 with
                    | datatype.Int, datatype.Int -> datatype.Int
                    | datatype.Float, datatype.Int -> datatype.Float
                    | datatype.Int, datatype.Float -> datatype.Float
                    | datatype.Float, datatype.Float -> datatype.Float
                    | datatype.Bool, datatype.Bool -> datatype.Bool
                    | _ -> failwith "cannot perform arithmetic and comparison on non numbers"

                module Int =    
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
                let Type(datatype1:datatype, binop:binop, datatype2:datatype) : datatype =
                    match datatype1, datatype2 with
                    | datatype.Bool, datatype.Bool -> datatype.Bool
                    | datatype.Int, datatype.Int -> datatype.Int
                    | _ -> failwith "cannot perform logical ops on non booleans"

                let Code(binop:binop, reg1:Reg, reg2:Reg) : Code =
                    match binop with
                    | Op_and -> And(reg1, reg1, reg2)
                    | Op_or -> Or(reg1, reg1, reg2)
                    | Op_eq -> CmpEqInt(reg1, reg1, reg2)
                    | Op_not_eq -> CmpNeInt(reg1, reg1, reg2)
                    | _ -> failwith "cannot non bool binop with bool"
                    |> Instruction.To.Code

            let Type(datatype1:datatype, binop:binop, datatype2:datatype) : datatype =
                match binop with
                | Op_add | Op_sub | Op_div | Op_mul | Op_not_eq 
                | Op_lt_eq | Op_gt | Op_gt_eq | Op_eq | Op_lt -> 
                    Number.Type(datatype1, binop, datatype2)
                | Op_or | Op_and -> 
                    Logical.Type(datatype1, binop, datatype2)

            let Code(datatype1:datatype, binop:binop, datatype2:datatype, reg1:Reg, reg2:Reg) : Code =
                match datatype1, datatype2 with
                | datatype.Int, datatype.Int -> Number.Int.Code(binop, reg1, reg2)
                | datatype.Float, datatype.Int -> IntToReal(reg2,reg2) :: Number.Real.Code(binop, reg1, reg2)
                | datatype.Int, datatype.Float -> IntToReal(reg1,reg1) :: Number.Real.Code(binop, reg1, reg2)
                | datatype.Float, datatype.Float -> Number.Real.Code(binop, reg1, reg2)
                | datatype.Bool, datatype.Bool -> Logical.Code(binop, reg1, reg2)
                | _ -> failwith "binops require bool-bool, int-int, int-float, float-int, or float-float datatypes"
        
        module Attr =
            let Create(code:Code, datatype:datatype) : ExprAttr =
                {
                    code = code
                    datatype = datatype
                }

        let rec ExprAttr(expr:expr, reg:Reg, env:Env) : ExprAttr = 
            match expr with
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
                let expr1_attr = ExprAttr(expr1, place_1, env)
                let expr2_attr = ExprAttr(expr2, place_2, env)
                let binop_code = Binop.Code(expr1_attr.datatype, binop, expr2_attr.datatype, place_1, place_2)
                let expr_code = expr1_attr.code @ expr2_attr.code @ binop_code
                let expr_datatype = Binop.Type(expr1_attr.datatype, binop, expr2_attr.datatype)
                Attr.Create(expr_code, expr_datatype)

            | expr.Eunop(unop, expr) -> 
                let expr_attr = ExprAttr(expr, reg, env)
                let expr_attr_code = expr_attr.code
                let expr_attr_datatype = expr_attr.datatype
                let unop_code = Unop.Code(expr_attr.datatype, unop, reg)
                let code = expr_attr_code @ unop_code
                Attr.Create(code, expr_attr_datatype)
                
            | expr.Elval(lv) ->
                let lv_var = Env.Find(LValue.Id(lv),env)
                let lv_var_typedef = lv_var.typedef
                let lv_var_datatype = lv_var.datatype
                let lv_var_slotNum = lv_var.slotNum
                let lv_var_passby = lv_var.passby
                match lv with
                | LId(id) -> 
                    let load_code = Instruction.Single.Load.Code(lv_var_passby, reg, lv_var_slotNum)
                    Attr.Create(load_code, lv_var_datatype)
                | LArrayElement(id, exprs) -> 
                    let offset_expr = Decleration.Array.Offset.Expr(lv_var_typedef, exprs)
                    let offset_expr_attr = ExprAttr(offset_expr, reg, env)
                    let offset_expr_code = offset_expr_attr.code
                    let load_code = 
                        let address_reg = reg + 1
                        [
                        LoadAddress(address_reg, lv_var_slotNum)
                        SubOffset(address_reg, address_reg, reg)
                        LoadIndirect(reg, address_reg)
                        ]
                    let code = offset_expr_code @ load_code
                    Attr.Create(code, lv_var_datatype)

            | expr.Eparens(expr) -> ExprAttr(expr, reg, env)
    
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
            let stmts_code(stmts) =
                stmts 
                |> List.map (fun stmt -> Code(stmt, env, procParams))
                |> List.concat
            match stmt with
            | Write(expr) -> 
                let expr_attr = Expr.ExprAttr(expr, 0, env)
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
                let rv_expr_attr = Expr.ExprAttr(RValue.Expr(rv),rv_reg,env)
                let rv_datatype = rv_expr_attr.datatype
                let rv_convert_code = 
                    match lv_var_datatype, rv_datatype with
                    | datatype.Float, datatype.Int -> [IntToReal(rv_reg,rv_reg)]
                    | _ -> []
                let rv_code = rv_expr_attr.code @ rv_convert_code
                let comment_code = [Comment("assign")]
                let store_code = 
                    match lv with
                    | LId(_) -> 
                        match lv_var.passby with
                        | Value -> [Store(lv_var_slotNum, rv_reg)]
                        | Reference -> [Load(1,lv_var_slotNum);StoreIndirect(1,rv_reg)]
                    | LArrayElement(_,exprs) -> 
                        let offset_expr = Decleration.Array.Offset.Expr(lv_var_decl, exprs)
                        let offset_expr_reg = 1
                        let offset_expr_attr = Expr.ExprAttr(offset_expr, offset_expr_reg, env)
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
                let store_code = Instruction.Single.Store.Code(lv_var.passby, 0, lv_var.slotNum)
                let read_code = [Read.Instr(lv_var_datatype)]
                let comment_code = [Comment("read")]
                [
                    comment_code 
                    read_code 
                    store_code
                ]
                |> List.concat
            | IfThen(expr,stmts) ->
                let expr_code = Expr.ExprAttr(expr, 0, env).code
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
                let expr_code = Expr.ExprAttr(expr, 0, env).code
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
                let expr_code = Expr.ExprAttr(expr, 0, env).code
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
                let reg = ref -1
                let exprs_code = 
                    exprs 
                    |> List.map (fun expr -> 
                        reg := !reg + 1
                        match expr with
                        | Elval(lv) -> 
                            match lv with
                            | LId(_) -> 
                                let lv_var = Env.Find(LValue.Id(lv),env)
                                let lv_var_slotNum = lv_var.slotNum
                                let proc_arg_passby = ProcParams.PassBy.ArgIndex(id, !reg, procParams)
                                match proc_arg_passby with
                                | Value -> [Load(!reg, lv_var_slotNum)]
                                | Reference -> [LoadAddress(!reg, lv_var_slotNum)]
                            | LArrayElement(_,_) -> Expr.ExprAttr(expr, !reg, env).code
                        | _ -> Expr.ExprAttr(expr, !reg, env).code
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
        let Code(stmts:statement list, env:Env, procParams:ProcParams) : Code = 
            stmts 
            |> List.map (fun s -> Statement.Code(s, env, procParams))
            |> List.concat

    module ProcedureBody =
        let Decls(pb:procedure_body) : decleration list =
            let (decls, _) = pb
            decls

        let Stmts(pb:procedure_body) : statement list = 
            let (_,stmts) = pb
            stmts
            
    module ParameterDef =   
        let PassBy(paramDef:parameter_def) : passby = 
            let (passby,_) = paramDef
            passby
        
        let TypeDef(paramDef:parameter_def) : type_def = 
            let (_,typeDef) = paramDef
            typeDef

        let Id(paramDef:parameter_def) : identifier =
            let paramdef_typedef = TypeDef(paramDef)
            Decleration.Id(paramdef_typedef)
        
        module Single =
            let Code(id:identifier, reg:Reg ref, env:Env) : Code = 
                reg := !reg + 1
                let var = Env.Find(id, env)
                let var_slotNum = var.slotNum
                [Store(var_slotNum, !reg)]

    module ParameterDefs =
        let Count(paramDefs:parameter_def list) : int =
            match paramDefs with
            | [] -> 0
            | _ -> 
                paramDefs
                |> List.map (fun paramDef -> 
                    let paramdef_typedef = ParameterDef.TypeDef(paramDef)
                    match paramdef_typedef with
                    | Single(_,_) -> 1
                    | Array(_,_,ranges) -> Decleration.Array.Offset.Int(ranges)                
                    )
                |> List.reduce (fun n1 n2 -> n1 + n2)

        let Env(paramDefs:parameter_def list, env:Env ref) = 
            let slotNum = ref 0
            paramDefs
            |> List.iter (fun paramdef -> 
                let paramdef_typedef = ParameterDef.TypeDef(paramdef)
                let param_passby = ParameterDef.PassBy(paramdef)
                match paramdef_typedef with
                | Single(datatype,id) -> 
                    Decleration.Single.Env(id, datatype, env, param_passby, slotNum, Single(datatype,id))
                | Array(datatype,id,ranges) -> failwith "cannot pass array as param"
                )

        let Code(paramDefs:parameter_def list, env:Env) : Code = 
            let reg = ref -1
            paramDefs
            |> List.map (fun paramDef -> 
                let paramdef_typedef = ParameterDef.TypeDef(paramDef)
                match paramdef_typedef with
                | Single(datatype,id) -> 
                    ParameterDef.Single.Code(id, reg, env)
                | Array(datatype,id,ranges) ->
                    let slotNums = Decleration.Array.Offset.Int(ranges)
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
        let Id(p:procedure) : identifier = 
            let (id,_,_) = p
            id

        let Body(p:procedure): procedure_body =
            let (_,_,pb) = p
            pb

        let Decls(p:procedure) : decleration list = ProcedureBody.Decls(Body(p))
        
        let Stmts(p:procedure) : statement list = ProcedureBody.Stmts(Body(p))
        
        let ParamDefs(p:procedure) : parameter_def list = 
            let (_,paramdefs,_) = p
            paramdefs

        let ProcParam(p:procedure, procParams:ProcParams ref) = 
            let procId = Id(p)
            let paramdefs = ParamDefs(p)
            ProcParams.AddMany(procId, paramdefs, procParams)

        let Code(p:procedure, procParams:ProcParams) : Code = 
            let decls = Decls(p)
            let stmts = Stmts(p)
            let env = ref Env.Init
            let paramdefs = ParamDefs(p)
//            let decl_count = Declerations.Count(decls)
//            let paramdef_count = ParameterDefs.Count(paramdefs)
//            let var_count = decl_count + paramdef_count
            ParameterDefs.Env(paramdefs, env)
            Declerations.Env(decls, env)
            let var_count = (!env).Count
            if Debug then Env.To.String(!env) |> System.Console.WriteLine
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
        let ProcParam(procs:procedure list, procParams:ProcParams ref) =
            procs
            |> List.iter (fun proc -> Procedure.ProcParam(proc, procParams)) 

        let Code(procs:procedure list) : Code =
            let procParams = ref (ProcParams.Create)
            ProcParam(procs, procParams)
            procs
            |> List.map (fun proc -> Procedure.Code(proc, !procParams)) 
            |> List.concat

    module Program =
        let Code(program:program) : Code = 
            [
            Call("main")
            Halt
            ]
            @
            Procedures.Code(program)

    let Generate(program:program) : string =
        Program.Code(program)
        |> List.map (fun i -> Instruction.To.String(i)) 
        |> String.concat "\n"
        



