open Snick_ast
  
module Int = struct
	let range((i:int),(n:int)) : int list =
   let rec aux acc i =
     if i <= n then
       aux (i::acc) (i+1)
     else (List.rev acc)
   in
   aux [] i
end

module Helper = struct
  module List = struct
  	let reduce fn list = 
      match list with
      | head::tail -> List.fold_left fn head tail
      | [] -> failwith "The list was empty!"
  end
end
(*creates new labels with a unique integer as a suffix*)
module Label = struct
  let counter : int ref = ref 0
  let _New(labelName:string) : string = 
    counter := (!counter) + 1;
    labelName ^ string_of_int(!counter)
end

module Types = struct
  type reg = int
  
  type slotNum = int

  (*brill built in function types*)
  type builtIn =
    | PrintString
    | PrintInt
    | PrintReal
    | PrintBool
    | ReadInt
    | ReadReal
    | ReadBool

  type label = string

  (*all brill instructions*)
  type instr =
    | Call of string
    | Halt
    | Label of string   
    | PushStackFrame of int
    | PopStackFrame of int
    | IntConst of reg * int 
    | StringConst of reg * string
    | RealConst of reg * float
    | Store of slotNum * reg 
    | Return
    | AddInt of reg * reg * reg
    | AddReal of reg * reg * reg
    | IntToReal of reg * reg
    | SubInt of reg * reg * reg
    | SubReal of reg * reg * reg
    | MulInt of reg * reg * reg
    | MulReal of reg * reg * reg
    | DivInt of reg * reg * reg
    | DivReal of reg * reg * reg
    | CallBuiltIn of builtIn
    | Load of reg * slotNum
    | CmpEqInt of reg * reg * reg
    | CmpNeInt of reg * reg * reg
    | CmpGtInt of reg * reg * reg
    | CmpGeInt of reg * reg * reg
    | CmpLtInt of reg * reg * reg
    | CmpLeInt of reg * reg * reg
    | CmpEqReal of reg * reg * reg
    | CmpNeReal of reg * reg * reg
    | CmpGtReal of reg * reg * reg
    | CmpGeReal of reg * reg * reg
    | CmpLtReal of reg * reg * reg
    | CmpLeReal of reg * reg * reg
    | And of reg * reg * reg
    | Or of reg * reg * reg
    | Not of reg * reg
    | BranchOnFalse of reg * label
    | BranchOnTrue of reg * label
    | BranchUncond of label
    | Comment of string
    | StoreIndirect of reg * reg
    | LoadIndirect of reg * reg
    | LoadAddress of reg * slotNum
    | AddOffset of reg * reg * reg
    | SubOffset of reg * reg * reg
    | Move of reg * reg 

  type code = instr list

  type varId = string

  (*variables (declerations and parameters) in a procedures environment*)
  type var = {datatype:datatype; slotNum:slotNum; passby:passby; typedef:type_def}

  (*environment containing all variables*)
  type env = (varId * var) list

  (*expression attributes. Parent nodes require data type and code from children during 
  expresssion code generation*)
  type exprAttr = {datatype:datatype; code:code}

  type procId = string

  (*holds all procedures and there parameter definitions. 
  Used in code generation for procedure invocation with reference parameters*)
  type procParams = (procId * parameter_def list) list
end

open Types

module ProcParams = struct
  let create : procParams = []

	let add((procId:procId), (paramDefs:parameter_def list), (procParams:procParams)) : procParams =
		(procId, paramDefs) :: procParams

  (*get paremeter definitions for a procedure*)
  let find ((procId:procId), (procParams:procParams)) : parameter_def list = 
    procParams 
		|> List.find (fun (pId, _) -> pId = procId)
		|> (fun (_,paramDefs) -> paramDefs)

  (*add parameter definitions to a procedure*)
  let addMany((procId:procId), (paramdefs:parameter_def list), (procParams:procParams ref)) =
    let existing_paramdefs = find(procId, !procParams) in
    procParams := add(procId, existing_paramdefs @ paramdefs, !procParams)

  module PassBy = struct
    (*gets the passby type (i.e. ref or val) for a procedure at a given argument index*)
    let argIndex((procId:procId), (argIndex:int), (procParams:procParams)) : passby =
      let paramdefs = find(procId, procParams) in
      let paramdef = List.nth paramdefs argIndex in
      let (passby, _, _) = paramdef in
      passby
					
    (*is argument index in procedure passby ref*)
    let ref((procId:procId), (argIndx:int), (procParams:procParams)) : bool =
      let passby = argIndex(procId, argIndx, procParams) in
      match passby with
      | Value -> false
      | Reference -> true
	end
end

module BuiltIn = struct
  module To = struct
		(*converts builtin brill types to it's string code form*)
    let string(builtin:builtIn) : string =
      match builtin with
      | PrintInt -> "print_int"
      | PrintReal -> "print_real"
      | PrintBool -> "print_bool"
      | PrintString -> "print_string"
      | ReadInt -> "read_int"
      | ReadReal -> "read_float"
      | ReadBool -> "read_bool"
	end
end

module Instruction = struct
  module To = struct
		(*takes a single instruction and converts it to a list of instructions (Code)*)
    let code(instr:instr) : code = [instr]

    (*converts brill instruction type into it's printable brill code form*)
    let string(instr:instr) : string =
      match instr with
      | IntConst(r, i) -> Printf.sprintf "int_const r%i, %i" r i
      | RealConst(r, f) -> Printf.sprintf "real_const r%i, %.2f" r f
      | StringConst(r, s) -> Printf.sprintf "string_const r%i, \"%s\"" r s
      | Store(sn,r) -> Printf.sprintf "store %i, r%i" sn r
      | Label(l) -> Printf.sprintf "%s:" l
      | PushStackFrame(i) -> Printf.sprintf "push_stack_frame %i" i
      | PopStackFrame(i) -> Printf.sprintf "pop_stack_frame %i" i
      | Call(n) -> Printf.sprintf "call %s" n
      | Halt -> "halt"
      | Return -> "return"
      | CallBuiltIn(bi) -> Printf.sprintf "call_builtin %s" (BuiltIn.To.string(bi))
      | AddInt(r1,r2,r3) -> Printf.sprintf "add_int r%i, r%i, r%i" r1 r2 r3
      | AddReal(r1,r2,r3) -> Printf.sprintf "add_real r%i, r%i, r%i" r1 r2 r3
      | IntToReal(r1,r2) -> Printf.sprintf "int_to_real r%i, r%i" r1 r2
      | SubInt(r1,r2,r3) -> Printf.sprintf "sub_int r%i, r%i, r%i" r1 r2 r3
      | SubReal(r1,r2,r3) -> Printf.sprintf "sub_real r%i, r%i, r%i" r1 r2 r3
      | DivInt(r1,r2,r3) -> Printf.sprintf "div_int r%i, r%i, r%i" r1 r2 r3
      | DivReal(r1,r2,r3) -> Printf.sprintf "div_real r%i, r%i, r%i" r1 r2 r3
      | MulInt(r1,r2,r3) -> Printf.sprintf "mul_int r%i, r%i, r%i" r1 r2 r3
      | MulReal(r1,r2,r3) -> Printf.sprintf "mul_real r%i, r%i, r%i" r1 r2 r3
      | Load(r,sn) -> Printf.sprintf "load r%i, %i" r sn
      | CmpEqInt(r1,r2,r3) -> Printf.sprintf "cmp_eq_int r%i, r%i, r%i" r1 r2 r3
      | CmpNeInt(r1,r2,r3) -> Printf.sprintf "cmp_ne_int r%i, r%i, r%i" r1 r2 r3
      | CmpGtInt(r1,r2,r3) -> Printf.sprintf "cmp_gt_int r%i, r%i, r%i" r1 r2 r3
      | CmpGeInt(r1,r2,r3) -> Printf.sprintf "cmp_ge_int r%i, r%i, r%i" r1 r2 r3
      | CmpLtInt(r1,r2,r3) -> Printf.sprintf "cmp_lt_int r%i, r%i, r%i" r1 r2 r3
      | CmpLeInt(r1,r2,r3) -> Printf.sprintf "cmp_le_int r%i, r%i, r%i" r1 r2 r3
      | CmpEqReal(r1,r2,r3) -> Printf.sprintf "cmp_eq_real r%i, r%i, r%i" r1 r2 r3
      | CmpNeReal(r1,r2,r3) -> Printf.sprintf "cmp_ne_real r%i, r%i, r%i" r1 r2 r3
      | CmpGtReal(r1,r2,r3) -> Printf.sprintf "cmp_gt_real r%i, r%i, r%i" r1 r2 r3
      | CmpGeReal(r1,r2,r3) -> Printf.sprintf "cmp_ge_real r%i, r%i, r%i" r1 r2 r3
      | CmpLtReal(r1,r2,r3) -> Printf.sprintf "cmp_lt_real r%i, r%i, r%i" r1 r2 r3
      | CmpLeReal(r1,r2,r3) -> Printf.sprintf "cmp_le_real r%i, r%i, r%i" r1 r2 r3
      | And(r1,r2,r3) -> Printf.sprintf "and r%i, r%i, r%i" r1 r2 r3
      | BranchOnFalse(r1,l) -> Printf.sprintf "branch_on_false r%i, %s" r1 l 
      | BranchOnTrue(r1,l) -> Printf.sprintf "branch_on_true r%i, %s" r1 l 
      | BranchUncond(l) -> Printf.sprintf "branch_uncond %s" l 
      | Comment(c) -> Printf.sprintf "# %s" c
      | Not(r1,r2) -> Printf.sprintf "not r%i, r%i" r1 r2
      | Or(r1,r2,r3) -> Printf.sprintf "or r%i, r%i, r%i" r1 r2 r3
      | StoreIndirect(r1,r2) -> Printf.sprintf "store_indirect r%i, r%i" r1 r2
      | LoadIndirect(r1,r2) -> Printf.sprintf "load_indirect r%i, r%i" r1 r2
      | LoadAddress(r,slotnum) -> Printf.sprintf "load_address r%i, %i" r slotnum
      | AddOffset(r1,r2,r3) -> Printf.sprintf "add_offset r%i, r%i, r%i" r1 r2 r3
      | SubOffset(r1,r2,r3) -> Printf.sprintf "sub_offset r%i, r%i, r%i" r1 r2 r3
      | Move(r1, r2) -> Printf.sprintf "move r%i, r%i" r1 r2
      |> (fun str -> 
          match instr with
          | Comment(_) -> str
          | Label(_) -> str
          | _ -> "   " ^ str
          )
	end
end

module Var = struct
  (*helper function to create var record*)
  let create((datatype:datatype), (slotNum:slotNum), (passby:passby), (typedef:type_def)) : var = 
   {datatype = datatype; slotNum = slotNum; passby = passby; typedef=typedef}
end

module Env = struct
  let init : env = [] 
	
  module SlotNum = struct
    (*gets slotnumber count in environment*)
    let max(env:env) : int = List.length(env)
  end
				
  module Var = struct
    (*get variable in procedure's environment*)
    let find((varId:identifier), (env:env)) : var = 
      env 
  		|> List.find (fun (vId, _) -> vId = varId)
  		|> (fun (_,var) -> var)
		
		(*add variable to environment*)
    let add((varId:identifier), (var:var), (env:env)) : env =
      (varId, var) :: env
			
		(*creates a variable and adds it to the environment*)
    let create((id:identifier), (datatype:datatype), (env:env ref), (passby:passby), 
			(slotNum:slotNum ref), (typedef:type_def)) =
      let decl_var = Var.create(datatype,!slotNum, passby, typedef) in
      env := add(id,decl_var,(!env));
      slotNum := !slotNum + 1
  end
end
	
module Decleration = struct
  (*gets datatype in decleration*)
  let datatype(decl:decleration) : datatype = 
    match decl with
    | (Single(datatype,_),_) -> datatype
    | (Array(datatype,_,_),_) -> datatype

  (*gets the identifier in a decleration*)
  let id(decl:decleration) : identifier = 
    match decl with
    | (Single(_,id),_) -> id
    | (Array(_,id,_),_) -> id	

  module Single = struct
    (*generates code for the initialisation of single (not arrays) declared variables only.
    Note that arrays are converted to singles prior to initialisation. 
    i.e. Array(a[1..n]) = Single(a[1]),..,Single(a[n]).
    Declared variables are initialised (i.e. int -> 0, bool -> false) into registers and 
    then stored in a stack slot. Stackslots are pre-determined in the environment*)
    let code((varId:varId), (env:env)) : code = 
      let decl_var = Env.Var.find(varId, env) in
      let decl_var_slotnum = decl_var.slotNum in
      let decl_datatype = decl_var.datatype in
      let reg = 0 in
      let store_instr = Store(decl_var_slotnum, reg) in
      let const_instr =
        match decl_datatype with
        | Int -> IntConst(reg, 0)
        | Float -> RealConst(reg, 0.0)
        | Bool -> IntConst(reg, 0)
        | String -> StringConst(reg, "") in
      [
      const_instr;
      store_instr
      ]
            
    (*Overloaded function for Single.Code above*)
    let code1((decl:decleration), (env:env)) : code =
      let decl_id = id(decl) in
      code(decl_id, env)
	end
	
	module Array = struct
    (*extracts out the ranges for an array decleration*)
    let ranges(decl:decleration) : range list =
      match decl with
      | (Single(_,_),_) -> failwith "singles do not have ranges"
      | (Array(_,_,ranges),_) -> ranges
            
    (*extracts out all the min parts within a set of ranges*)
    let mins(ranges:range list) = 
      ranges 
      |> List.map (fun (s,_) -> s)

    (*for a set of ranges, calculates the size of each range*)
    let sizes(ranges:range list) = 
      ranges 
      |> List.map (fun (s,e) -> e - s + 1)

		module Offset = struct
      (*creates a snick expression for the row-major index into an 
      array from a set of snick expressions which describe the position in a 
      multidimensional array.*)
      let expr((decl:decleration), (exprs:expr list)) : expr = 
        let exprs_count = List.length(exprs) in
        let ranges = ranges(decl) in
        let ranges_mins = mins(ranges) in
        let ranges_sizes = sizes(ranges) in
        (* Sum[1,d]( Prod[j=i+1,d]( N[j] ) * n[i]) where N[j] is size of dim, n[i] is index in array, d = dim number *)
        let offset_inner_product(j, d, ranges_sizes) : expr = 
          if 
            j > d
          then 
            Eint(1, Expr_None)
          else
            Int.range(j,d) 
            |> List.map (fun n -> List.nth (ranges_sizes) (n-1) )
            |> Helper.List.reduce (fun n1 n2 -> n1 * n2)
            |> (fun n -> Eint(n, Expr_None)) in
        let offset_outer_sum(d, exprs, ranges_sizes) : expr = 
          let i_to_d = Int.range(1,d) in 
          let index_exprs = 
              List.combine exprs ranges_mins
              |> List.map (fun (e, s) -> Ebinop(e, Op_sub, Eint(s, Expr_None),Expr_None)) in
          List.combine index_exprs i_to_d
          |> List.map (fun (e, i) -> Ebinop(e, Op_mul, offset_inner_product(i+1, d, ranges_sizes), Expr_None))
          |> Helper.List.reduce (fun e1 e2 -> Ebinop(e1, Op_add, e2, Expr_None)) in
        offset_outer_sum(exprs_count, exprs, ranges_sizes)
		end
		
		(*calculates the size of a multidimensional array*)
    let size(ranges:range list) : int =
      ranges 
      |> List.map (fun (rmin, rmax) -> rmax - rmin + 1) 
      |> Helper.List.reduce (fun n1 n2 -> n1 * n2)
            
    (*creates a set of var names for all elements of a multidimensional array.
    the first var has no suffix and all var names after it have a sequential number
    as a suffix. i.e. varName, varName2, varName3,.., varNameN *)
    let varNames((varName:string), (ranges:range list)) : string list =
      Int.range(0,size(ranges))
      |> List.map(fun i ->
        let varName = 
          if i = 0 
          then varName
          else Printf.sprintf "%s%i" varName i
				in
        varName
        )

    (*creates variables for the environment for all elements of a multidimensional array*)
    let env((id:identifier),(datatype:datatype),(env:env ref),(passby:passby),(slotNum:slotNum ref),
			(ranges:range list)) =
      varNames(id, ranges)
      |> List.iter(fun varName ->
        Env.Var.create(varName,datatype, env, passby, slotNum, Array(datatype, id, ranges))
        )

    (*generates code for the intialisation of all elements in a multidimensional array*)
    let code((decl:decleration), (env:env)) : code =
      let decl_id = id(decl) in
      let decl_ranges = ranges(decl) in
      varNames(decl_id, decl_ranges)
      |> List.map(fun varName -> Single.code(varName, env) )
      |> List.concat
	end
	
	(*determines the size of a decleration taking into account the size of multidimensional array types*)
  let count(decl:decleration): int = 
    match decl with
    | (Single(_,_),_) -> 1
    | (Array(_,_,ranges),_) -> Array.size(ranges)
end

module Declerations = struct        
  (*generates code for all declared variable initialisations*)
  let code((decls:decleration list), (env:env)) : code =
    decls
    |> List.map (fun decl -> 
      match decl with
      | (Single(_,_),_) -> Decleration.Single.code1(decl, env)
      | (Array(_,_,_),_) -> Decleration.Array.code(decl, env)
      )
    |> List.concat

  (*adds declared variables to the environment*)
  let env((decls:decleration list), (env:env ref)) =
    let slotNum = ref (Env.SlotNum.max(!env)) in
    decls 
    |> List.iter (fun decl -> 
      match decl with
      | (Single(datatype,id),_) -> 
        Env.Var.create(id,datatype, env, Value, slotNum, Single(datatype, id))
      | (Array(datatype,id,ranges),_) -> 
        Decleration.Array.env(id, datatype, env, Value, slotNum, ranges)
      )

  (*determines the total number of declerations with multidimensional arrays flattened*)
  let count(decls:decleration list): int = 
    match decls with
    | [] -> 0
    | _ -> 
      decls
      |> List.map (fun decl -> Decleration.count(decl))
      |> Helper.List.reduce (fun n1 n2 -> n1 + n2)
end

module LValue = struct
        (*extracts out the identifier in the lvalue type*)
        let id(lv:lvalue) : identifier = 
            match lv with
            | LId(id,_) -> id
            | LArrayElement(id,_,_) -> id
end

module Expr = struct
  module Unop = struct
      module Int = struct
        (*generates code for a unary integer expression*)
        let code((unop:unop), (reg:reg), (reg2:reg)) : code =
          match unop with
          | Op_minus -> [IntConst(reg2,-1);MulInt(reg,reg,reg2)]
          | _ -> failwith "cannot unop a non minus with int"

			end

      module Real = struct
        (*generates code for a unary real expression*)
        let code((unop:unop), (reg:reg), (reg2:reg)) : code =
          match unop with
          | Op_minus -> [RealConst(reg2,-1.0);MulReal(reg,reg,reg2)]
          | _ -> failwith "cannot unop a non minus with real"
			end

      module Bool = struct
        (*generates code for a unary bool expression*)
        let code((unop:unop), (reg:reg), (reg2:reg)) : code =
          match unop with
          | Op_not -> [Not(reg,reg)]
          | _ -> failwith "cannot unop a non not with bool"
			end
            
      (*delegates code generation based on type of expression*)
      let code((datatype1:datatype), (unop:unop), (reg:reg)) : code =
          let reg2 = reg + 1 in
          match datatype1 with
          | Int -> Int.code(unop, reg, reg2)
          | Float -> Real.code(unop, reg, reg2)
          | Bool -> Bool.code(unop, reg, reg2)
          | String -> failwith "cannot unop on string"
	end
	
	module Binop = struct
    module Number = struct
      (*determines resultant type of binary expression based on the types
      of it's sub expressions. *)
      let _type((datatype1:datatype), (binop:binop), (datatype2:datatype)) : datatype =
        match datatype1, datatype2 with
        | Int, Int -> Int
        | Float, Int -> Float
        | Int, Float -> Float
        | Float, Float -> Float
        | Bool, Bool -> Bool
        | _ -> failwith "cannot perform arithmetic and comparison on non numbers"

			module Int = struct  
        (*generates code for integer binary expressions*)
        let code((binop:binop), (reg1:reg), (reg2:reg)) : code =
          let instr =
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
					in
          Instruction.To.code(instr)

			end

      module Real = struct
        (*generates code for real binary expressions*)
        let code((binop:binop), (reg1:reg), (reg2:reg)) : code =
          let instr =
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
          in
					Instruction.To.code(instr)
			end
		end
			
		module Logical = struct
      (*determines resultant type of binary logical expression based on the types
      of it's sub expressions. *)
      let _type((datatype1:datatype), (binop:binop), (datatype2:datatype)) : datatype =
        match datatype1, datatype2 with
        | Bool, Bool -> Bool
        | Int, Int -> Int
        | _ -> failwith "cannot perform logical ops on non booleans/integers"

      (*generates code for logical binary expressions*)
      let code((binop:binop), (reg1:reg), (reg2:reg)) : code =
        let instr =
  				match binop with
          | Op_and -> And(reg1, reg1, reg2)
          | Op_or -> Or(reg1, reg1, reg2)
          | Op_eq -> CmpEqInt(reg1, reg1, reg2)
          | Op_not_eq -> CmpNeInt(reg1, reg1, reg2)
          | _ -> failwith "cannot non bool binop with bool"
        in
				Instruction.To.code(instr)
		end
		
    (*determines the type for a binary expression*)
    let _type((datatype1:datatype), (binop:binop), (datatype2:datatype)) : datatype =
      match binop with
      | Op_add | Op_sub | Op_div | Op_mul | Op_not_eq 
      | Op_lt_eq | Op_gt | Op_gt_eq | Op_eq | Op_lt -> 
          Number._type(datatype1, binop, datatype2)
      | Op_or | Op_and -> 
          Logical._type(datatype1, binop, datatype2)
          
    (*generates code for binary expressions. Note that integers are converted to floats in cases
    where there is a mix of float and int. This function delegates the code generation based on 
    the type of expression (int, float or bool)*)
    let code((datatype1:datatype), (binop:binop), (datatype2:datatype), 
			(reg1:reg), (reg2:reg)) : code =
      match datatype1, datatype2 with
      | Int, Int -> Number.Int.code(binop, reg1, reg2)
      | Float, Int -> IntToReal(reg2,reg2) :: Number.Real.code(binop, reg1, reg2)
      | Int, Float -> IntToReal(reg1,reg1) :: Number.Real.code(binop, reg1, reg2)
      | Float, Float -> Number.Real.code(binop, reg1, reg2)
      | Bool, Bool -> Logical.code(binop, reg1, reg2)
			| _ -> failwith "binops require bool-bool, int-int, int-float, float-int, or float-float datatypes"
	end
	
	module Attr = struct
    (*helper function for creating expression attribute (datatype and code)*)
    let create((c:code), (datatype:datatype)) : exprAttr =
      {
          code = c;
          datatype = datatype
      }
	end
        
  (*traverses an expression tree and returns an annotated version with datatypes
  and brill code (expression attribute tree) *)
  let rec exprAttr((expr:expr), (reg:reg), (env:env), (proc_arg_passby:passby)) : exprAttr = 
		match expr with
    (*bool expressions are represented by integer constants in brill*)
    | Ebool(b,_) ->
      let bool_as_int =
        match b with
        | true -> 1
        | false -> 0 in
      let int_const_code = [IntConst(reg, bool_as_int)] in
      Attr.create(int_const_code, Bool)

    | Estring(s, _) ->
      let string_const_code = [StringConst(reg, s)] in
      Attr.create(string_const_code,String)

    | Eint(i, _) -> 
      let int_const_code = [IntConst(reg, i)] in
      Attr.create(int_const_code, Int)
                
    | Efloat(f, _) ->
      let real_const_code = [RealConst(reg, f)] in
      Attr.create(real_const_code, Float)
                
    | Ebinop(expr1,binop,expr2,_) -> 
      let place_1 = reg in
      let place_2 = reg + 1 in
      let expr1_attr = exprAttr(expr1, place_1, env, proc_arg_passby) in
      let expr2_attr = exprAttr(expr2, place_2, env, proc_arg_passby) in
      let expr1_code = expr1_attr.code in
      let expr2_code = expr2_attr.code in
      let expr1_datatype = expr1_attr.datatype in
      let expr2_datatype = expr2_attr.datatype in
      let binop_code = Binop.code(expr1_datatype, binop, expr2_datatype, place_1, place_2) in
      let expr_code = expr1_code @ expr2_code @ binop_code in
      let expr_datatype = Binop._type(expr1_attr.datatype, binop, expr2_attr.datatype) in
      Attr.create(expr_code, expr_datatype)

    | Eunop(unop, expr, _) -> 
      let expr_attr = exprAttr(expr, reg, env, proc_arg_passby) in
      let expr_code = expr_attr.code in
      let expr_datatype = expr_attr.datatype in
      let unop_code = Unop.code(expr_attr.datatype, unop, reg) in
      let code = expr_code @ unop_code in
      Attr.create(code, expr_datatype)
                
    | Elval(lv, _) ->
      let lv_var = Env.Var.find(LValue.id(lv),env) in
      let lv_var_typedef = lv_var.typedef in
      let lv_var_datatype = lv_var.datatype in
      let lv_var_slotNum = lv_var.slotNum in
      let lv_var_passby = lv_var.passby in
			let exprAttr =
  			match lv with
        | LId(id, _) -> 
          let load_code = 
            (*if the variable is passed by reference then the variable must be dereferenced 
            (using LoadIndirect) before use*)
            match lv_var_passby with
            | Value -> [Load(reg, lv_var_slotNum)]
            | Reference -> [Load(reg, lv_var_slotNum);LoadIndirect(reg, reg)] in
          Attr.create(load_code, lv_var_datatype)
        | LArrayElement(id, exprs, _) -> 
          let offset_expr = Decleration.Array.Offset.expr((lv_var_typedef, Expr_None), exprs) in
          let offset_expr_attr = exprAttr(offset_expr, reg, env, proc_arg_passby) in
          let offset_expr_code = offset_expr_attr.code in
          let address_reg = reg + 1 in
          (*if the array element is being used in a procedure invocation where
          the argument is pass by reference, then the address must be used.*)
          let proc_arg_passby_load_code =
            match proc_arg_passby with
            | Value -> LoadIndirect(reg, address_reg)
            | Reference -> Move(reg, address_reg) in
          let load_code = 
            [
            LoadAddress(address_reg, lv_var_slotNum);
            SubOffset(address_reg, address_reg, reg);
            proc_arg_passby_load_code
            ] in
          let code = offset_expr_code @ load_code in
          Attr.create(code, lv_var_datatype) in
			exprAttr

  	| Eparens(expr, _) -> exprAttr(expr, reg, env, proc_arg_passby)
end

module RValue = struct
  let expr(rv:rvalue) : expr =
    match rv with
    | Rexpr(expr) -> expr
end

module Statement = struct
  
	module Write = struct
    let instr(datatype:datatype) : instr =
      match datatype with
      | Bool -> CallBuiltIn(PrintBool)
      | Float -> CallBuiltIn(PrintReal)
      | Int -> CallBuiltIn(PrintInt)
      | String -> CallBuiltIn(PrintString)

	end
  
  module Read = struct
    let instr(datatype:datatype) : instr =
      match datatype with
      | Bool -> CallBuiltIn(ReadBool)
      | Float -> CallBuiltIn(ReadReal)
      | Int -> CallBuiltIn(ReadInt)
      | String -> failwith "cannot read string"
	end

	let rec code((stmt:statement), (env:env), (procParams:procParams)) : code =
    (*generates code for multiple statements*)
    let stmts_code(stmts : statement list) =
        stmts 
        |> List.map (fun stmt -> code(stmt, env, procParams))
        |> List.concat in        
    match stmt with
    | Write(expr) -> 
        let expr_attr = Expr.exprAttr(expr, 0, env, Value) in
        let expr_code = expr_attr.code in
        let write_code = [Write.instr(expr_attr.datatype)] in
        let comment_code = [Comment("write")] in
        [
        comment_code;
        expr_code;
        write_code
        ]
        |> List.concat
 
    | Assign(lv, rv) -> 
        let lv_var = Env.Var.find(LValue.id(lv), env) in
        let lv_var_slotNum = lv_var.slotNum in
        let lv_var_datatype = lv_var.datatype in
        let lv_var_decl = lv_var.typedef in
        let rv_reg = 0 in
        let rv_expr_attr = Expr.exprAttr(RValue.expr(rv),rv_reg,env,Value) in
        let rv_datatype = rv_expr_attr.datatype in
        (*ensures that integers can be assigned to reals*)
        let rv_convert_code = 
            match lv_var_datatype, rv_datatype with
            | Float, Int -> [IntToReal(rv_reg,rv_reg)]
            | _ -> [] in
        let rv_code = rv_expr_attr.code @ rv_convert_code in
        let comment_code = [Comment("assign")] in
        let store_code = 
            match lv with
            | LId(_,_) -> 
                (*storing/assign instructions depend on whether the identifier is
                a parameter that's passed by value or reference*)
                let code = 
  								match lv_var.passby with
                  | Value -> [Store(lv_var_slotNum, rv_reg)]
                  | Reference -> [Load(1,lv_var_slotNum);StoreIndirect(1,rv_reg)] in
								code
            | LArrayElement(_,exprs,_) -> 
                let offset_expr = Decleration.Array.Offset.expr((lv_var_decl, Expr_None), exprs) in
                let offset_expr_reg = 1 in
                let offset_expr_attr = Expr.exprAttr(offset_expr, offset_expr_reg, env, Value) in
                let offset_expr_code = offset_expr_attr.code in
                let array_address_reg = 3 in
                offset_expr_code
                @
                [
                LoadAddress(array_address_reg,lv_var_slotNum);
                SubOffset(array_address_reg,array_address_reg,offset_expr_reg);
                StoreIndirect(array_address_reg, rv_reg)
                ]
				in
        [
        comment_code;
        rv_code;
        store_code
        ]
        |> List.concat

    | Read(lv) ->
        let lv_var = Env.Var.find(LValue.id(lv), env) in
        let lv_var_datatype = lv_var.datatype in
        let lv_var_slotNum = lv_var.slotNum in
        (*convert address to value if identifier is a parameter passed by reference*)
        let store_code = 
            match lv_var.passby with
            | Value -> []
            | Reference -> [StoreIndirect(0,0)]
            @
            [Store(lv_var.slotNum, 0)] in
        let read_code = [Read.instr(lv_var_datatype)] in
        let comment_code = [Comment("read")] in
        [
            comment_code;
            read_code;
            store_code
        ]
        |> List.concat

    | IfThen(expr,stmts) ->
        let expr_code = (Expr.exprAttr(expr, 0, env, Value)).code in
        let then_label_string = Label._New("then") in
        let then_label_code = [Label(then_label_string)] in
        let branch_on_false_code = [BranchOnFalse(0, then_label_string)] in
        let stmts_code = stmts_code(stmts) in
        let comment_code = [Comment("ifThen")] in
        [
            comment_code;
            expr_code;
            branch_on_false_code;
            stmts_code;
            then_label_code
        ]
        |> List.concat
 
    | IfThenElse(expr,then_stmts,else_stmts) ->
        let expr_code = (Expr.exprAttr(expr, 0, env, Value)).code in
        let else_label_string = Label._New("else") in
        let else_label_code = [Label(else_label_string)] in
        let branch_on_false_code = [BranchOnFalse(0, else_label_string)] in
        let after_label_string = Label._New("after") in
        let after_label_code = [Label(after_label_string)] in
        let then_stmts_code = stmts_code(then_stmts) in
        let else_stmts_code = stmts_code(else_stmts) in
        let branch_uncond_code = [BranchUncond(after_label_string)] in
        [
            expr_code;
            branch_on_false_code;
            then_stmts_code;
            branch_uncond_code;
            else_label_code;
            else_stmts_code;
            after_label_code
        ]
        |> List.concat
        |> (fun c -> [Comment("ifThenElse")]@c)

    | WhileDo(expr, stmts) ->
        let expr_code = (Expr.exprAttr(expr, 0, env, Value)).code in
        let after_label_string = Label._New("after") in
        let after_label_code = [Label(after_label_string)] in
        let branch_on_false_code = [BranchOnFalse(0, after_label_string)] in
        let stmts_code = stmts_code(stmts) in
        let begin_label_string = Label._New("begin") in
        let begin_label_code = [Label(begin_label_string)] in
        let branch_uncond_code = [BranchUncond(begin_label_string)] in
        let comment_code = [Comment("whileDo")] in
        [
            comment_code;
            begin_label_code;
            expr_code;
            branch_on_false_code;
            stmts_code;
            branch_uncond_code;
            after_label_code
        ]
        |> List.concat

    | InvokeProc(id, exprs) -> 
      let call_code = [Call(id)] in
      (*the index of each expression argument in the procedure invocation must match the
      register number. i.e. argument 1's value/address must go in register 1.*)
      let reg = ref (-1) in
			let exprs_code = 
          exprs 
          |> List.map (fun expr -> 
              reg := !reg + 1;
              (*gets the passby for the current argument index*)
              let proc_arg_passby = ProcParams.PassBy.argIndex(id, !reg, procParams) in
              match expr with
              | Elval(lv,_) -> 
                  match lv with
                  | LId(_,_) -> 
                      let lv_var = Env.Var.find(LValue.id(lv),env) in
                      let lv_var_slotNum = lv_var.slotNum in
                      (*if the argument index is a passby reference, an address must be stored in
                      the register*)
                      let code = 
  											match proc_arg_passby with
                        | Value -> [Load(!reg, lv_var_slotNum)]
                        | Reference -> [LoadAddress(!reg, lv_var_slotNum)] in
											code
                  | LArrayElement(_,_,_) -> (Expr.exprAttr(expr, !reg, env, proc_arg_passby)).code
              | _ -> (Expr.exprAttr(expr, !reg, env, Value)).code
              )    
          |> List.concat in
      let comment_code = [Comment("InvokeProc")] in
      [
      comment_code;
      exprs_code;
      call_code
      ]
      |> List.concat
end

module Statements = struct
  (*generates code for all statements*)
  let code((stmts:statement list), (env:env), (procParams:procParams)) : code = 
    stmts 
    |> List.map (fun s -> Statement.code(s, env, procParams))
    |> List.concat
end

module ProcedureBody = struct
  (*extracts out the declerations from a procedure body*)
  let decls(pb:procedure_body) : decleration list =
    let (decls, _) = pb in
    decls

  (*extracts out the statements from a procedure body*)
  let stmts(pb:procedure_body) : statement list = 
    let (_,stmts) = pb in
    stmts			
end
            
module ParameterDef =  struct
  (*extracts out the passby type for a parameter definition*)
  let passBy(paramDef:parameter_def) : passby = 
    let (passby,_,_) = paramDef in
    passby
        
  (*extracts out the type definition for a parameter definition*)
  let typeDef(paramDef:parameter_def) : type_def = 
    let (_,typeDef,_) = paramDef in
    typeDef

  (*extracts out the identifier for a parameter definition*)
  let id(paramDef:parameter_def) : identifier =
    let paramdef_typedef = typeDef(paramDef) in
    Decleration.id((paramdef_typedef, Expr_None))
		
	module Single = struct
    (*when a procedure is invoked, parameters in the calling procedure are
    stored in rgeisters. In the callee procedure, these values are stored in
    the stack for reuse.*)
    let code((id:identifier), (reg:reg ref), (env:env)) : code = 
      reg := !reg + 1;
      let var = Env.Var.find(id, env) in
      let var_slotNum = var.slotNum in
      [Store(var_slotNum, (!reg))]
	end
						
end
	
module ParameterDefs = struct
  (*determines the total number of single parameters taking into account 
  multidimensional arrays.*)
  let count(paramDefs:parameter_def list) : int =
    match paramDefs with
    | [] -> 0
    | _ -> 
      paramDefs
      |> List.map (fun paramDef -> 
        let paramdef_typedef = ParameterDef.typeDef(paramDef) in
        match paramdef_typedef with
        | Single(_,_) -> 1
        | Array(_,_,ranges) -> Decleration.Array.size(ranges)                
        )
      |> Helper.List.reduce (fun n1 n2 -> n1 + n2)

  (*adds parameter definitions to the environment*)
  let env((paramDefs:parameter_def list), (env:env ref)) = 
    let slotNum = ref 0 in
    paramDefs
    |> List.iter (fun paramdef -> 
      let paramdef_typedef = ParameterDef.typeDef(paramdef) in
      let param_passby = ParameterDef.passBy(paramdef) in
      match paramdef_typedef with
      | Single(datatype,id) -> 
        Env.Var.create(id, datatype, env, param_passby, slotNum, Single(datatype,id))
      | Array(_,_,_) -> failwith "cannot pass array as param"
      )

  (*generates code for single and array parameters (register to stack instructions)*)
  let code((paramDefs:parameter_def list), (env:env)) : code = 
    let reg = ref (-1) in
    paramDefs
    |> List.map (fun paramDef -> 
      let paramdef_typedef = ParameterDef.typeDef(paramDef) in
      match paramdef_typedef with
      | Single(datatype,id) -> 
        ParameterDef.Single.code(id, reg, env)
      | Array(datatype,id,ranges) ->
        let slotNums = Decleration.Array.size(ranges) in
        Int.range(0,slotNums)(*[for i in 0..slotNums -> i]*)
        |> List.map(fun i ->
          let varName = 
            if i = 0 
            then id
            else Printf.sprintf "%s%i" id i in
          ParameterDef.Single.code(varName, reg, env)
          )
        |> List.concat
      )
    |> List.concat

end

module Procedure = struct
  (*extracts out the identifier for a procedure*)
  let id(p:procedure) : identifier = 
    let (id,_,_) = p in
    id

  (*extracts out the procedure body from a procedure*)
  let body(p:procedure): procedure_body =
    let (_,_,pb) = p in
    pb

  (*extracts out the declerations from a procedure*)
  let decls(p:procedure) : decleration list = ProcedureBody.decls(body(p))
        
  (*extracts out the statements from a procedure*)
  let stmts(p:procedure) : statement list = ProcedureBody.stmts(body(p))
        
  (*extracts out the parameter definitions from a procedure*)
  let paramDefs(p:procedure) : parameter_def list = 
    let (_,paramdefs,_) = p in
    paramdefs

  (*adds a procedure and it's parameters to the procedure-parameters data map*)
  let procParam((p:procedure), (procParams:procParams ref)) = 
    let procId = id(p) in
    let paramdefs = paramDefs(p) in
    ProcParams.addMany(procId, paramdefs, procParams)

  (*generates code for a procedure. calculates the stack frame size for
  push/pop and adds parameters/declerations to an environment*)
  let code((p:procedure), (procParams:procParams)) : code = 
    let decls = decls(p) in
    let stmts = stmts(p) in
    let env = ref Env.init in
    let paramdefs = paramDefs(p) in
    ParameterDefs.env(paramdefs, env);
    Declerations.env(decls, env);
    let var_count = List.length(!env) in
    let decl_code = Declerations.code(decls, !env) in
    [
    [
    Label(id(p));
    Comment("prologue");
    PushStackFrame(var_count);
    Comment("parameters")
    ];
    ParameterDefs.code(paramdefs, !env);
    [Comment("declerations")];
    decl_code;
    [Comment("statements")];
    Statements.code(stmts, !env, procParams);
    [
    Comment("epilogue");
    PopStackFrame(var_count);
    Return
    ]
    ]
    |> List.concat

end
	
module Procedures = struct
  (*creates procedure-parameters data map for all procedures in program*)
  let procParam((procs:procedure list), (procParams:procParams ref)) =
    procs
    |> List.iter (fun proc -> Procedure.procParam(proc, procParams)) 

  (*generates code for all procedures*)
  let code(procs:procedure list) : code =
    (*creates procedure-parameters map for use in procedure invocation*)
    let procParams = ref (ProcParams.create) in
    procParam(procs, procParams);
    procs
    |> List.map (fun proc -> Procedure.code(proc, !procParams)) 
    |> List.concat
end

module Program = struct
  (*generates code for entire program*)
  let code(program:program) : code = 
    [
    Call("main");
    Halt
    ]
    @
    Procedures.code(program)
end

(*generates code and formats as string*)
let generate(program:program) : string =
  Program.code(program)
  |> List.map (fun i -> Instruction.To.string(i)) 
  |> String.concat "\n"

	
	
	
	
	
	
	
	
	
	
	
	