(*
Author: Costa Armen
Purpose: To define the type structure of the AST 
General Notes:
All types for the ast are contained in this file. Some types contain
comments on future extensibility. 
*)

(*identifier for procs, params to procs and variables*)
type identifier = string
  
(*describes a min-max range in static array declerations.
i.e. int arrayVarName[1..2,1..4]*)
type range = (int*int)

(*declared data types. String is not included as
this only appears in the write operation. String is never used
as a decleration. i.e. bool variable *)
type datatype =
  | Bool
  | Int
  | Float
  | String

(* possible types of an expression, used for syntax checking *)
type expr_type =
  | Expr_Bool
  | Expr_Int
  | Expr_Float
  | Expr_String
  | Expr_None
    
(*Two variations of type declerations exist. 
Array and non-array (single). Arrays require a list of
ranges. *)
type type_def = 
  | Single of (datatype * identifier)
  | Array of (datatype * identifier * range list)
    
(*All binary operations. Bool, Comparison and Arithmetic.*)
type binop =
  | Op_add 
  | Op_sub 
  | Op_mul 
  | Op_div
  | Op_eq 
  | Op_lt
  | Op_or  
  | Op_and
  | Op_not_eq
  | Op_lt_eq
  | Op_gt
  | Op_gt_eq
    
(*All unary operations. Bool and Arithmetic*)
type unop =
  | Op_minus
  | Op_not

(* Op_type_math is the type that the parent and child expressions are int or float*)
(* Op_type_bool isthe type that the parent and child expressions are bool*)
(* Op_type_math_to_bool is the type that the child expressions are 
  int or float but the parent must be bool
Op_type_both_to_bool = the expression can be bool or math and the result is bool*)
type op_type =
  | Op_type_math_to_math 
  | Op_type_bool_to_bool
  | Op_type_math_to_bool  
  | Op_type_both_to_bool
    
(*Left hand side of an assignment (variables
and array elements can be assigned to). Note that an 
array access may have expressions that can resolve to 
an integer. i.e. arrayVar[1+1] = arrayVar[2] where 1+1
is an expression. This results in a mutually recursive type
between lvalue and expr, hence the 'and' keyword between the
two types. *)
type lvalue =
  | LId of (identifier * expr_type)
  | LArrayElement of (identifier * expr list* expr_type)
    
(*all expressions. includes basic data types, variable refs, 
binary/unary expressions and expressions wrapped in parenthesis.*)
and expr =
  | Estring of (string * expr_type)
  | Ebool of (bool * expr_type)
  | Eint of (int * expr_type)
  | Efloat of (float * expr_type)
  | Elval of (lvalue * expr_type)
  | Ebinop of (expr * binop * expr * expr_type)
  | Eunop of (unop * expr * expr_type)
  | Eparens of (expr * expr_type)

(* right hand side (rhs) of assignment statement*)
type rvalue =
  | Rexpr of expr
    
type decleration = (type_def * expr_type)
  
(*all statements in procedure body. Atomic statements such as 
Assignments, read/write, procedure invocations, and, 
compound statements such as ifthen, ifthenelse and whiledo. 
May have to split atomic/compound statements into seperate types
to aid in analysis phase later on. 
i.e. 
statement = | atomic | compound.  
atomic = assign of (lavlue * rvalue) ..etc.
compound = IfThen of (expr * statement list) ...etc.
*)
type statement = 
  | Assign of (lvalue * rvalue) (*atomic*)
  | Read of lvalue (*atomic*)
  | Write of expr (*atomic*)
  | InvokeProc of (identifier * expr list) (*atomic*)
  | IfThen of (expr * statement list) (*compound*)
  | IfThenElse of (expr * statement list * statement list) (*compound*)
  | WhileDo of (expr * statement list) (*compound*)
    
(*procedure body composed of declerations and statements*)
type procedure_body = (decleration list * statement list)
  
(*pass by type used in procedure param definitions*)
type passby = | Value | Reference
  
(*param definition (i.e. 'ref int paramName1') in procedure 
definitions*)
type parameter_def = (passby * type_def * expr_type)
  
(*entire procedure including its definition and content*)
type procedure = (identifier * parameter_def list * procedure_body)
  
(*an entire program is a sequence of procedures*)
type program = procedure list

type tbl_type =
  | Proc
  | Invoke
  | Current

(* 3 possible types. Procedure, paramters and invoke *)
(*tbl_type define the symbol table that will be used for insert and add
Proc is used for storing all procedures and their details 
Invoke is  for storing all invoked procedures and parameters ** Invoke is used for syntac checking only,
           the Codegen will never uses Invoke table
Current is for storeing all parameters in a procedure (type = Param). It will use the current procedure name 
          as the real name and changes everytime the parser reads a new procedure *)
type symbol_data =
  | Invoke_symbol of statement  (* for store InvokeProc only*)
  | Proc_symbol of procedure
  | Param_symbol of parameter_def

(* a single symbol tbl *)
type symbol_table = (string, symbol_data) Hashtbl.t

(*return the AST and the symbol tbl*)
type program_with_sym_tbl = (program *  (string, symbol_table) Hashtbl.t)



