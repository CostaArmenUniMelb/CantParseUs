(*
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
    
(*Two variations of type declerations exist. 
Array and non-array (single). Arrays require a list of
ranges. *)
type type_def = 
  | Single of (datatype * identifier)
  | Array of (datatype * identifier * range list)
    
(*All binary operations. Bool, Comparison and Arithmetic.
We may have to seperate these later on to aid in semantic
analysis. i.e. bool_binop, arithmentic_binop*)
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
    
(*Left hand side of an assignment (variables
and array elements can be assigned to). Note that an 
array access may have expressions that can resolve to 
an integer. i.e. arrayVar[1+1] = arrayVar[2] where 1+1
is an expression. This results in a mutually recursive type
between lvalue and expr, hence the 'and' keyword between the
two types. *)
type lvalue =
  | LId of identifier
  | LArrayElement of (identifier * expr list)
    
(*all expressions. includes basic data types, variable refs, 
binary/unary expressions and expressions wrapped in parenthesis.*)
and expr =
  | Estring of string
  | Ebool of bool
  | Eint of int
  | Efloat of float
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | Eparens of expr

(* right hand side (rhs) of assignment statement. This was going 
to be deleted as all rhs assignments are expressions which makes
this a redundant wrapper. There was a comment that stated to leave
this for future extension. *)
type rvalue =
  | Rexpr of expr
    
type decleration = type_def
  
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
type parameter_def = (passby * type_def)
  
(*entire procedure including its definition and content*)
type procedure = (identifier * parameter_def list * procedure_body)
  
(*an entire program is a sequence of procedures*)
type program = procedure list
