(* Specification of an AST for bean *)
type ident = string
 
type int_range = (int*int)

(* Keep aliases intact for pretty printing. *)
type datatype =
  | Bool
  | Int
  | Float

type typedef = 
  | Single of (ident * datatype)
  | Array of (ident * datatype * int_range list)
    
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

type unop =
  | Op_minus
  | Op_not
    
type lvalue =
  | LId of ident
  | LArrayIndex of (ident * expr list)
and expr =
  | Ebool of bool
  | Eint of int
  | Efloat of float
  | Elval of lvalue
  | Ebinop of (expr * binop * expr)
  | Eunop of (unop * expr)
  | Eparens of expr

(* Will need to AST elements with additional data.  *)
type rvalue =
  | Rexpr of expr

type decl = (ident * datatype)

type stmt = 
  | Assign of (lvalue * rvalue)
  | Read of lvalue
  | Write of expr
  | InvokeProc of (ident * expr list)
  | IfThen of (expr * stmt list)
  | IfThenElse of (expr * stmt list * stmt list)
  | WhileDo of (expr * stmt list)

type proc_body = {
  decls : typedef list ;
  stmts : stmt list
}
  
type passby = | Value | Reference
 
type paramdef = (passby * typedef)
  
type proc = (ident * paramdef list * proc_body)
  
type program = proc list
