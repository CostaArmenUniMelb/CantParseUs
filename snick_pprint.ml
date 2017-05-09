(*
Author: Costa Armen
Purpose: To format and print the AST
General Notes:
This file contains all the functions to format
the ast. All types defined in Snick_ast.ml have an
associated function with the prefix 'format_' which refers
to, 'formatted'. i.e. 'datatype = Bool | Int | Float' from
Snick_ast.ml has a function here with the name 'format_datatype'
which takes a parameter of type 'datatype'. This file also contains
helper functions.
*)

open Snick_ast
open Format
  
(*helper functions - indenting and 'reductions'*)

(*creates a specified number of spaces. 
i.e. indent 5 -> '     ' *)
let indent num_of_whitespace = 
  let rec indentation count spaces  =
    match spaces with
      | _ when count > 0 -> indentation (count-1) (" "^spaces)
      | _ -> spaces
  in
  let result = indentation num_of_whitespace "" in
  if num_of_whitespace <=0 then "" else result
    
(*indents an entire list by prefixing a specified number of whitespace 
to all elements of that list.*)
let indent_list num_of_whitespace list = 
  list 
  |> List.map (fun element -> (indent num_of_whitespace) ^ element)
  
(*contains the level of whitespace increment per indentation. 
i.e. 1 indent -> 4 whitespace chars*)
let add_indent num_of_whitespace = num_of_whitespace + 4
  
(*reduces a list of strings and places a delimiting string between each 
consequtive pair. i.e reduce "," ["a";"b";"c"] -> "a,b,c" *)
let reduce delimiter list =
  match list with
    | [] -> ""
    | _ -> List.fold_left (fun s1 s2 -> s1 ^ delimiter ^ s2) (List.hd list) (List.tl list)
      
(*formatted types*)
      
(*each datatype type is simply formatted to its lowercase equivalent*)
let format_dataType datatype = 
  match datatype with
    | Bool -> "bool"
    | Int -> "int"
    | Float -> "float"
      
(*formats the pair of datatype and identifier.*)
let format_dataType_identifier datatype_identifier =  
  let (datatype, identifier) = datatype_identifier
  in
  (format_dataType datatype) ^ " " ^ identifier
    
(*formats a pair of ints to represent a range.
i.e. 1 to 2 -> '1..2' *)
let format_int_range int_pair = 
  let (min, max) = int_pair in
  (string_of_int min) ^ ".." ^ (string_of_int max)
    
(*formats a list of int ranges which is used when declaring a 
multi dim array*)
let format_int_range_list int_range_list = 
  List.map (fun i -> format_int_range i) int_range_list
  |> reduce ","
    
(*formats all type definitions, single and array.
i.e. 
single: (bool,"exampleIdentifierName") -> bool exampleIdentifierName 
array: (int,"exampleIdentifierName",[(1,2);(1,3)]) -> int exampleIdentifierName[1..2,1..3]
*)
let format_typedef typedef = 
  match typedef with
    | Single(datatype,identifier) -> format_dataType_identifier (datatype,identifier)
    | Array(datatype,identifier,range_list) -> 
      (format_dataType_identifier (datatype,identifier)) ^ "[" ^ (format_int_range_list range_list) ^ "]"
        
(*formats binop as symbols. Spaces between wrapping expression elements is accounted 
for in the expression formatter, not here*)
let format_binop binop =
  match binop with
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
     
(*formats unary operation as symbol or keyword.*) 
let format_unop unop = 
  match unop with
    | Op_minus -> "-"
    | Op_not -> "not "
      
let format_bool bool = 
  match bool with
    | true -> "true"
    | false -> "false" 
      
(*gets the binary operator in the expression if it exists*)
let rec try_get_binop expression = 
  match expression with
    | Ebinop(ebinop) -> 
      let (_, binop, _,_) = ebinop
     	in
      Some binop
    | Eparens(expr,expr_type) -> try_get_binop expr
    | _ -> None
          
(*determines if the RHS or LHS child expression requires parenthesis based on the operators 
of a parent and it's rightmost child. The mapping of operator pairs and whether it 
requires parenthesis was derived using precdence and associativity rules.*)
let requires_parenthesis binop expression isLHS =
  match (try_get_binop expression) with
    | Some(expression_binop) -> 
      let binops_require_parens =
        match binop, expression_binop, isLHS with
          | Op_mul, Op_div, false -> true (*e.g. 3*(5/7) *)
          | Op_mul, Op_add, _ -> true   (*e.g. 3*(5+7) *)
          | Op_mul, Op_sub, _ -> true   (*e.g. 3*(5-7) *)
          | Op_div, Op_mul, false -> true (*e.g. 3/(5*7) *)
          | Op_div, Op_div, false -> true (*e.g. 3/(5/7) *)
          | Op_div, Op_add, _ -> true (*e.g. 3/(5+7) *)
          | Op_div, Op_sub, _ -> true (*e.g. 3/(5-7) *)
          | Op_sub, Op_add, false -> true(*e.g. 3-(5+7) *)
          | Op_sub, Op_sub, false -> true   (*e.g. 3-(5-7)*)
          | Op_and, Op_or, _ -> true
          | Op_or, Op_and, _ -> true
          | _ -> false
      in
      binops_require_parens
    | None -> false
            
(*formats the left hand side of an assignment. 
Note that lvalue and expression are mutually recursive types and therefore require 
mutually recursive functions to format them. The 'and' keyword between the two
formatting functions allows for this mutual recursion.*)
let rec format_lvalue lval =
  match lval with
    | LId(i) -> i
    | LArrayElement(id,ind) -> 
      let expression_list = format_expression_list ind in
      id ^ "[" ^ expression_list ^ "]"

(*formats a single expression*)
and format_expression expression =
  match expression with
    | Estring(s) -> let (s, _ ) = s in s
    | Ebool(b) -> let (b, _ ) = b in format_bool b
    | Eint(i) -> let (i, _ ) = i in string_of_int i
    | Efloat(f) -> let (f, _ ) = f in  string_of_float f
    | Elval(lval) -> let (lval, _ ) = lval in format_lvalue lval
    | Ebinop(ebinop) -> 
      let (expression1, binop, expression2,_) = ebinop
      in 
      let formatted_expression1 =
        match (requires_parenthesis binop expression1 true) with
        | true -> "(" ^ (format_expression expression1) ^ ")"
        | false -> (format_expression expression1) 
      in
      let formatted_expression2 =
        match (requires_parenthesis binop expression2 false) with
        | true -> "(" ^ (format_expression expression2) ^ ")"
        | false -> (format_expression expression2) 
      in
      (formatted_expression1)^" "^ (format_binop binop)^" "^formatted_expression2
          
    | Eunop(eunop) -> 
      let (unaryop, expression, expr_type) = eunop
      in
      (format_unop unaryop) ^ (format_expression expression)
    | Eparens(expression,expr_type) -> format_expression expression

(*formats a list of expressions for use in array access and proc invocation 
hence the comma delimiting (reduce function)*)
and format_expression_list list = List.map (fun expression -> format_expression expression) list |> reduce ", "
  
(*formats right hand side of assignment statement*)
let format_rvalue rval =
  match rval with
    | Rexpr(e) -> format_expression e
      
(*formats a decleration (typedef) by adding indentation and a semicolon to terminate the 
atomic statement*)
let format_decl indent_num decl = let (typedef , expr_type) = decl in (indent indent_num) ^ format_typedef typedef ^ ";"
  
(*formats a decleration list and delimits them by an endline character*)
let format_decleration_list indent_num list = 
  list
  |> List.map (fun decleration -> format_decl indent_num decleration)  
  |> List.filter (fun decleration -> decleration <> "")
  |> reduce "\n"
    
(*formats both atomic and compound statements. *)
let rec format_statement indent_num statement =
  let current_indent = indent indent_num 
  in
  match statement with
    | Assign(lvalue,rvalue) -> reduce " " [(format_lvalue lvalue);":=";(format_rvalue rvalue)] ^ ";"
    | Read(lvalue) -> "read" ^ " " ^ (format_lvalue lvalue) ^ ";"
    | Write(expression) -> "write" ^ " " ^ (format_expression expression) ^ ";" 
    | InvokeProc(identifier,expression_list) -> identifier ^ "(" ^ format_expression_list expression_list ^ ")" ^ ";"
    | IfThen(expression,statement_list) -> 
      "if " ^ format_expression expression ^ " then\n" ^ 
        format_statement_list (add_indent indent_num) statement_list ^ "\n" ^ 
        current_indent ^ 
      "fi"
    | IfThenElse(expression,then_statement_list,else_statement_list) -> 
      "if " ^ format_expression expression ^ " then\n" ^ 
        format_statement_list (add_indent indent_num) then_statement_list ^ "\n" ^ 
        current_indent ^ 
      "else\n" ^ 
        format_statement_list (add_indent indent_num) else_statement_list ^ "\n" ^ 
        current_indent ^ 
      "fi"
    | WhileDo(expression,statement_list) -> 
      "while " ^ format_expression expression ^ " do\n" ^
        format_statement_list (add_indent indent_num) statement_list ^ "\n" ^
        current_indent ^ 
       "od"       
(*formats a list of statements and delimits them with a newline*)
and format_statement_list indent_num list = 
  list 
  |> List.map (fun statement -> format_statement indent_num statement) 
  |> indent_list indent_num 
  |> reduce "\n"
    
(*formats a procedure body. If no declerations exist, then leave empty otherwise separate
declerations from statements with an empty blank line.*)
let format_procedure_body indent_num procedure =
  let (declerations,statements) = procedure 
  in
  let formatted_declerations = (format_decleration_list (add_indent indent_num) declerations)
  in
  let formatted_statements = format_statement_list (add_indent indent_num) statements
  in
  match formatted_declerations with
    | "" -> "\n" ^ formatted_statements
    | _ -> (reduce "\n\n" [formatted_declerations;formatted_statements])
     
(*formats passby resulting in val or ref keywords*) 
let format_passby passby =
  match passby with
    | Value -> "val"
    | Reference -> "ref"
      
(*formats parametereter in procedure definition.
i.e. format_parameter (Value,(Bool,"ExampleName")) -> 'val bool ExampleName' *)
let format_parameter parameter =
  let (passby, typedef , expr_type) = parameter
  in
  (format_passby passby) ^ " " ^ (format_typedef typedef)
    
(*formats entire procedure (name, parameters and procedure body)*)
let format_procedure procedure =
  let (identifier, parameters, procedure_body) = procedure
  in
  let formatted_parameters = List.map (fun p -> format_parameter p) parameters |> reduce ", "
  in
  "proc " ^ identifier ^ " (" ^ formatted_parameters ^ ")\n" ^ (format_procedure_body 1 procedure_body) ^ "\nend"

(*formats a program by formatting a sequence of procedures and delimiting them by empty blank lines.*)
let print_program procedure_list = 
  let program = 
    procedure_list
    |> List.map (fun procedure -> format_procedure procedure)  
    |> List.rev 
    |> reduce "\n\n"
  in
  program ^ "\n"
  
  
  