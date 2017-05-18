/*
The parser for the snick. 
It defines 
1.the token types for the Lexer 
2.the parser rules that are used to generate AST
*/

%{
open Snick_ast
open Snick_symbol
open Snick_analyze

let main_tbl = init_main_tbl;;

(* remove unexpected double quotes at the begining and end of a String *)
let string_clean str = 
  if String.length str > 1 then
    String.sub str 1 ((String.length str)-2)
  else 
    str

%}

/*Variables*/
%token <bool> BOOL_CONST
%token <float> FLOAT_CONST
%token <int> INT_CONST
%token <string> STRING_CONST

/*Identifier*/
%token <string> IDENT

/*Declaration*/
%token BOOL INT FLOAT

/*Bool val*/
%token TRUE FALSE

/*Ref*/
%token REF VAL

/*Statements*/
%token WRITE READ
%token ASSIGN
%token SEMICOLON COMMA
%token AND OR

%token LBRACK RBRACK DDOT 
%token LPAREN RPAREN

/*Operations */
%token EQ NOTEQ LT GT LTEQ GTEQ
%token PLUS MINUS MUL DIV

/*Unary*/
%token NOT

/*Condition*/
%token IF THEN ELSE FI

/*While*/
%token DO WHILE OD

/*Procedure*/
%token MAIN PROC END

%token EOF

/*Priority*/
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ LT GT LTEQ GTEQ NOTEQ
%left PLUS MINUS  /*Lower precendence*/
%left MUL DIV     /*Higher precendence*/
%nonassoc UMINUS  /*Highest precendence*/

%type <Snick_ast.program_with_sym_tbl> program_with_sym_tbl

%start program_with_sym_tbl
%%

/* Rules */

program_with_sym_tbl:
  | program { ($1, get_main_sym_tbl) }

program:
  | procedures { init_prog; finalize_prog ( $1 ) } 

procedures:
  | procedures procedure { $2 :: $1 }
  | procedure { [$1] }

procedure:
  | PROC procedure_define LPAREN parameter_defs RPAREN procedure_body END {add_proc($2); check_proc ( $2, List.rev $4, $6 )  }

procedure_define:
  | IDENT {add_proc($1); $1;}

procedure_body:
  | declerations statements { ( List.rev $1, List.rev $2 ) }

/*Parameters*/
parameter_defs:
  | parameter_defs_body { $1 }
  | { [] } /*allow blank parameter_def */

/*Parameters can be connected by comma*/
parameter_defs_body:
  | parameter_def { [$1] }
  | parameter_defs_body COMMA parameter_def { $3 :: $1 } /*parameter_defs can be connected by commas*/

parameter_def:
  | VAL type_def_single_only { check_param ( Value, $2, Expr_None ) }
  | REF type_def_single_only { check_param ( Reference, $2, Expr_None ) }

/*Declarations*/
declerations :
  | declerations decleration { $2 :: $1 }
  | { [] } /*Declaration can be blank*/

decleration :
  | type_def SEMICOLON { check_dec ($1, Expr_None) }

type_def :
  | datatype IDENT {Single ($1,$2)}
  | datatype IDENT LBRACK ranges RBRACK {Array ($1, $2, $4)}

/*Parameter can be declared as Single only*/
type_def_single_only :
  | datatype IDENT {Single ($1,$2)}

datatype:
  | BOOL { Bool }
  | INT { Int }
  | FLOAT { Float }

ranges:
  | ranges COMMA range  { $3 :: $1 }
  | range  { [$1] }
 
range:
  | INT_CONST DDOT INT_CONST  { ($1, $3) }

/*Statements*/
/* Builds statements in reverse order */
statements:
  | statements statement { $2 :: $1 }
  | statement { [$1] }

statement:
  | READ lvalue SEMICOLON { Read $2 }
  | WRITE expr SEMICOLON { Write $2 }
  | lvalue ASSIGN rvalue SEMICOLON { check_assign ( Assign ($1, $3) ) }
  | IDENT LPAREN exprs_nullable RPAREN SEMICOLON { check_invoke ( InvokeProc ($1, List.rev $3) ) } /* Invoke procedure*/
  | IF expr THEN statements FI { IfThen( check_expr_bool ($2), List.rev $4) }
  | IF expr THEN statements ELSE statements FI { IfThenElse ( check_expr_bool ($2), List.rev $4, List.rev $6) }
  | WHILE expr DO statements OD { WhileDo ( check_expr_bool ($2), List.rev $4) }

rvalue :
  | expr { Rexpr $1 }

lvalue:
  | IDENT { check_lvalue ( LId ($1,Expr_None) ) }
  | IDENT LBRACK exprs RBRACK  { check_lvalue ( LArrayElement ( $1, List.rev $3,Expr_None) ) }

/*Expressions*/
/*Multiple expressions connected by ","*/
exprs:
  | expr { [$1]  }
  | exprs COMMA expr { $3 :: $1 }

/* Allow Expression to be blank, used for invoking a function */
exprs_nullable: 
  | exprs { $1  }
  | { [] }

/*Expression*/
expr:
  | BOOL_CONST { Ebool ($1, Expr_Bool) }
  | INT_CONST { Eint ($1, Expr_Int) }
  | FLOAT_CONST { Efloat ($1, Expr_Float) }
  | STRING_CONST { Estring (string_clean $1 , Expr_String) }
  | lvalue { assign_expr (Elval ($1, Expr_None)) } 

  | expr PLUS expr {check_expr_op (Ebinop ($1, Op_add, $3, Expr_None)) }
  | expr MINUS expr { check_expr_op (Ebinop ($1, Op_sub, $3, Expr_None))  }
  | expr MUL expr { check_expr_op (Ebinop ($1, Op_mul, $3, Expr_None))  }
  | expr DIV expr { check_expr_op (Ebinop ($1, Op_div, $3, Expr_None))  }
  | expr EQ expr { check_expr_op (Ebinop ($1, Op_eq, $3, Expr_None))  }
  | expr NOTEQ expr { check_expr_op (Ebinop ($1, Op_not_eq, $3, Expr_None))  }
  | expr LT expr { check_expr_op (Ebinop ($1, Op_lt, $3, Expr_None))  }
  | expr GT expr { check_expr_op (Ebinop ($1, Op_gt, $3, Expr_None))  }
  | expr LTEQ expr { check_expr_op (Ebinop ($1, Op_lt_eq, $3, Expr_None))  }
  | expr GTEQ expr { check_expr_op (Ebinop ($1, Op_gt_eq, $3, Expr_None) ) }
  | expr AND expr { check_expr_op (Ebinop ($1, Op_and, $3, Expr_None))  }
  | expr OR expr { check_expr_op (Ebinop ($1, Op_or, $3, Expr_None))  }

  | MINUS expr %prec UMINUS { assign_expr (Eunop (Op_minus, $2, Expr_None)) }
  | NOT expr  { assign_expr (Eunop (Op_not, $2, Expr_None)) }
  | LPAREN expr RPAREN { assign_expr (Eparens ($2, Expr_None)) }
