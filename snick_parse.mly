/*
The parser for the snick. 
It defines 
1.the token types for the Lexer 
2.the parser rules that are used to generate AST
*/

%{
open Snick_ast
open Snick_analyze

let main_tbl = Snick_analyze.init_main_tbl;;
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
%nonassoc EQ LT MT LTEQ MTEQ
%left PLUS MINUS  /*Lower precendence*/
%left MUL DIV     /*Higher precendence*/
%nonassoc UMINUS  /*Highest precendence*/

%type <Snick_ast.program_with_sym_tbl> program_with_sym_tbl

%start program_with_sym_tbl
%%
/* Rules */
/*Start from top to bottom*/

/*Program is the top most rule*/

program_with_sym_tbl:
  | program {($1, Snick_analyze.get_main_sym_tbl)}

program:
  | procedures { Snick_analyze.init_prog; Snick_analyze.finalize_prog ( $1 ) } 

procedures:
  | procedures procedure { $2 :: $1 }
  | procedure { [$1] }

procedure:
  | PROC procedure_define LPAREN parameter_defs RPAREN procedure_body END {add_proc($2); Snick_analyze.check_proc ( $2, List.rev $4, $6 )  }

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
  | VAL type_def_single_only { Snick_analyze.check_param ( Value, $2, Expr_None ) }
  | REF type_def_single_only { Snick_analyze.check_param ( Reference, $2, Expr_None ) }

/*Declarations*/
declerations :
  | declerations decleration { $2 :: $1 }
  | { [] } /*Declaration can be blank*/

decleration :
  | type_def SEMICOLON { Snick_analyze.check_dec ($1, Expr_None) }

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
  | lvalue ASSIGN rvalue SEMICOLON { Snick_analyze.check_assign ( Assign ($1, $3) ) }
  | IDENT LPAREN exprs_nullable RPAREN SEMICOLON { Snick_analyze.check_invoke ( InvokeProc ($1, List.rev $3) ) } /* Invoke procedure*/
  | IF expr THEN statements FI { IfThen( Snick_analyze.check_expr_bool ($2), List.rev $4) }
  | IF expr THEN statements ELSE statements FI { IfThenElse ( Snick_analyze.check_expr_bool ($2), List.rev $4, List.rev $6) }
  | WHILE expr DO statements OD { WhileDo ( Snick_analyze.check_expr_bool ($2), List.rev $4) }

rvalue :
  | expr { Rexpr $1 }

lvalue:
  | IDENT { Snick_analyze.check_lvalue ( LId ($1,Expr_None) ) }
  | IDENT LBRACK exprs RBRACK  { Snick_analyze.check_lvalue ( LArrayElement ( $1, List.rev $3,Expr_None) ) }

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
  | STRING_CONST { Estring ($1, Expr_String) }
  | lvalue { Snick_analyze.assign_expr (Elval ($1, Expr_None)) } 

  | expr PLUS expr {Snick_analyze.check_expr_op (Ebinop ($1, Op_add, $3, Expr_None)) }
  | expr MINUS expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_sub, $3, Expr_None))  }
  | expr MUL expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_mul, $3, Expr_None))  }
  | expr DIV expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_div, $3, Expr_None))  }
  | expr EQ expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_eq, $3, Expr_None))  }
  | expr NOTEQ expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_not_eq, $3, Expr_None))  }
  | expr LT expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_lt, $3, Expr_None))  }
  | expr GT expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_gt, $3, Expr_None))  }
  | expr LTEQ expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_lt_eq, $3, Expr_None))  }
  | expr GTEQ expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_gt_eq, $3, Expr_None) ) }
  | expr AND expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_and, $3, Expr_None))  }
  | expr OR expr { Snick_analyze.check_expr_op (Ebinop ($1, Op_or, $3, Expr_None))  }

  | MINUS expr %prec UMINUS { Snick_analyze.assign_expr (Eunop (Op_minus, $2, Expr_None)) }
  | NOT expr  { Snick_analyze.assign_expr (Eunop (Op_not, $2, Expr_None)) }
  | LPAREN expr RPAREN { Snick_analyze.assign_expr (Eparens ($2, Expr_None)) }
