/*
The parser for the snick. 
It defines 
1.the token types for the Lexer 
2.the parser rules that are used to generate AST
*/

%{
open Snick_ast
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

%type <Snick_ast.program> program

%start program
%%
/* Rules */
/*Start from top to bottom*/

/*Program is the top most rule, program must contains exactly on "main" procedure*/
program:
  | procedures procedure_main procedures { List.append $3 ($2::$1) } 

procedures:
  | procedures procedure { $2 :: $1 }
  | { [] }

procedure:
  | PROC IDENT LPAREN parameter_defs RPAREN procedure_body END {($2,List.rev $4,$6)}

/*Procedure "main" are parameterless*/
procedure_main:
  | PROC MAIN LPAREN RPAREN procedure_body END { ("main", [] ,$5) }

procedure_body:
  | declerations statements { (List.rev $1, List.rev $2) }

/*Parameters*/
parameter_defs:
  | parameter_defs_body { $1 }
  | { [] } /*allow blank parameter_def */

/*Parameters can be connected by comma*/
parameter_defs_body:
  | parameter_def { [$1] }
  | parameter_defs_body COMMA parameter_def { $3 :: $1 } /*parameter_defs can be connected by commas*/

parameter_def:
  | VAL type_def {(Value,$2)}
  | REF type_def {(Reference,$2)}

/*Declarations*/
declerations :
  | declerations decleration { $2 :: $1 }
  | { [] } /*Declaration can be blank*/

decleration :
  | type_def SEMICOLON { $1 }

type_def :
  | datatype IDENT {Single ($1,$2)}
  | datatype IDENT LBRACK ranges RBRACK {Array ($1, $2, $4)}

datatype:
  | BOOL { Bool }
  | INT { Int }
  | FLOAT { Float }

ranges:
  |  ranges COMMA range  { $3 :: $1 }
  |  range  { [$1] }
 
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
  | lvalue ASSIGN rvalue SEMICOLON { Assign ($1, $3) }
  | IDENT LPAREN exprs_nullable RPAREN SEMICOLON { InvokeProc($1, List.rev $3) } /* Invoke procedure*/
  | IF expr THEN statements FI { IfThen($2, List.rev $4) }
  | IF expr THEN statements ELSE statements FI {IfThenElse($2, List.rev $4,List.rev $6) }
  | WHILE expr DO statements OD { WhileDo($2, List.rev $4) }

rvalue :
  | expr { Rexpr $1 }

lvalue:
  | IDENT { LId $1 }
  | IDENT LBRACK exprs RBRACK  { LArrayElement( $1, List.rev $3) }

/*Expressions*/
/*Multiple expressions connected by ","*/
exprs:
  |expr { [$1]  }
  |exprs COMMA expr { $3 :: $1 }

/* Allow Expression to be blank, used for invoking a function */
exprs_nullable: 
  |exprs { $1  }
  |{ [] }

/*Expression*/
expr:
  | BOOL_CONST { Ebool $1 }
  | INT_CONST { Eint $1 }
  | FLOAT_CONST { Efloat $1 }
  | STRING_CONST { Estring $1 }
  | lvalue { Elval $1 } 

  | expr PLUS expr { Ebinop ($1, Op_add, $3) }
  | expr MINUS expr { Ebinop ($1, Op_sub, $3) }
  | expr MUL expr { Ebinop ($1, Op_mul, $3) }
  | expr DIV expr { Ebinop ($1, Op_div, $3) }
  | expr EQ expr { Ebinop ($1, Op_eq, $3) }
  | expr NOTEQ expr { Ebinop ($1, Op_not_eq, $3) }
  | expr LT expr { Ebinop ($1, Op_lt, $3) }
  | expr GT expr { Ebinop ($1, Op_gt, $3) }
  | expr LTEQ expr { Ebinop ($1, Op_lt_eq, $3) }
  | expr GTEQ expr { Ebinop ($1, Op_gt_eq, $3) }
  | expr AND expr { Ebinop ($1, Op_and, $3) }
  | expr OR expr { Ebinop ($1, Op_or, $3) }

  | MINUS expr %prec UMINUS { Eunop (Op_minus, $2) }
  | NOT expr  { Eunop (Op_not, $2) }
  | LPAREN expr RPAREN { Eparens $2 }