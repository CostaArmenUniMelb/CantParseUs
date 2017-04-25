(*
The lexer for the snack. 
It reads the input symbols and generate tokens for the parser
*)
{
open Snack_parse

(*Exception for error detection*)
exception Lexer_error of string;;
exception Parser_error of string;;

(* Show the error position*)
let error_pos lexbuf = 
  let pos = Lexing.lexeme_start_p lexbuf in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  let token = Lexing.lexeme lexbuf in
  Format.sprintf "Token:'%s', Line: %d, Column: %d" token pos.Lexing.pos_lnum col 

(*Raise an error if any*)
let raise_lexer_fail lexbuf =
  raise(Lexer_error  (error_pos lexbuf))

let raise_parser_fail lexbuf =
  raise(Parser_error (error_pos lexbuf))
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let alnum = alpha | '_' | '\'' | digit
let digits = digit+
let ident = (alpha | '_') alnum*
rule token = parse
  (* Escape chars *)
  [' ' '\t']    { token lexbuf }     (* skip blanks *)
  | '\n'      { Lexing.new_line lexbuf ; token lexbuf } (* Increment line number for error detection *)

  (* Variables *)
  | '\"'[^  '\n']*'\"' as lxm  { STRING_CONST lxm }   (*don't allow new line in the string*)
  | digits as lxm  { INT_CONST(int_of_string lxm) }
  | (digit * ['.'])?digits as lxm  { FLOAT_CONST(float_of_string lxm) }

  (* Keywords *)
  | "true" { BOOL_CONST true }
  | "false" { BOOL_CONST false }
  | "bool" { BOOL }
  | "int" { INT }
  | "float" { FLOAT }
  | "ref" { REF }
  | "val" { VAL }
  | "read" { READ }
  | "write" { WRITE }
  | ":=" { ASSIGN }
  | "and" { AND }
  | "or" { OR }
  | "do" { DO }
  | "while" { WHILE }
  | "od" { OD }
  | "not" { NOT }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fi" { FI }
  | "proc" { PROC }
  | "main" { MAIN }
  | "end" { END }

  (*Symbols*)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ".." { DDOT }
  | '=' { EQ }
  | "!=" { NOTEQ }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LTEQ }
  | ">=" { GTEQ }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | ';' { SEMICOLON }
  | ',' { COMMA }

  (*Indetifier*)
  | ident as lxm  { IDENT lxm } (*give the lower priorifor the identifier to make sure that it will match other symbols first*)

  (*Other symbols*)
  | eof { EOF } (* terminate if found EOF*)
  | '#' { comment lexbuf } (* go to the rule for the comment*)

  (*Throw the Unknown_Token error when not match any token*)
  | _ { raise_lexer_fail lexbuf} 

(*parse comment with different rules*)
and comment = parse 
             '\n' { token lexbuf }  (* return to the regular rule after finding a new line *)
              | eof { EOF } (* terminate if found EOF*)
              | _ { comment lexbuf } (* keep skipping chars until finding a new line*)