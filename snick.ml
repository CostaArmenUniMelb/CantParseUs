module P = Snick_parse
open Snick_symbol
open Codegen

(* Argument parsing code *)
let infile_name = ref None

type compiler_mode = PrettyPrint | Compile
let mode = ref Compile

(* --------------------------------------------- *)
(*  Specification for command-line options       *)
(* --------------------------------------------- *)
let (speclist:(Arg.key * Arg.spec * Arg.doc) list) =
  ["-p",
     Arg.Unit(fun () -> mode := PrettyPrint),
     " Run the compiler in pretty-printer mode"
  ]

let main () =
  (* Parse the command-line arguments *)
  Arg.parse speclist
      (begin fun fname -> infile_name := Some fname end)
      "bean [-p] [bean source]" ;
  (* Open the input file *)
  let infile = match !infile_name with
  | None -> stdin
  | Some fname -> open_in fname in
  (* Initialize lexing buffer *)
  let lexbuf = Lexing.from_channel infile in
  (* Call the parser *)
  try
    let (prog,main_sym_tbl) = Snick_parse.program_with_sym_tbl Snick_lex.token lexbuf in
    match !mode with
    | PrettyPrint -> print_string (Snick_pprint.print_program prog)
    | Compile -> print_string (generate(prog))
  with
    | Parsing.Parse_error -> Snick_lex.raise_parser_fail lexbuf 
    | Snick_analyze.Syntax_error _ as err -> let msg = Printexc.to_string err in Snick_lex.raise_syntax_fail lexbuf msg

let _ = main ()
