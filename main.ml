(* File main.ml *)
open Ast
let _ =
try
print_string "start";
   let c = open_in Sys.argv.(1) in
    let lexbuf = Lexing.from_channel c in

let result = Llvm_yacc.program Llvm_lex.token lexbuf in
close_in c;
     List.iter (fun s -> print_string (string_of_fundecl s)) result
   

with Llvm_lex.Eof ->
print_string "exit";
    exit 0
