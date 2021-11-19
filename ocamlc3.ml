open Lexer;;

let lexed_file = Lexer.lex_file "input.asm" in
let mapped_lexed_file = List.map (List.map Lexer.token_print) lexed_file in
List.iter (List.iter (Printf.printf "%s\n")) mapped_lexed_file
