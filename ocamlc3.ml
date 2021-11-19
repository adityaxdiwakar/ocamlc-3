open Lexer;;

let lexed_file = Lexer.lex_file "input.asm" in
let mapped_lexed_file : string list list = List.map begin
    List.map Lexer.token_print end lexed_file in
List.iter begin
    List.iter(Printf.printf "%s\n") end mapped_lexed_file
