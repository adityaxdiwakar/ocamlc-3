let lexed_file = Lexer.lex_file "input.asm" in
let mapped_lexed_file = (Lexer.token_print 
                        |> List.map
                        |> List.map) lexed_file in
(Printf.printf "%s\n" |> List.iter |> List.iter) mapped_lexed_file
