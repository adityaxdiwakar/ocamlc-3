open Lexer;;

let line = ".ORIG ADEMERG: ,ADD R6, R5, R2 x784  x7216 x8317 "
    ^ "#-3921 #391 #019 .END; text" in
Printf.printf "%s\n" (Lexer.sprint_list (Lexer.lex_line line));;
