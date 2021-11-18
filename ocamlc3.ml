open Lexer;;

 let sprint_list lst = 
    let rec sprint_list_helper l = 
        match l with
        | [] -> ""
        | h :: t -> begin
            Printf.sprintf "; %s%s" (Lexer.token_print h) 
                (sprint_list_helper t) end
    in Printf.sprintf "[%s]" (sprint_list_helper lst);;

let line = ".ORIG ADEMERG: ,ADD R6, R5, R2 x784  x7216 x8317 "
    ^ "#-3921 #391 #019 .END; text" in
Printf.printf "%s\n" (sprint_list (Lexer.lex_line line));;
