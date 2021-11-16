module Lexer =
    struct
        type tokens = 
            | Ws
            | Comment of string
            | Hex of string
            | Num of string
            | Str of string
            | Op of string
            | Directive of string
            | Reg of string
            | Label of string
            | Ident of string

        let patterns = [
            "[\\s,]+"
        ]
        
        let lex_line line = 
            let ret = [] in
            let rec lx r = 
                Printf.printf "%s %d" line r;
                let rec pattern_match p =
                    match p with
                    | []     -> raise Not_found
                    | h :: t -> begin
                        match Re.exec (Re.Pcre.regexp h) line ~pos:r with
                        | exception Not_found -> begin
                            pattern_match t 
                        end
                        | x -> x
                    end
    end
            
