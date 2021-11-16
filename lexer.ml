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
        
        type 't production = { 
            regex: string,
            ctor: 't -> token
        }

        let productions = [
            { regex = "[\\s,]+"; ctor = fun _ -> Ws };
            { regex = ";.*"; ctor = fun x -> Comment x };
            { regex = "0?x[0-9a-fA-F]+"; ctor = fun x -> Hex x };
            { regex = "#?-?[0-9]+"; ctor = fun x -> Number x };
        ]
        
        let lex_line line = 
            let ret = [] in
            let rec lx r = 
                Printf.printf "%s %d" line r;
                let rec pattern_match p =
                    match p with
                    | []     -> raise Not_found
                    | h :: t -> begin
                        match Re.exec (Re.Pcre.regexp h.regex) line ~pos:r with
                        | exception Not_found -> begin
                            pattern_match t 
                        end
                        | x -> begin

                        end
                    end
    end


(*
    Solution: we need to look through all productions
    - when we fidn a match, we need to append to some type of list
*)
            
