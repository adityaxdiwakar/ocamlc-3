open Re;;

module Lexer =
    struct
        type token = 
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
            regex: string;
            ctor: 't -> token
        }

        let productions = [
            { regex = "[\\s,]+"; ctor = fun _ -> Ws };
            { regex = ";.*"; ctor = fun x -> Comment x };
            { regex = "0?x[0-9a-fA-F]+"; ctor = fun x -> Hex x };
            { regex = "#?-?[0-9]+"; ctor = fun x -> Num x };
        ]

        let token_print x = 
            match x with
            | Ws            -> "WS()"
            | Comment(v)    -> (Printf.sprintf "Comment(%s)" v)
            | Hex(v)        -> (Printf.sprintf "Hex(%s)" v)
            | Num(v)        -> (Printf.sprintf "Num(%s)" v)
            | _             -> "Not recognized!"
        
        let lex_line line = 
            let rec lx r = 
                if r == String.length line then [] else 
                let rec pattern_match p =
                    match p with
                    | []     -> raise Not_found
                    | { regex = reg; ctor = ctor } :: tail -> begin
                        match exec (Pcre.regexp reg) line ~pos:r with
                        | exception Not_found -> pattern_match tail
                        | x -> begin
                            match (Group.start x 0) == r with
                            | false -> pattern_match tail
                            | true  -> begin
                                (ctor (Group.get x 0)) :: lx (Group.stop x 0)
                            end
                        end
                    end
                in pattern_match productions
            in lx 0
                
end
