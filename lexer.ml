module Lexer =
    struct
        type tokens = 
            | Ws of string
            | Comment of string
            | Hex of string
            | Num of string
            | Str of string
            | Op of string
            | Directive of string
            | Reg of string
            | Label of string
            | Id of string
    end
            
