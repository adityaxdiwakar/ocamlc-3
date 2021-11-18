module Lexer :       
    sig
        type token =
            Ws
            | Comment of string
            | Hex of string
            | Num of string
            | Str of string
            | Op of string
            | Directive of string
            | Reg of string
            | Label of string
        type 't production = { regex : string; ctor : 't -> token; }
        val productions : string production list
        val token_print : token -> string
        val lex_line : string -> token list
        val sprint_list : token list -> string
    end
