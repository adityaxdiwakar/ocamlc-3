type token =         
    Ws
  | Comma
  | Comment of string
  | Hex of string
  | Num of string
  | Str of string
  | Op of string
  | Directive of string
  | Reg of string
  | Label of string
val pp_token :
  Ppx_deriving_runtime.Format.formatter -> token -> Ppx_deriving_runtime.unit
val show_token : token -> Ppx_deriving_runtime.string
val lex_line : string -> token list
val lex_lines : string list -> token list list
val lex_file : string -> token list list
