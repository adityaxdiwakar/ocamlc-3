type parsed_list = Parser.token list [@@deriving show]
type lexed_list = Lexer.token list [@@deriving show]
type int_list = int list [@@deriving show]

let output func = List.map (fun x -> print_endline (func x))
let full_lex filename = filename |> Lexer.lex_file
let full_parse filename = full_lex filename |> Parser.parse
let full_assemble filename = full_parse filename |> Assembler.assemble;;

output show_parsed_list (full_parse "tests/input.asm");;
print_endline (show_int_list (full_assemble "tests/input.asm"))
