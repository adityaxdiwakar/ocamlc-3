let lexed_file = Lexer.lex_file "input.asm" in
let token_imm_parses = match lexed_file with
| []        -> raise Not_found
| hd :: _   -> Parser.token_imm_parse hd in
let str_parsed_tokens = List.map Parser.str_token token_imm_parses in
List.iter (Printf.printf "%s\n") str_parsed_tokens
