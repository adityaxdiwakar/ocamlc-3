let lex = Lexer.lex_file "input.asm" 
  |> List.hd (* unsafe *)
  |> Parser.token_imm_parse in
Printf.printf "%s\n" (Parser.show_parser_list (lex));;
