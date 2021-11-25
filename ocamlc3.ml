type parsed_list = Parser.parsed_token list [@@deriving show];;
Printf.printf "%s\n" begin
  "input.asm"
  |> Lexer.lex_file           (* lex file *)
  |> List.hd                  (* get head, unsafe *)
  |> Parser.token_imm_parse   (* parse *)
  |> show_parsed_list 
end
