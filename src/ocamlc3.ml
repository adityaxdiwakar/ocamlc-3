type parsed_list  = Parser.token list [@@deriving show];;
type lexed_list   = Lexer.token list  [@@deriving show];;
type int_list     = int list          [@@deriving show];;

print_endline begin
  try
    show_int_list begin
      "tests/input.asm"
      |> Lexer.lex_file                   (* lex each line *)
      |> List.map Parser.token_imm_parse  (* parse for each line *)
      |> List.filter (fun x -> x != [])   (* get rid of empty lines *)
      |> Parser.full_parse_lines          (* parse every line *)
      |> Assembler.get_bits
    end
  with Failure v -> (Printf.sprintf "Error: %s" v)
end
