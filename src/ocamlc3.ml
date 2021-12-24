type parsed_list  = Parser.token list [@@deriving show];;
type lexed_list   = Lexer.token list  [@@deriving show];;

print_endline begin
  try
    String.concat "\n" begin
      "tests/input.asm"
      |> Lexer.lex_file                   (* lex each line *)
      |> List.map show_lexed_list         (* show lexed lines *)
    end
  with Failure v -> (Printf.sprintf "Error: %s" v)
end;;

print_endline "\n";;

print_endline begin
  try
    String.concat "\n" begin
      "tests/input.asm"
      |> Lexer.lex_file                   (* lex each line *)
      |> List.map Parser.token_imm_parse  (* parse for each line *)
      |> List.filter (fun x -> x != [])   (* get rid of empty lines *)
      |> Parser.full_parse_lines          (* parse every line *)
      |> List.map show_parsed_list        (* show parsed lists *)
    end
  with Failure v -> (Printf.sprintf "Error: %s" v)
end;;

print_endline "\n";;
type int_list = string list [@@deriving show];;

print_endline begin
  try
    show_int_list begin
      "tests/assembler.asm"
      |> Lexer.lex_file                   (* lex each line *)
      |> List.map Parser.token_imm_parse  (* parse for each line *)
      |> List.filter (fun x -> x != [])   (* get rid of empty lines *)
      |> Parser.full_parse_lines          (* parse every line *)
      |> Assembler.get_bits
    end
  with Failure v -> (Printf.sprintf "Error: %s" v)
end
