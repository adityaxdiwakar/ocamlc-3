type parsed_list  = Parser.token list [@@deriving show];;
type lexed_list   = Lexer.token list  [@@deriving show];;

(*
List.map (Printf.printf "%s\n") begin
  "input.asm"
  |> Lexer.lex_file                   (* lex each line *)
  |> List.map show_lexed_list         (* show lexed lines *)
end;;
*)

print_endline begin
  try
    String.concat "\n" begin
      "input.asm"
      |> Lexer.lex_file                   (* lex each line *)
      |> List.map Parser.token_imm_parse  (* parse for each line *)
      |> List.filter (fun x -> x != [])   (* get rid of empty lines *)
      |> Parser.full_parse_lines          (* parse every line *)
      |> List.map show_parsed_list        (* show parsed lists *)
    end
  with Failure v -> (Printf.sprintf "Error: %s" v)
end
