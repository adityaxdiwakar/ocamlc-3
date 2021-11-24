type opcode =
  | Add
  | And
  | Br of int
  | Jmp
  | Jsrr
  | Jsr
  | Ldi
  | Ldr
  | Ld
  | Lea
  | Not
  | Ret
  | Rti
  | Sti
  | Str
  | St
  | Trap

type directive = 
  | Orig
  | End
  | Fill
  | Blkw
  | Stringz

type parsed_tokens =
  | Op          of opcode
  | Register    of int
  | Num         of int
  | Directive   of directive
  | Label       of string

let directive_str_to_type = function
  | "ORIG"    -> Orig
  | "END"     -> End
  | "FILL"    -> Fill
  | "BLKW"    -> Blkw
  | "STRINGZ" -> Stringz

let opcode_str_to_type = function
  | "ADD"   -> Add
  | "AND"   -> And

  (* match branch statements *)
  | "BRp"   -> Br 1
  | "BRz"   -> Br 2
  | "BRzp"  -> Br 3
  | "BRn"   -> Br 4
  | "BRnp"  -> Br 5
  | "BRnz"  -> Br 6
  | "BRnzp" (* synonymous with "BR" *)
  | "BR"    -> Br 7
  (* finish matching branch statements *)

  | "JMP"   -> Jmp
  | "JSRR"  -> Jsrr
  | "JSR"   -> Jsr
  | "LDI"   -> Ldi
  | "LDR"   -> Ldr
  | "LD"    -> Ld
  | "LEA"   -> Lea
  | "NOT"   -> Not
  | "RET"   -> Ret
  | "RTI"   -> Rti
  | "STI"   -> Sti
  | "STR"   -> Str
  | "ST"    -> St

  (* TODO: extend to more traps *)
  | "TRAP"  -> Trap

let token_imm_parse tokens = 
  let parse_indv_token = function
    | Lexer.Ws 
    | Lexer.Comment       -> None
    | Lexer.Hex(v)        (* interpretable as number *)
    | Lexer.Num(v)        -> Number begin
      String.get v 0
      |> fun x -> if x == 'x' then "0" ^ v else v end
    | Lexer.Op(v)         -> opcode_str_to_type v
    | Lexer.Directive(v)  -> directive_str_to_type v
    | Lexer.Reg(v)        -> Register begin
      v |> fun x -> String.get x 1 
        |> int_of_char - int_of_char '0' end
    | Lexer.Label(v)      -> Label begin
      v |> String.length 
        |> fun x -> String.sub v 0 (x - 1) end
  in List.map parse_indv_token tokens
