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
  [@@deriving show]

type directive = 
  | Orig
  | End
  | Fill
  | Blkw
  | Stringz
  [@@deriving show]

type parsed_token =
  | Comma
  | Op          of opcode
  | Register    of int
  | Num         of int
  | Directive   of directive
  | Label       of string
  [@@deriving show]

let directive_str_to_type = function
  | "ORIG"    -> Orig
  | "END"     -> End
  | "FILL"    -> Fill
  | "BLKW"    -> Blkw
  | "STRINGZ" -> Stringz
  | _         -> raise Not_found

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

  | _       -> raise Not_found

let token_imm_parse tokens = 
  let parse_indv_token = function

    (* ignore whitespace *)
    | Lexer.Ws 
    | Lexer.Comment(_)    -> None

    (* preserve commas *)
    | Lexer.Comma         -> Some Comma

    (* TODO: don't ignore strings *)
    | Lexer.Str(_)        -> None

    (* parse numbers *)
    | Lexer.Hex(v)        (* interpretable as number *)
    | Lexer.Num(v)        -> Some begin 
      Num begin String.get v 0 
        |> begin function
           | 'x' -> "0" ^ v 
           | '#' -> v |> String.length 
                      |> fun x -> String.sub v 1 (x - 1)
           | _   -> raise Not_found end
        |> int_of_string 
      end end

    (* parse opcodes, directives *)
    | Lexer.Op(v)         -> Some (Op (opcode_str_to_type v))
    | Lexer.Directive(v)  -> Some (Directive (directive_str_to_type v))

    (* parse register value, not checking for \in [0,7] *)
    | Lexer.Reg(v)        -> Some begin
      Register begin String.get v 1
        |> fun x -> int_of_char x - int_of_char '0' 
      end end

    (* parse label name, removing ':' *)
    | Lexer.Label(v)      -> Some begin 
      Label begin v 
        |> String.length 
        |> fun x -> String.sub v 0 (x - 1) 
      end end

  in List.filter_map parse_indv_token tokens

let rec match_register_grp n tokens = 
  (* TODO: make exceptions more verbose *)
  match tokens, n with
  (* only one register left *) 
  | (Register(i) :: _, 1)           -> [Register i]

  (* many registers left, found a comma after *)
  | (Register(i) :: Comma :: tl, _) -> 
      Register i :: match_register_grp (n - 1) tl

  (* any other formula is invalid *)
  | (_, _)                          -> raise Not_found 

let match_register_imm_grp tokens =
  (* would look like Rx, Ry, Num(z) *)
  let reg_grp = match_register_grp 2 tokens in
  match tokens with
  | _ :: _ :: _ ::  Comma :: Num(v) :: _  
      -> reg_grp @ [Num v] (* `@` joins have bad performance, but n = 3 *)
  | _ -> raise Not_found