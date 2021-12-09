type opcode =
  | Add   | And   | Br of int
  | Jmp   | Jsrr  | Jsr
  | Ldi   | Ldr   | Ld
  | Lea   | Not   | Ret
  | Rti   | Sti   | Str
  | St    | Trap
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
           | _   -> v end
        |> int_of_string 
      end end

    (* parse opcodes, directives *)
    | Lexer.Op(v)         -> Some (Op (opcode_str_to_type v))
    | Lexer.Directive(v)  -> Some (Directive (directive_str_to_type v))

    (* parse register value, not checking for \in [0,7] *)
    | Lexer.Reg(v)        -> Some begin
        Register (int_of_char (String.get v 1) - int_of_char '0')
      end

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
  | (Register(i) :: _, 1)           -> Some [Register i]

  (* many registers left, found a comma after *)
  | (Register(i) :: Comma :: tl, _) -> 
    match_register_grp (n-1) tl 
    |> begin function 
        | None -> None 
        | Some v -> Some (Register i :: v) end

  (* any other formula is invalid *)
  | (_, _)                          -> None

let match_register_imm_grp n tokens =
  (* would look like Rx, Ry, Num(z) *)
  let reg_grp = match_register_grp n tokens in
  match (reg_grp, tokens) with
  | Some grp,  _ :: _ :: _ ::  Comma :: Num(v) :: _
      -> Some (grp @ [Num v]) (* `@` joins have bad performance, but n = 3 *)
  | _ -> None 

let full_parse_line tokens = 
  match tokens with
  | Op v :: tl when v == And || v == Add -> begin
      let groups = List.filter_map
          (fun x -> x) 
          [match_register_grp 3 tl; match_register_imm_grp 2 tl] in
      match groups with 
      | [] -> []
      | hd :: _ -> (Op v) :: hd end
  | _ :: _          
  | []              -> raise Not_found

let full_parse_lines tokens_list = List.map full_parse_line tokens_list
