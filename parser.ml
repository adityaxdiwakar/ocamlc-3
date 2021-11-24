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
  | ORIG
  | END
  | FILL
  | BLKW
  | STRINGZ

type parsed_tokens =
  | Op          of opcode
  | Register    of int
  | Num         of int
  | Directive   of directive
  | Label       of string

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
    | Ws | Comment    -> None
    | Hex(v) | Num(v) ->
      v |> fun x -> if String.get x 0 == 'x' then "0" ^ x else x 
    | Op(v)         -> opcode_str_to_type v
    | Directive(v)  -> (Printf.sprintf "Directive(%s)"  v)
    | Reg(v)        -> (Printf.sprintf "Register(%s)"   v)
    | Label(v)      -> (Printf.sprintf "Label(%s)"      v)
    | _             -> (Printf.sprintf "Unrecognized()"  )

