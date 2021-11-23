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
  | "ADD" -> Add
  | "AND" -> And

let token_imm_parse tokens = 
  let parse_indv_token = function
    | Ws | Comment    -> None
    | Hex(v) | Num(v) -> begin
      v |> fun x -> if String.get x 0 == 'x' then "0" ^ x else x end 
    | Op(v)         -> begin
      match v with 
    end
    | Directive(v)  -> (Printf.sprintf "Directive(%s)"  v)
    | Reg(v)        -> (Printf.sprintf "Register(%s)"   v)
    | Label(v)      -> (Printf.sprintf "Label(%s)"      v)
    | _             -> (Printf.sprintf "Unrecognized()"  )

