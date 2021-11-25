(* opcode type declaration and deriving *)
type opcode =        
    Add
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
val pp_opcode :
  Ppx_deriving_runtime.Format.formatter ->
  opcode -> Ppx_deriving_runtime.unit
val show_opcode : opcode -> Ppx_deriving_runtime.string

(* directive type declaration and deriving *)
type directive = Orig | End | Fill | Blkw | Stringz
val pp_directive :
  Ppx_deriving_runtime.Format.formatter ->
  directive -> Ppx_deriving_runtime.unit
val show_directive : directive -> Ppx_deriving_runtime.string

(* parsed token type declaration and deriving *)
type parsed_token =
    Op of opcode
  | Register of int
  | Num of int
  | Directive of directive
  | Label of string
val pp_parsed_token :
  Ppx_deriving_runtime.Format.formatter ->
  parsed_token -> Ppx_deriving_runtime.unit
val show_parsed_token : parsed_token -> Ppx_deriving_runtime.string

val token_imm_parse : Lexer.token list -> parsed_token list
