(*
  { regex = "ADD|AND|BRn?z?p?|JMP|JSRR|JSR|LDI|LDR|LD|LEA"
    ^ "|NOT|RET|RTI|STI|STR|ST|TRAP"; ctor = fun x -> Op x };
*)

type opcode =
  | ADD
  | AND
  | BR of int
  | JMP
  | JSRR
  | JSR
  | LDI
  | LDR
  | LD
  | LEA
  | NOT
  | RET
  | RTI
  | STI
  | STR
  | ST
  | TRAP

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
