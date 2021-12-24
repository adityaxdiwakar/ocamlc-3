(*   Op Code -> Binary
ADD  | 0   0   0   1 |
AND  | 0   1   0   1 |
BR   | 0   0   0   0 |
JMP  | 1   1   0   0 |
JSR  | 0   1   0   0 |
JSRR | 0   1   0   0 |
LD   | 0   0   1   0 |
LDI  | 1   0   1   0 |
LDR  | 0   1   1   0 |
LEA  | 1   1   1   0 |
NOT  | 1   0   0   1 |
RET  | 1   1   0   0 |
RTI  | 1   0   0   0 |
ST   | 0   0   1   1 |
STI  | 1   0   1   1 |
STR  | 0   1   1   1 |
TRAP | 1   1   1   1 |
N/A  | 1   1   0   1 |  *) 

let bit_opcode = function
  | Parser.Add  -> 0b0001
  | Parser.And  -> 0b0101
  | Parser.Br _ -> 0b0000
  | Parser.Jmp  -> 0b1100
  | Parser.Jsr  
  | Parser.Jsrr -> 0b0100
  | Parser.Ld   -> 0b0010
  | Parser.Ldi  -> 0b1010
  | Parser.Ldr  -> 0b0110
  | Parser.Lea  -> 0b1110
  | Parser.Not  -> 0b1001
  | Parser.Ret  -> 0b1100
  | Parser.Rti  -> 0b1000
  | Parser.St   -> 0b0011
  | Parser.Sti  -> 0b1011
  | Parser.Str  -> 0b0111
  | Parser.Trap -> 0b1111

let bit_register reg = 1 lsl reg
