type opcode =
  | Add   | And   | Br of int
  | Jmp   | Jsrr  | Jsr
  | Ldi   | Ldr   | Ld
  | Lea   | Not   | Ret
  | Rti   | Sti   | Str
  | St    | Trap
  [@@deriving show]

type directive = 
  | Orig  | End   | Fill
  | Blkw  | Stringz
  [@@deriving show]

type token =
  | Comma
  | Op          of opcode
  | Register    of int
  | Num         of int
  | Directive   of directive
  | Label       of string
  [@@deriving show]

let directive_str_to_type = function
  | ".ORIG"     -> Orig
  | ".END"      -> End
  | ".FILL"     -> Fill
  | ".BLKW"     -> Blkw
  | ".STRINGZ"  -> Stringz
  | (_ as s)    -> failwith begin  
      Printf.sprintf "Invalid directive %s" s end

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

let full_parse_line tokens = 
  match tokens with

  (* ADD or AND following the form of 
   *  (Op And) (Register) (Register) (Register)
   *  (Op And) (Register) (Register) (Num)
   *  (Op Add) (Register) (Register) (Register)
   *  (Op Add) (Register) (Register) (Num)
   * *)

  | (Op And as v) :: tl 
  | (Op Add as v) :: tl -> begin
      match tl with
      |    (Register _ as w) :: Comma 
        :: (Register _ as x) :: Comma
        :: third :: [] -> begin 
       match third with
       | (Register _) -> [v; w; x; third]
       | (Num _)      -> [v; w; x; third]
       | _            -> raise Not_found
     end
      | _ -> raise Not_found
   end

  
  (* NOT following the form of
   *  (Op Not) (Register) (Register)
   * *)

  | (Op Not as v) :: tl -> begin
      match tl with
      | (Register _ as w) :: Comma :: (Register _ as x) :: []  -> [v; w; x]
      | _ -> raise Not_found end


  (* BR, JMP, JSR, or JSRR following the form of
   *  (Op Br (n)) (Num)
   *  (Op Jmp) (Register)
   *  (Op Jsr) (Num)
   *  (Op Jsrr) (Register)
   * *)

  | Op (Br _  as v) :: tl 
  | Op (Jmp   as v) :: tl   
  | Op (Jsr   as v) :: tl 
  | Op (Jsrr  as v) :: tl 
  | Op (Trap  as v) :: tl -> begin
      match tl with
      | (Num _) as w :: _ -> begin match v with 
          | Br _ | Jsr | Trap -> [Op v; w]
          | _ -> raise Not_found end

      | (Register _) as w :: _ -> begin match v with
          | Jmp | Jsrr -> [Op v; w]
          | _ -> raise Not_found end

      | _ -> raise Not_found
    end

  | (Op Ld  as v) :: tl
  | (Op Ldi as v) :: tl
  | (Op Lea as v) :: tl
  | (Op St  as v) :: tl
  | (Op Sti as v) :: tl -> begin
      match tl with
      | (Register _ as w) :: Comma :: (Num _ as x) :: _ -> [v; w; x] 
      | _ -> raise Not_found end

  | (Op Str as v) :: tl
  | (Op Ldr as v) :: tl -> begin
      match tl with
      | (Register _ as w) :: Comma :: (Register _ as x) :: Comma 
        :: (Num _ as y) :: _ -> [v; w; x; y] 
      | _ -> raise Not_found end

  | (Op Ret as v) :: _ 
  | (Op Rti as v) :: _ -> [v]

  | Directive v :: tl -> begin
      match v, tl with
      | Orig, (Num _ as w) :: _ -> [Directive v; w]
      | End, _                  -> [Directive v] 
      | _                       -> raise Not_found end

  (* TODO: Label should not raise `Not_found` *)
  | Comma       :: _
  | Register  _ :: _ 
  | Num       _ :: _ 
  | Label     _ :: _ 
  | [] -> raise Not_found

let full_parse_lines tokens_list = List.map full_parse_line tokens_list
