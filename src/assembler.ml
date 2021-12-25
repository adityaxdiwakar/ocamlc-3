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

let bounds_check bits num = 
  let abs_bound = (1 lsl (bits - 1)) in
  if num >= -abs_bound && num <= (abs_bound - 1) then true
  else false

let get_bit line = 
  match line with
  | Parser.Op v :: tl  -> begin
    let msb = (bit_opcode v) lsl 12 in
    match v with
    
    (* handle either ADD or AND *)
    | Parser.Add
    | Parser.And -> begin
      match tl with

      (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
      (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
      (* | 0   0   0   1 |     DR    |    SR1    | 0 | 0   0 |    SR2    | *) 
      |    Parser.Register a 
        :: Parser.Register b 
        :: Parser.Register c 
        :: _  -> msb + (a lsl 9) + (b lsl 6) + c

      (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
      (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
      (* | 0   0   0   1 |     DR    |    SR1    | 1 |       imm5        | *)
      |    Parser.Register a
        :: Parser.Register b
        :: Parser.Num      v
        :: _ -> begin
        if (bounds_check 5 v) 
        then msb + (a lsl 9) + (b lsl 6) + (1 lsl 5) + v
        else failwith "Immediate value outside range"
      end

      | _ -> assert false
    end

    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
    (* | 0   0   0   0 | n | z | p |             PCoffset9             | *)
    | Parser.Br f -> begin
      match tl with
      | Parser.Num v :: _ -> begin
        if (bounds_check 9 v) 
        then msb + (f lsl 9)
        else failwith "PCoffset9 outside range"
      end

      | _ -> assert false
    end 

    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0       *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+      *)
    (* | 1   1   0   0 | 0   0   0 |   BaseR   | 0   0   0   0   0   0 | JMP  *)
    (* | 0   1   0   0 | 0 | 0   0 |   BaseR   | 0   0   0   0   0   0 | JSRR *)
    | Parser.Jmp 
    | Parser.Jsrr -> begin
      match tl with
      | Parser.Register a :: _ -> msb + (a lsl 6)
      | _ -> assert false
    end

    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
    (* | 0   1   0   0 | 1 |               PCoffset11                  | *)
    | Parser.Jsr -> begin
      match tl with
      | Parser.Num v :: _ -> begin
        if (bounds_check 11 v)
        then msb + (1 lsl 11) + v
        else failwith "PCoffset11 outside range"
      end
      | _ -> assert false
    end

    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0       *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+      *)
    (* | 0   0   1   0 |     DR    |             PCoffset9             | LD   *)
    (* | 1   0   1   0 |     DR    |             PCoffset9             | LDI  *)
    (* | 1   1   1   0 |     DR    |             PCoffset9             | LEA  *)
    (* | 0   0   1   1 |     SR    |             PCoffset9             | ST   *)
    (* | 1   0   1   1 |     SR    |             PCoffset9             | STI  *)
    | Parser.Ld
    | Parser.Ldi
    | Parser.Lea
    | Parser.St
    | Parser.Sti -> begin
        match tl with
        | Parser.Register a :: Parser.Num v :: _ -> begin
          if (bounds_check 9 v)
          then msb + (a lsl 9) + v
          else failwith "PCOffset9 outside range"
        end
        | _ -> assert false
      end
    
    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0       *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+      *)
    (* | 0   1   1   0 |     DR    |   BaseR   |        offset6        | LDR  *)
    (* | 0   1   1   1 |     SR    |   BaseR   |        offset6        | STR  *)
    | Parser.Ldr
    | Parser.Str -> begin
      match tl with
      | Parser.Register a :: Parser.Register b :: Parser.Num v :: _ -> begin
        if (bounds_check 6 v)
        then msb + (a lsl 9) + (b lsl 6) + v
        else failwith "offset6 outside range"
      end
      | _ -> assert false
    end

    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
    (* | 1   0   0   1 |     DR    |     SR    | 1   1   1   1   1   1 | *) 
    | Parser.Not -> begin
      match tl with
      | Parser.Register a :: Parser.Register b :: _ -> begin
        msb + (a lsl 9) + (b lsl 6) + 63
      end
      | _ -> assert false
    end

    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
    (* | 1   0   0   0 | 0   0   0   0   0   0   0   0   0   0   0   0 | *)
    | Parser.Rti -> msb + (7 lsl 6)

    (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
    (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
    (* | 1   1   0   0 | 0   0   0 | 1   1   1 | 0   0   0   0   0   0 | *)
    | Parser.Ret -> msb

    | Parser.Trap -> begin
      match tl with
      | Parser.Num v :: _ -> begin
        if (bounds_check 8 v)
        then msb + v 
        else failwith "trapvect8 outside range"
      end
      | _ -> assert false
    end
  end

  (* unreachable *)
  | _ -> assert false

let rec get_bits ?(pc = -1) lines =
  let nxt_adr = pc + 16 in
  match lines with
  | [] -> []
  | line :: rem -> begin
    match line with
    | Parser.Op _ :: _ -> (get_bit line):: (get_bits rem ~pc:nxt_adr)
    | Parser.Label _ :: tl -> begin
      (* TODO: put label into hashmap of all labels *)
      (* Hashtbl.put ... *)
      if tl = [] then get_bits rem ~pc:pc
      else (get_bit tl) :: (get_bits rem ~pc:nxt_adr)
    end
    | Parser.Directive Parser.Orig :: Parser.Num v :: _ -> begin
      if pc = -1 then get_bits rem ~pc:v (* initialize program counter *)
      else failwith "Cannot originate inside block"
    end
    | Parser.Directive Parser.End :: _ -> begin
      if pc = -1 then failwith "Cannot end outside of block"
      else get_bits rem ~pc:(-1) (* uninitialize program counter *)
    end
    | _ -> assert false
  end
