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
  let abs_bound = (1 lsl bits) in
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
        :: _  -> begin
          let bits = msb + (a lsl 9) + (b lsl 6) + c in
          Printf.sprintf "%X" bits
      end

      (*   15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  *)
      (* +---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+---+ *)
      (* | 0   0   0   1 |     DR    |    SR1    | 1 |       imm5        | *)
      |    Parser.Register a
        :: Parser.Register b
        :: Parser.Num      v
        :: _ -> begin
          if (bounds_check 5 v) then begin
            let bits = msb + (a lsl 9) + (b lsl 6) + (1 lsl 5) + v in
            Printf.sprintf "%d" bits
        end
        else failwith "Immediate value outside range"
      end

      | _ -> assert false
    end

    (* unreachable *)
    | _ -> assert false
  end
  (* unreachable *)
  | _                 -> assert false

let rec get_bits ?(pc = -1) lines =
  let nxt_adr = pc + 16 in
  match lines with
  | [] -> []
  | line :: rem -> begin
    match line with
    | Parser.Op _ :: _ -> (get_bit line):: (get_bits rem ~pc:nxt_adr)
    | Parser.Label label :: tl -> begin
      (* TODO: put label into hashmap of all labels *)
      (* Hashtbl.put ... *)
      print_endline label;
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
    | _ -> failwith "Unreachable, if you see this, uh-oh D:"
  end
