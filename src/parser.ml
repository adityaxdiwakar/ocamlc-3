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

type directive = Orig | End | Fill | Blkw | Stringz [@@deriving show]

type token =
  | Comma
  | Op of opcode
  | Register of int
  | Num of int
  | Directive of directive
  | Label of string
[@@deriving show]

type token_list = token list [@@deriving show]

let directive_str_to_type = function
  | ".ORIG" -> Orig
  | ".END" -> End
  | ".FILL" -> Fill
  | ".BLKW" -> Blkw
  | ".STRINGZ" -> Stringz
  | _ as s -> failwith (Printf.sprintf "Invalid directive %s" s)

let opcode_str_to_type = function
  | "ADD" -> Add
  | "AND" -> And
  (* match branch statements *)
  | "BRp" -> Br 1
  | "BRz" -> Br 2
  | "BRzp" -> Br 3
  | "BRn" -> Br 4
  | "BRnp" -> Br 5
  | "BRnz" -> Br 6
  | "BRnzp" (* synonymous with "BR" *) | "BR" -> Br 7
  | "JMP" -> Jmp
  | "JSRR" -> Jsrr
  | "JSR" -> Jsr
  | "LDI" -> Ldi
  | "LDR" -> Ldr
  | "LD" -> Ld
  | "LEA" -> Lea
  | "NOT" -> Not
  | "RET" -> Ret
  | "RTI" -> Rti
  | "STI" -> Sti
  | "STR" -> Str
  | "ST" -> St
  (* TODO: extend to more traps *)
  | "TRAP" -> Trap
  | _ as s -> failwith (Printf.sprintf "Invalid opcode %s" s)

let token_imm_parse tokens =
  let parse_indv_token = function
    (* ignore whitespace *)
    | Lexer.Ws | Lexer.Comment _ -> None
    (* preserve commas *)
    | Lexer.Comma -> Some Comma
    (* TODO: don't ignore strings *)
    | Lexer.Str _ -> None
    (* parse numbers *)
    | Lexer.Hex v (* interpretable as number *) | Lexer.Num v ->
        Some
          (Num
             (String.get v 0
             |> (function
                  | 'x' -> "0" ^ v
                  | '#' -> v |> String.length |> fun x -> String.sub v 1 (x - 1)
                  | _ -> v)
             |> int_of_string))
    (* parse opcodes, directives *)
    | Lexer.Op v -> Some (Op (opcode_str_to_type v))
    | Lexer.Directive v -> Some (Directive (directive_str_to_type v))
    (* parse register value, not checking for \in [0,7] *)
    | Lexer.Reg v ->
        Some (Register (int_of_char (String.get v 1) - int_of_char '0'))
    (* parse label name, removing ':' *)
    | Lexer.Label v ->
        Some (Label (v |> String.length |> fun x -> String.sub v 0 (x - 1)))
  in

  List.filter_map parse_indv_token tokens

let file_imm_parse lst =
  List.filter (fun x -> x != []) (List.map token_imm_parse lst)

let fail_w_token token fmt lst =
  failwith
    (Printf.sprintf "%s should be followed by:\n\t%s, but found:\n%s\n"
       (* token *)
       (show_token token)
       (* proper format for this instruction *)
       fmt
       (* input/wrong list of tokens *)
       (show_token_list lst))

let rec full_parse_line tokens =
  match tokens with
  (* ADD or AND following the form of 
   *  (Op And) (Register) (Register) (Register)
   *  (Op And) (Register) (Register) (Num)
   *  (Op Add) (Register) (Register) (Register)
   *  (Op Add) (Register) (Register) (Num)
   * *)
  | (Op And as v) :: tl | (Op Add as v) :: tl -> (
      let fmt =
        "[Parser.Register; Parser.Comma; Parser.Register; "
        ^ "Parser.Comma;\n\tParser.Register OR Parser.Num]"
      in
      match tl with
      | [ (Register _ as w); Comma; (Register _ as x); Comma; third ] -> (
          match third with
          | Register _ -> [ v; w; x; third ]
          | Num _ -> [ v; w; x; third ]
          | _ -> fail_w_token v fmt tl)
      | _ -> fail_w_token v fmt tl)
  (* NOT following the form of
   *  (Op Not) (Register) (Register)
   * *)
  | (Op Not as v) :: tl -> (
      match tl with
      | [ (Register _ as w); Comma; (Register _ as x) ] -> [ v; w; x ]
      | _ ->
          let fmt = "[Parser.Register; Parser.Comma; Parser.Register]" in
          fail_w_token v fmt tl)
  (* BR, JMP, JSR, or JSRR following the form of
   *  (Op Br (n)) (Num)
   *  (Op Jmp) (Register)
   *  (Op Jsr) (Num)
   *  (Op Jsrr) (Register)
   * *)
  | Op (Br _ as v) :: tl
  | Op (Jmp as v) :: tl
  | Op (Jsr as v) :: tl
  | Op (Jsrr as v) :: tl
  | Op (Trap as v) :: tl -> (
      let ov = Op v in
      match tl with
      | (Num _ as w) :: _ -> (
          match v with
          | Br _ | Jsr | Trap -> [ ov; w ]
          | _ -> fail_w_token w "[Parser.Num]" tl)
      | (Register _ as w) :: _ -> (
          match v with
          | Jmp | Jsrr -> [ ov; w ]
          | _ -> fail_w_token w "[Parser.Register]" tl)
      | _ -> (
          match v with
          | Br _ | Jsr | Trap -> fail_w_token ov "[Parser.Num]" tl
          | Jmp | Jsrr -> fail_w_token ov "[Parser.Register]" tl
          | _ -> raise (Failure "Unreachable")))
  (* LD, LDI, LEA, ST, STI following the form of
   *  (Op Ld) (Register) (Num)
   *  (Op Ldi) (Register) (Num)
   *  (Op Lea) (Register) (Num)
   *  (Op St) (Register) (Num)
   *  (Op Sti) (Register) (Num)
   * *)
  | (Op Ld as v) :: tl
  | (Op Ldi as v) :: tl
  | (Op Lea as v) :: tl
  | (Op St as v) :: tl
  | (Op Sti as v) :: tl -> (
      match tl with
      | [ (Register _ as w); Comma; (Num _ as x) ] -> [ v; w; x ]
      | _ as tl ->
          let fmt = "[Parser.Register; Parser.Comma; Parser.Num]" in
          fail_w_token v fmt tl)
  (* STR, LDR following the form of
   *  (Op Str) (Register) (Register) (Num)
   *  (Op Ldr) (Register) (Register) (Num)
   * *)
  | (Op Str as v) :: tl | (Op Ldr as v) :: tl -> (
      match tl with
      | (Register _ as w)
        :: Comma :: (Register _ as x) :: Comma :: (Num _ as y) :: _ ->
          [ v; w; x; y ]
      | _ as tl ->
          let fmt =
            "[Parser.Register; Parser.Comma; Parser.Register;"
            ^ "\n\t\t Parser.Comma; Parser.Num]"
          in
          fail_w_token v fmt tl)
  (* RET, RTI following the form of
   *  (Op Ret) 
   *  (Op Rti)
   * *)
  | (Op Ret as v) :: _ | (Op Rti as v) :: _ -> [ v ]
  | (Directive _ as v) :: tl -> (
      match v with
      | Directive Orig -> (
          match tl with
          | [ (Num _ as w) ] -> [ v; w ]
          | _ -> fail_w_token v "[Parser.Num]" tl)
      | Directive End -> (
          match tl with
          | [] -> [ v ]
          | _ ->
              failwith
                (Printf.sprintf
                   "%s should NOT be followed by a token, but found %s"
                   (* directive *)
                   (show_token v)
                   (* input/wrong list of tokens *)
                   (show_token_list tl)))
      | _ ->
          failwith
            (Printf.sprintf "Unrecognized directive %s, found:\n\t%s"
               (show_token v)
               (show_token_list (v :: tl))))
  | (Label _ as v) :: tl -> if tl == [] then [ v ] else v :: full_parse_line tl
  | Comma :: _ | Register _ :: _ | Num _ :: _ | [] ->
      failwith "Need tokens to parse, cannot parse []"

let parse tokens_list = List.map full_parse_line (file_imm_parse tokens_list)
