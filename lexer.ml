open Re;;

module Lexer =
  struct
    type token = 
      | Ws
      | Comment of string
      | Hex of string
      | Num of string
      | Str of string
      | Op of string
      | Directive of string
      | Reg of string
      | Label of string
    
    type 't production = { 
      regex: string;
      ctor: 't -> token
    }

    let productions = [
      { regex = "[\\s,]+"; ctor = fun _ -> Ws };
      { regex = ";.*"; ctor = fun x -> Comment x };
      { regex = "0?x[0-9a-fA-F]+"; ctor = fun x -> Hex x };
      { regex = "#?-?[0-9]+"; ctor = fun x -> Num x };
      { regex = "ADD|AND|BRn?z?p?|JMP|JSRR|JSR|LDI|LDR|LD|LEA"
        ^ "|NOT|RET|RTI|STI|STR|ST|TRAP"; ctor = fun x -> Op x };
      { regex = "\\.ORIG|\\.END|\\.FILL|\\.BLKW|\\.STRINGZ"; 
        ctor = fun x -> Directive x };
      { regex = "[rR][0-6]"; ctor = fun x -> Reg x };
      { regex = "[a-zA-Z0-9_-]+:"; ctor = fun x -> Label x };
    ]

    let token_print x = 
      match x with
      | Ws            -> (Printf.sprintf "WS()"            )
      | Comment(v)    -> (Printf.sprintf "Comment(%s)"    v)
      | Hex(v)        -> (Printf.sprintf "Hex(%s)"        v)
      | Num(v)        -> (Printf.sprintf "Num(%s)"        v)
      | Op(v)         -> (Printf.sprintf "Opcode(%s)"     v)
      | Directive(v)  -> (Printf.sprintf "Directive(%s)"  v)
      | Reg(v)        -> (Printf.sprintf "Register(%s)"   v)
      | Label(v)      -> (Printf.sprintf "Label(%s)"      v)
      | _             -> (Printf.sprintf "Unrecognized()"  )
    
    let lex_line line: token list = 
      let rec lx r: token list = 
        if r == String.length line then [] else 
        let rec pattern_match p =
          match p with
          | []   -> raise Not_found
          | { regex = reg; ctor = ctor } :: tail -> begin
            match exec (Pcre.regexp reg) line ~pos:r with
            | exception Not_found -> pattern_match tail
            | x -> match (Group.start x 0) == r with
                   | false -> pattern_match tail
                   | true  -> ctor (Group.get x 0) :: lx (Group.stop x 0) end
        in pattern_match productions
      in lx 0

    let get_file_lines file = 
      let ic = open_in file in
      let read_line () =
        try Some (input_line ic) with End_of_file -> None in
      let rec loop acc = 
        match read_line () with
        | Some x -> loop (x :: acc)
        | None -> close_in ic; List.rev acc
      in loop []

    let lex_lines lines: token list list = List.map lex_line lines

    let lex_file filename: token list list = lex_lines (get_file_lines filename)

    let sprint_list lst = 
      let rec splist l = 
        match l with
        | [] -> ""
        | h :: t -> Printf.sprintf "; %s%s" (token_print h) (splist t)
      in Printf.sprintf "[%s]" (splist lst);;
              
end
