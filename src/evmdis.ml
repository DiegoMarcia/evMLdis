let stoparith = ["STOP"; "ADD"; "MUL"; "SUB"; "DIV" ; "SDIV" ; "MOD" ; "SMOD" ; "ADDMOD" ; "MULMOD" ; "EXP" ; "SIGNEXTEND"]
let logicbyte = ["LT" ; "GT" ; "SLT" ; "SGT" ; "EQ" ; "ISZERO" ; "AND" ; "OR" ; "XOR" ; "NOT" ; "BYTE"]
let envirinfo = ["ADDRESS" ; "BALANCE" ; "ORIGIN" ; "CALLER" ; "CALLVALUE" ; "CALLDATALOAD" ; "CALLDATASIZE" ; "CALLDATACOPY" ; "CODESIZE" ; "CODECOPY" ; "GASPRICE" ; "EXTCODESIZE" ; "EXTCODECOPY"]
let blockinfo = ["BLOCKHASH" ; "COINBASE" ; "TIMESTAMP" ; "NUMBER" ; "DIFFICULTY" ; "GASLIMIT"]
let storeflow = ["POP" ; "MLOAD" ; "MSTORE" ; "MSTORE8" ; "SLOAD" ; "SSTORE" ; "JUMP" ; "JUMPI" ; "PC" ; "MSIZE" ; "GAS" ; "JUMPDEST"]
let crcallret = ["CREATE" ; "CALL" ; "CALLCODE" ; "RETURN" ; "DELEGATECALL"]

let get_next_instr text pos =
   match int_of_string ("0x" ^ String.sub text pos 2) with
     n when n >= 0 && n <= 11    -> List.nth stoparith n
   | n when n >= 16 && n <= 26   -> List.nth logicbyte (n - 16)
   | 32                          -> "SHA3"
   | n when n >= 48 && n <= 60   -> List.nth envirinfo (n - 48)
   | n when n >= 64 && n <= 69   -> List.nth blockinfo (n - 64)
   | n when n >= 80 && n <= 91   -> List.nth storeflow (n - 80)
   | n when n >= 96 && n <= 127  -> "PUSH" ^ string_of_int (n - 95)
   | n when n >= 128 && n <= 143 -> "DUP"  ^ string_of_int (n - 127)
   | n when n >= 144 && n <= 159 -> "SWAP" ^ string_of_int (n - 143)
   | n when n >= 160 && n <= 164 -> "LOG"  ^ string_of_int (n - 160)
   | n when n >= 240 && n <= 244 -> List.nth crcallret (n - 240)
   | 254                         -> "INVALID"
   | 255                         -> "SUICIDE"
   | _                           -> "UNKNOWN OPCODE " ^ "0x" ^ (String.sub text pos 2)

let get_op_size text pos =
   match int_of_string ("0x" ^ String.sub text pos 2) with
     n when n >= 96 && n <= 127 -> (n - 95) * 2
   | _                          -> 0

let safe_substr s start len =
   let maxlen = String.length s - start in
   let len = min len maxlen in
   String.sub s start len

let get_op text pos op_size =
   if op_size > 0 then
      " 0x" ^ safe_substr text (pos + 2) op_size
   else
      ""

let rec print_list text pos =
   if pos < (String.length text - 1) then
      let instr = get_next_instr text pos in
      let op_size = get_op_size text pos in
      let line = Printf.sprintf "%#x\t" (pos/2) ^ instr ^ (get_op text pos op_size) in
      print_endline line;
      print_list text (pos + 2 + op_size)

let check_format str =
   Str.string_match (Str.regexp_case_fold "^\\(0x\\)?\\(\\([a-f]\\|[0-9]\\)*\\)$") str 0 && String.length str mod 2 = 0

let get_code str =
   if check_format str then
      if String.sub str 0 2 = "0x" then
         String.sub str 2 (String.length str - 2)
      else
         str
   else
      failwith ("evmdis: Only 8-bit values in hexadecimal format admitted")

let () =
    match Array.length Sys.argv with
      2 -> (try (print_list (get_code (String.trim Sys.argv.(1))) 0) with Failure m -> print_endline m)
    | _ -> print_endline "evmdis: Invalid number of arguments.\n\tUsage: evmdis <bytecode>"
