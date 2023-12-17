open Format

let name = "AArch64"

type register = Register of int

let pp_register fmt (Register n) = fprintf fmt "X%d" n

let id_to_register id = Register id

type instruction = STR of register * register
                 | LDR of register * register
                 (* ... *)

let to_concrete_instruction = let open Instruction in function
  | Store (value, location) -> STR (value, location)
  | Load (value, location) -> LDR (value, location)

let print_asm fmt = function
  | STR (value, location) -> fprintf fmt "STR %a,[%a]" pp_register value pp_register location
  | LDR (value, location) -> fprintf fmt "LDR %a,[%a]" pp_register value pp_register location