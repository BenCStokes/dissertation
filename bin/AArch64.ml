open Format

let name = "AArch64"

type register = Register of int

let pp_register fmt (Register n) = fprintf fmt "X%d" n

let id_to_register id = Register id

type instruction = STR of register * register
                 | LDR of register * register
                 | DMB of Instruction.arm_barrier_param
                 | DSB of Instruction.arm_barrier_param
                 | ISB
                 | CBNZ of register * int
                 | ADD of register * register * register
                 | EOR of register * register * register
                 (* ... *)

let to_concrete_instructions = let open Instruction in function
  | Store (value, location) -> [STR (Register value, Register location)]
  | Load (value, location) -> [LDR (Register value, Register location)]
  | ArmDMBEquiv param -> [DMB param]
  | ArmDSBEquiv param -> [DSB param]
  | ArmISBEquiv -> [ISB]
  | ControlDependency reg -> [CBNZ (id_to_register reg, 4)]
  | DataDependency (reg1, reg2)
  | AddressDependency (reg1, reg2) -> [EOR (Register 14, id_to_register reg1, id_to_register reg1); ADD (id_to_register reg2, id_to_register reg2, Register 14)]

let pp_barrier_param fmt = let open Instruction in function
  | SY -> fprintf fmt "SY"
  | LD -> fprintf fmt "LD"

let print_asm fmt = function
  | STR (value, location) -> fprintf fmt "STR %a,[%a]" pp_register value pp_register location
  | LDR (value, location) -> fprintf fmt "LDR %a,[%a]" pp_register value pp_register location
  | DMB param -> fprintf fmt "DMB %a" pp_barrier_param param
  | DSB param -> fprintf fmt "DSB %a" pp_barrier_param param
  | ISB -> fprintf fmt "ISB"
  | CBNZ (reg, offset) -> fprintf fmt "CBNZ %a,.+%d" pp_register reg offset
  | ADD (dst, src1, src2) -> fprintf fmt "ADD %a,%a,%a" pp_register dst pp_register src1 pp_register src2
  | EOR (dst, lhs, rhs) -> fprintf fmt "EOR %a,%a,%a" pp_register dst pp_register lhs pp_register rhs