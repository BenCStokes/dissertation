open Format

type initial_value = Constant of int64
                   | VirtualAddress of string

let pp_initial_value fmt = function
  | Constant n -> fprintf fmt "extz(%#Lx, 64)" n
  | VirtualAddress address -> fprintf fmt "%s" address