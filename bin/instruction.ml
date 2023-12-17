type register = int

type t = Load of register * register
       | Store of register * register
       (* ... *)